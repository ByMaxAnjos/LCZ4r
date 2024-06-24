#' Assess the Urban Heat Island Intensity
#'
#' This function calculates the Urban Heat Island (UHI) intensity based on air temperature measurements and Local Climate Zones (LCZ). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
#'
#' @param x A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()} function.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var The name of the variable in the data frame representing air temperature.
#' @param station_id The name of the variable in the data frame representing station IDs.
#' @param ... Additional arguments for the \code{selectBydata} from \code{openair} package. These include:
#' \itemize{
#'   \item A start date string in the form "1/2/1999" or in format "YYYY-mm-dd", e.g., "1999-02-01".
#'   \item A year or years to select, e.g., year = 1998:2004 to select 1998-2004 inclusive, or year = c(1998, 2004) to select 1998 and 2004.
#'   \item A month or months to select. Can either be numeric, e.g., month = 1:6 to select January to June, or by name, e.g., month = c("January", "December").
#' }
#' @param method Method to calculate the UHI intensity. Options include "LCZ" and "manual".
#' @param Turban Urban station references, if the method "manual" is selected.
#' @param Trural Rural station references, if the method "manual" is selected.
#' @param group If TRUE urban, rural and UHI - related temperatures are grouped in the same plot.
#' @param time.freq The time period to average to. Default is \dQuote{hour}, but can also be \dQuote{day}, \dQuote{week}, \dQuote{month}, or \dQuote{year}.
#' @param by The time-series split option. Options include \dQuote{year}, \dQuote{season}, \dQuote{seasonyear}, \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend}, \dQuote{site}, \dQuote{daylight}, or \dQuote{dst} (daylight saving time). See the \code{type} argument in the \code{openair} package: \url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}.
#' @param impute Method to impute missing values. Options include \dQuote{mean}, \dQuote{median}, \dQuote{knn}, or \dQuote{bag}.
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series, station references) into your directory.
#' @param ylab The label for the y-axis.
#' @param ylab2 The label for the right y-axis.
#' @param xlab The label for the x-axis. Default is \dQuote{Time}.
#' @param title The title of the plot. Default is an empty string.
#' @param caption The source data caption.
#'
#' @return A visual representation of the time series of UHI in \code{ggplot} or data frame .csv format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate hourly UHI intensity for 2019 using the LCZ method
#' my_uhi <- lcz_uhi_intensity(lcz_map, data_frame = lcz_data, var = "airT",
#'                             station_id = "station", year = 2019,
#'                             time.freq = "hour", method = "LCZ")
#'
#'
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_uhi_intensity <- function(x, data_frame = "", var = "", station_id = "", ..., time.freq = "hour",
                              method = "LCZ", Turban = NULL, Trural = NULL, by = NULL, impute = NULL,
                              group = FALSE, iplot = TRUE, isave = FALSE, ylab = "Air temperature [C]",
                              xlab = "Time", ylab2 = "UHI", title = "", caption = "LCZ4r,2024") {

  # Check and validate raster inputs -----------------------------------------------

   if (missing(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map* functions")
  }
  if (!inherits(x, "SpatRaster")) {
    warning("The input 'x' is not a SpatRaster object. Attempting to convert it using terra::rast()")
    x <- try(terra::rast(x), silent = TRUE)
    if (inherits(x, "try-error")) {
      stop("Failed to convert input 'x' to SpatRaster. Please provide a valid SpatRaster object.")
    }
  }
    if (terra::nlyr(x) > 1) {
      x <- x[[2]]
    }

  if (terra::crs(x, proj = TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {
    warning("The input 'x' is not in WGS84 projection. Reprojecting to WGS84.")
    x <- terra::project(x, "+proj=longlat +datum=WGS84 +no_defs")
  }


# Check data inputs -------------------------------------------------------
  if (!is.data.frame(data_frame)) {
    stop("The 'data_frame' input must be a data frame containing air temperature measurements,station IDs, latitude, and longitude.")
  }

  if (missing(var) || !var %in% colnames(data_frame)) {
    stop("The 'var' input must be a column name in 'data_frame' representing air temperature.")
  }

  if (missing(station_id) || !station_id %in% colnames(data_frame)) {
    stop("The 'station_id' input must be a column name in 'data_frame' representing stations.")
    }

  if (!("Latitude" %in% tolower(colnames(data_frame)) || "latitude" %in% tolower(colnames(data_frame)))) {
    stop("The 'latitude' input must be a column name in 'data_frame' representing each station's latitude.")
  }

  if (!("Longitude" %in% tolower(colnames(data_frame)) || "longitude" %in% tolower(colnames(data_frame)))) {
    stop("The 'longitude' input must be a column name in 'data_frame' representing each station's longitude")
  }

  # Pre-processing time series ----------------------------------------------

  #Rename and define my_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(my_id = dplyr::cur_group_id(),
                  date = lubridate::as_datetime(date)) %>%
    openair::selectByDate(...)

  df_processed$var_interp <- base::as.numeric(df_processed$var_interp)

  # Impute missing values if necessary
  if (!is.null(impute)) {
    impute_methods <- c("mean", "median", "knn", "bag")
    if (!(impute %in% impute_methods)) {
      stop("Invalid impute method. Choose from 'mean', 'median', 'knn', or 'bag'.")
    }
    impute_function <- switch(impute,
                              "mean" = recipes::step_impute_mean,
                              "median" = recipes::step_impute_median,
                              "knn" = recipes::step_impute_knn,
                              "bag" = recipes::step_impute_bag
    )
    lcz_recipe <- recipes::recipe(var_interp ~ ., data = df_processed) %>%
      impute_function(.data$var_interp)
    df_processed <- lcz_recipe %>%
      recipes::prep(df_processed) %>%
      recipes::bake(new_data = NULL)
    base::message("The missing values have been imputed with ", impute)
  }

  # Geospatial operations ---------------------------------------------------

  #Convert lcz_map to polygon
  base::names(x) <- "lcz"
  lcz_shp <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  #Get shp LCZ stations from lat and long
  shp_stations <- df_processed %>%
    dplyr::distinct(.data$latitude, .data$longitude, .keep_all = T) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
  #Intersect poi shp stations with lcz shp
  lcz_stations <- sf::st_intersection(shp_stations, lcz_shp) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$my_id, .data$station, .data$lcz)

  #merge data-model with lcz_station to get lcz class
  df_model <-
    dplyr::inner_join(df_processed, lcz_stations, by = c("station", "my_id")) %>%
    dplyr::mutate(
      lcz = base::as.factor(.data$lcz),
      my_id = base::as.factor(.data$my_id))

  my_stations <- df_model %>%
    dplyr::distinct(.data$my_id, .data$lcz, .data$station)

  # check urban and rural stations ------------------------------------------

  if (method != "LCZ" && method != "manual") {
    stop("The 'method' input must be either 'LCZ' or 'manual'.")
  }

  if (method == "manual" && (is.null(Turban) || is.null(Trural))) {
    stop("When using the 'manual' method, both 'Turban' and 'Trural' must be provided.")
  }


# Select urban and rural stations -----------------------------------------

  if (method == "LCZ") {
    select_stations <- function(data) {
      urban_preference <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      rural_preference <- c("11", "12", "13", "14", "15", "16")

      find_station <- function(stations, preferences) {
        for (pref in preferences) {
          station <- stations %>% dplyr::filter(lcz == pref)
          if (nrow(station) > 0) {
            return(station)
          }
        }
        return(NULL)
      }

      urban_station <- find_station(data, urban_preference)
      rural_station <- find_station(data, rural_preference)

      base::list(urban = urban_station, rural = rural_station)
    }

    result <- select_stations(my_stations)
    urban_station <- result$urban
    rural_station <- result$rural

    if (is.null(urban_station) || is.null(rural_station)) {
      stop("Unable to find suitable urban or rural stations based on urban-LCZs (1 - 10) and rural-LCZs (11 - 16). Please, try the manual method")
    }

    lcz_model <- df_model %>%
      dplyr::filter(.data$station %in% c(paste0(urban_station$station), paste0(rural_station$station))) %>%
      dplyr::mutate(reference = ifelse(.data$station == paste0(urban_station$station), "urban", "rural"))

  }

  if (method == "manual") {

    urban_station <- Turban
    rural_station <- Trural

    lcz_model <- df_model %>%
      dplyr::filter(.data$station %in% c(paste0(urban_station), paste0(rural_station))) %>%
      dplyr::mutate(reference = base::ifelse(.data$station == paste0(urban_station), "urban", "rural"))

    if (is.null(lcz_model) || nrow(lcz_model)==0) {
      stop("Unable to find suitable urban or rural station. Please, try the LCZ method")
    }

  }

  # Settings for plots ------------------------------------------------------

  lcz <- tibble::as_tibble(c(seq(1, 10, 1), seq(11, 17))) %>%
    purrr::set_names("lcz")

  lcz.name <- tibble::as_tibble(c(
    "Compact highrise", "Compact midrise", "Compact lowrise",
    "Open highrise", "Open midrise", "Open lowrise",
    "Lightweight low-rise", "Large lowrise", "Sparsely built",
    "Heavy Industry", "Dense trees", "Scattered trees",
    "Bush, scrub", "Low plants", "Bare rock or paved",
    "Bare soil or sand", "Water"
  )) %>%
    purrr::set_names("lcz.name")

  lcz.col <- tibble::as_tibble(c(
    "#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
    "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
    "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA"
  )) %>%
    purrr::set_names("lcz.col")

  if (method == "LCZ") {
    lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col) %>%
      dplyr::mutate(lcz = as.factor(.data$lcz)) %>%
      dplyr::inner_join(my_stations, by = "lcz") %>%
      dplyr::filter(.data$station %in% c(paste0(urban_station$station), paste0(rural_station$station))) %>%
      dplyr::mutate(reference = ifelse(.data$station == paste0(urban_station$station), "urban", "rural"))
  } else if (method == "manual") {
    lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col) %>%
      dplyr::mutate(lcz = as.factor(.data$lcz)) %>%
      dplyr::inner_join(my_stations, by = "lcz") %>%
      dplyr::filter(.data$station %in% c(paste0(urban_station), paste0(rural_station))) %>%
      dplyr::mutate(reference = ifelse(.data$station == paste0(urban_station), "urban", "rural"))
  }

  color_values <- lcz_df %>%
    dplyr::select(.data$reference, .data$lcz.col) %>%
    dplyr::distinct(.data$reference, .keep_all = TRUE)

  urban_col <- color_values %>% dplyr::filter(.data$reference == "urban") %>%
    dplyr::pull(.data$lcz.col)

  rural_col <- color_values %>% dplyr::filter(.data$reference == "rural") %>%
    dplyr::pull(.data$lcz.col)

  uhi_col <- c(uhi = "black")

  #Define the theme for graphics
  lcz_theme <- ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(color = "black", size = 16, hjust = 0.5),
    panel.background = ggplot2::element_rect(color = NA, fill = "grey97"),
    panel.grid.minor = ggplot2::element_line(color = "grey90"),
    panel.grid.major.y = ggplot2::element_line(color = "grey90"),
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.5)),
    axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
    axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.5)),
    axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
    legend.text = ggplot2::element_text(size = 13),
    legend.title = ggplot2::element_text(size = 14, face = "bold"),
    legend.key = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.02, "cm"),
    plot.margin = ggplot2::margin(25, 25, 10, 25),
    plot.caption = ggplot2::element_text(color = "grey40", hjust = 1, size = 10)
  )

  #Define hemisphere
  extract_hemisphere <- function(raster) {
    # Get the extent of the raster
    extent <- raster::extent(raster::raster(raster))
    # Check the ymin value of the extent
    if (extent@ymin >= 0) {
      hemisphere <- "northern"
    } else {
      hemisphere <- "southern"
    }

    return(hemisphere)
  }

  # Extract the hemisphere
  hemisphere <- extract_hemisphere(raster= {{ x }})

  my_latitude <- lcz_model$latitude[1]
  my_longitude <- lcz_model$longitude[1]

  # Define time series frequency with without "by"--------------------------------------------
  if (is.null(by)) {
    mydata <- openair::timeAverage(
      lcz_model,
      pollutant = "var_interp",
      avg.time = time.freq,
      type = c("reference")
    ) %>% stats::na.omit() %>%
      tidyr::pivot_wider(id_cols = .data$date, names_from = .data$reference, values_from = .data$var_interp) %>%
      stats::na.omit() %>%
      dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2))


    if (group == TRUE) {

      # Make A Y-Axis Transformer ----
      train_sec <- function(primary, secondary, na.rm = TRUE) {
        # Thanks Henry Holm for including the na.rm argument!
        from <- base::range(secondary, na.rm = na.rm)
        to   <- base::range(primary, na.rm = na.rm)
        # Forward transform for the data
        forward <- function(x) {
          scales::rescale(x, from = from, to = to)
        }
        # Reverse transform for the secondary axis
        reverse <- function(x) {
          scales::rescale(x, from = to, to = from)
        }
        base::list(fwd = forward, rev = reverse)
      }
      sec <- train_sec(mydata$urban, mydata$uhi)

      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature"), alpha = 0.8) +
        ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature"), alpha = 0.8) +
        ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI")) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
                                    guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_x_datetime(expand = c(0,0)) +
        ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
        ggplot2::theme_bw() + lcz_theme
    }
    else {
      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI"), alpha = 0.8) +
        ggplot2::scale_x_datetime(expand = c(0,0)) +
        ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
        ggplot2::theme_bw() + lcz_theme
    }

    if (isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_uhi_plot.png")
      ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
      file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_df.csv")
      utils::write.csv(mydata, file.2)
      file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_stations.csv")
      utils::write.csv(lcz_df, file.3)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    if (iplot == FALSE) {

      mydata$date <- lubridate::as_datetime(mydata$date)

      return(mydata)

    } else {

      return(final_graph)

    }

  }

  # Define with by  ---------------------------------------------------------

  if (!is.null(by)) {

    if (by %in% "day") {
      stop("The 'day' does not work with the argument by")
    }

    if (length(by) < 2 & by %in% c("daylight", "month", "year", "season", "seasonyear", "yearseason")) {

      mydata <- openair::cutData(lcz_model, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>% stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        hemisphere= hemisphere,
        latitude = my_latitude,
        longitude = my_longitude,
        type = c("reference", "my_time")
      ) %>% stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit() %>%
        dplyr::mutate(date = base::as.factor(date))

      label_format <- switch(
        by,
        "daylight" = "%H",
        "season" = "%b %d",
        "seasonyear" = "%b %d",
        "yearseason" = "%b %d",
        "year" = "%b %d",
        "month" = "%d",
        "%b %d"  # Default case if none of the above match
      )

      if (group == TRUE) {

        # Make A Y-Axis Transformer
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to   <- base::range(primary, na.rm = na.rm)
          # Forward transform for the data
          forward <- function(x) {
            scales::rescale(x, from = from, to = to)
          }
          # Reverse transform for the secondary axis
          reverse <- function(x) {
            scales::rescale(x, from = to, to = from)
          }
          base::list(fwd = forward, rev = reverse)
        }
        sec <- train_sec(mydata$urban, mydata$uhi)

        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group =1)) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
                                      guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(name = "",values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw()+ lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x", shrink = TRUE) +
          ggplot2::scale_x_discrete(expand = c(0,0),
                                    labels= function(x) base::format(lubridate::as_datetime(x), label_format),
                                    guide = ggplot2::guide_axis(check.overlap = TRUE
                                    )) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
                         strip.background = ggplot2::element_rect(linetype = "dotted"),
                         legend.box.spacing = ggplot2::unit(20, "pt"),
                         panel.spacing = ggplot2::unit(3, "lines"),
                         axis.ticks.x = ggplot2::element_blank()
          )

      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group= 1), alpha = 0.8) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw()+ lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x", shrink = TRUE) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_x_discrete(expand = c(0,0),
                                    labels= function(x) base::format(lubridate::as_datetime(x), label_format),
                                    guide = ggplot2::guide_axis(check.overlap = TRUE
                                    )) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
                         strip.background = ggplot2::element_rect(linetype = "dotted"),
                         legend.box.spacing = ggplot2::unit(20, "pt"),
                         panel.spacing = ggplot2::unit(3, "lines"),
                         axis.ticks.x = ggplot2::element_blank()
          )
      }


      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_uhi_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      if (iplot == FALSE) {

        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata)

      } else {

        return(final_graph)

      }


    }

    if (length(by) > 1 & by %in% "daylight") {

      mydata <- openair::cutData(lcz_model, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>% stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        hemisphere= hemisphere,
        latitude = my_latitude,
        longitude = my_longitude,
        type = c("reference", by)
      ) %>% stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, by), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit() %>%
        dplyr::mutate(date = base::as.factor(date))

      # convert the vector to a string
      by_reversed <- base::rev(by)
      by_str <- base::paste(by_reversed, collapse = " ~ ")

      # convert the string to a formula
      by_formula <- base::eval(base::parse(text = by_str))

      if (group == TRUE) {

        # Make A Y-Axis Transformer
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to   <- base::range(primary, na.rm = na.rm)
          # Forward transform for the data
          forward <- function(x) {
            scales::rescale(x, from = from, to = to)
          }
          # Reverse transform for the secondary axis
          reverse <- function(x) {
            scales::rescale(x, from = to, to = from)
          }
          base::list(fwd = forward, rev = reverse)
        }
        sec <- train_sec(mydata$urban, mydata$uhi)

        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group = 1)) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
                                      guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw()+ lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(by_formula, scales = "free_x") +
          ggplot2::scale_x_discrete(expand = c(0,0),
                                    guide = ggplot2::guide_axis(check.overlap = TRUE
                                    )) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(1, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group = 1), alpha = 0.8) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_grid(by_formula, scales = "free_x") +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_x_discrete(expand = c(0,0),
                                    guide = ggplot2::guide_axis(check.overlap = TRUE
                                    )) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(1, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      }

      if (isave == TRUE) {

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_uhi_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      if (iplot == FALSE) {

        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata)

      } else {

        return(final_graph)

      }

    }

    else {

      mydata <- openair::cutData(lcz_model, type = by,
                                 hemisphere = hemisphere,
                                 latitude = my_latitude,
                                 longitude = my_longitude
      ) %>% stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>% stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

      if (group == TRUE) {

       # Dual y-axis
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to   <- base::range(primary, na.rm = na.rm)
          # Forward transform for the data
          forward <- function(x) {
            scales::rescale(x, from = from, to = to)
          }
          # Reverse transform for the secondary axis
          reverse <- function(x) {
            scales::rescale(x, from = to, to = from)
          }
          base::list(fwd = forward, rev = reverse)
        }
        sec <- train_sec(mydata$urban, mydata$uhi)

        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group = 1)) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
                                      guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "",caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
          ggplot2::scale_x_datetime(expand = c(0,0), guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90))+
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold",hjust = 0,size = 10),
                         strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group = 1), alpha = 0.8) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black"))+
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "",caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_grid(~ my_time, scales = "free_x") +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_x_datetime(expand = c(0,0), guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90))+
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold",hjust = 0, size = 10),
                         strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      }


      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_uhi_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 10, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      if (iplot == FALSE) {

        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata)

      } else {

        return(final_graph)

      }

    }

  }

}
