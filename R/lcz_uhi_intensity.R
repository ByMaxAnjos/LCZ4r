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
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_uhi_intensity <- function(x, data_frame = "", var = "", station_id = "", ..., time.freq = "hour",
                              method = "LCZ", Turban = "", Trural = "", by = NULL, impute = NULL,
                              group = FALSE, iplot = TRUE, isave = FALSE, ylab = "Air temperature [C]",
                              xlab = "Time", ylab2 = "UHI [C]", title = "", caption = "LCZ4r,2024") {

  # Check and validate inputs -----------------------------------------------
  if (is.null(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map( )")
  }

  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast(x)
  }

  if (terra::crs(x, proj=TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {

    # If not, project it to WGS84
    x <- terra::project(x, "+proj=longlat +datum=WGS84 +no_defs")

  }

  x<- x[[1]]


  # Pre-processing time series ----------------------------------------------

  #Rename and define my_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(my_id = dplyr::cur_group_id()) %>%
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

  # Select station urban and rural ------------------------------------------
  my_stations <- df_model %>%
    dplyr::distinct(.data$my_id, .data$lcz, .data$station)

  if (method == "LCZ") {
    select_stations <- function(data) {
      urban_preference <- c("1", "2", "3", "4")
      rural_preference <- c("12", "13", "14", "9", "6")

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
      stop("Unable to find suitable urban or rural station. Please, try the manual method")
    }

    lcz_model <- df_model %>%
      dplyr::filter(.data$station %in% c(paste0(urban_station$station), paste0(rural_station$station))) %>%
      dplyr::mutate(reference = ifelse(.data$station == paste0(urban_station$station), "urban", "rural"))

  }

  else if (method == "manual") {

    urban_station <- Turban
    rural_station <- Trural

    lcz_model <- df_model %>%
      dplyr::filter(.data$station %in% c(paste0(urban_station), paste0(rural_station))) %>%
      dplyr::mutate(reference = base::ifelse(.data$station == paste0(urban_station), "urban", "rural"))
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
    axis.text.x = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
    legend.text = ggplot2::element_text(size = 13),
    legend.title = ggplot2::element_text(size = 14, face = "bold"),
    legend.key = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.02, "cm"),
    plot.margin = ggplot2::margin(25, 25, 10, 25),
    plot.caption = ggplot2::element_text(color = "grey40", hjust = 1, size = 10)
  )

  # Function for Y-dual if group == TRUE

  if (group == TRUE) {
    transformer_dual_y_axis <- function(data, primary_column, secondary_column, include_y_zero = FALSE) {
    params_tbl <- data %>%
      dplyr::summarise(
        max_primary = max(!! dplyr::enquo(primary_column)),
        min_primary = min(!! dplyr::enquo(primary_column)),
        max_secondary = max(!! dplyr::enquo(secondary_column)),
        min_secondary = min(!! dplyr::enquo(secondary_column))
      )

    if (include_y_zero) {
      params_tbl$min_primary <- 0
      params_tbl$min_secondary <- 0
    }

    params_tbl <- params_tbl %>%
      dplyr::mutate(
        scale = (.data$max_secondary - .data$min_secondary) / (.data$max_primary - .data$min_primary),
        shift = .data$min_primary - .data$min_secondary
      )

    scale_func <- function(x) {
      x * params_tbl$scale - params_tbl$shift
    }

    inv_func <- function(x) {
      (x + params_tbl$shift) / params_tbl$scale
    }

    list(
      scale_func = scale_func,
      inv_func = inv_func,
      params_tbl = params_tbl
    )
  }
  }

  # Define time series frequency with argument "by"--------------------------------------------
  if (is.null(by)) {
    mydata <- openair::timeAverage(
      lcz_model,
      pollutant = "var_interp",
      avg.time = time.freq,
      type = c("reference")
    ) %>% stats::na.omit() %>%
      dplyr::ungroup(.data$reference) %>%
      tidyr::pivot_wider(id_cols = .data$date, names_from = .data$reference, values_from = .data$var_interp) %>%
      dplyr::mutate(uhi = .data$urban - .data$rural)
    # pivot_longer(c(rural, urban, uhi), names_to = "reference")

    if (group == TRUE) {

      # Make A Y-Axis Transformer ----
      transformer <- mydata %>%
        transformer_dual_y_axis(
          primary_column   = .data$rural,
          secondary_column = .data$uhi,
          include_y_zero   = TRUE
        )

      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature"), alpha = 0.8) +
        ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature"), alpha = 0.8) +
        ggplot2::geom_line(ggplot2::aes(y = transformer$inv_func(.data$uhi), color = "UHI")) +
        ggplot2::scale_y_continuous( sec.axis = ggplot2::sec_axis(~ transformer$scale_func(.), name = ylab2)) +
        ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
        ggplot2::theme_bw() + lcz_theme
    }
    else {
      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI"), alpha = 0.8) +
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

    return(final_graph)

    if (iplot == FALSE) {return(mydata)}

  }

  if (!is.null(by)) {

    if (length(by) < 2 & by %in% c("daylight", "season", "seasonyear")) {
      # Extract AXIS information from CRS
      axis_matches <- terra::crs({{x}}, parse = TRUE)[14]
      # Extract hemisphere from AXIS definition
      hemisphere <- base::ifelse(base::grepl("north", axis_matches), "northern", "southern")
      my_latitude <- lcz_model$latitude[1]
      my_longitude <- lcz_model$longitude[1]
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
        dplyr::ungroup(.data$my_time) %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = .data$urban - .data$rural)

      if (group == TRUE) {

        # Make A Y-Axis Transformer ----
        transformer <- mydata %>%
          transformer_dual_y_axis(
            primary_column   = .data$rural,
            secondary_column = .data$uhi,
            include_y_zero   = TRUE
          )

        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature"), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature"), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = transformer$inv_func(.data$uhi), color = "UHI")) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ transformer$scale_func(.), name = ylab2)) +
          ggplot2::scale_color_manual(name = "",
                                      values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw()+ lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x", shrink = TRUE) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
                         strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI"), alpha = 0.8) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw()+ lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x", shrink = TRUE) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
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
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_uhi_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      return(final_graph)

      if (iplot == FALSE) {return(mydata)}

    }

    if (length(by) > 1 & by %in% "daylight") {
      # Extract AXIS information from CRS
      axis_matches <- terra::crs({{x}}, parse = TRUE)[14]
      # Extract hemisphere from AXIS definition
      hemisphere <- base::ifelse(base::grepl("north", axis_matches), "northern", "southern")
      my_latitude <- lcz_model$latitude[1]
      my_longitude <- lcz_model$longitude[1]
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
        dplyr::ungroup(by) %>%
        tidyr::pivot_wider(id_cols = c(.data$date, by), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = .data$urban - .data$rural)

      # convert the vector to a string
      by_str <- base::paste(by, collapse = " ~ ")

      # convert the string to a formula
      by_formula <- base::eval(base::parse(text = by_str))

      if (group == TRUE) {
        # Make A Y-Axis Transformer ----
        transformer <- mydata %>%
          transformer_dual_y_axis(
            primary_column   = .data$rural,
            secondary_column = .data$uhi,
            include_y_zero   = TRUE
          )
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature"), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature"), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = transformer$inv_func(.data$uhi), color = "UHI")) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ transformer$scale_func(.), name = ylab2)) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw()+ lcz_theme

        final_graph <-
          graph + ggplot2::facet_grid(by_formula, scales = "free_x") +
          ggplot2::theme( strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
                          strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI"), alpha = 0.8) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_grid(by_formula, scales = "free_x") +
          ggplot2::theme( strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
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

      return(final_graph)

      if (iplot == FALSE) { return(mydata) }

    }

    else {
      mydata <- openair::cutData(lcz_model, type = by, hemisphere = hemisphere,
                                 latitude = my_latitude,
                                 longitude = my_longitude
      ) %>% stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>% stats::na.omit() %>% dplyr::ungroup(.data$my_time) %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = .data$urban - .data$rural)

      if (group == TRUE) {

        transformer <- mydata %>%
          transformer_dual_y_axis(
            primary_column   = .data$rural,
            secondary_column = .data$uhi,
            include_y_zero   = TRUE
          )

        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature"), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature"), alpha = 0.8) +
          ggplot2::geom_line(ggplot2::aes(y = transformer$inv_func(.data$uhi), color = "UHI")) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ transformer$scale_func(.), name = ylab2)) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "",caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold",hjust = 0,size = 12),
                         strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI"), alpha = 0.8) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black"))+
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "",caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold",hjust = 0,size = 12),
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

      return(final_graph)

      if (iplot == FALSE) { return(mydata) }

    }

  }


}
