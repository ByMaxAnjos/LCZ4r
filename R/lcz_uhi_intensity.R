#' Assess the Urban Heat Island Intensity
#'
#' This function calculates the Urban Heat Island (UHI) intensity based on air temperature measurements and Local Climate Zones (LCZ). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
#'
#' @param x A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()} function.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var The name of the variable in the data frame representing air temperature.
#' @param station_id The name of the variable in the data frame representing station IDs.
#' @param ... Additional arguments for the \code{selectByDate} function from the \code{openair} package. These arguments allow for flexible selection of specific time periods (year, month, day, hour). Examples of how to use these arguments include:
#' \itemize{
#'   \item \strong{Year(s)}: Numeric value(s) specifying the year(s) to select. For example, \code{year = 1998:2004} selects all years between 1998 and 2004 (inclusive), while \code{year = c(1998, 2004)} selects only the years 1998 and 2004.
#'   \item \strong{Month(s)}: Numeric or character value(s) specifying the months to select. Numeric examples: \code{month = 1:6} (January to June), or character examples: \code{month = c("January", "December")}.
#'   \item \strong{Day(s)}: Numeric value(s) specifying the days to select. For instance, \code{day = 1:30} selects days from 1 to 30, or \code{day = 15} selects only the 15th day of the month.
#'   \item \strong{Hour(s)}: Numeric value(s) specifying the hours to select. For example, \code{hour = 0:23} selects all hours in a day, while \code{hour = 9} selects only the 9th hour.
#'   \item \strong{Start date}: A string specifying the start date in either start="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#'   \item \strong{End date}: A string specifying the start date in either end="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#' }
#' @param extract.method A character string specifying the method used to assign the LCZ class to each station point. The default is `"simple"`. Available methods are:
#'   \itemize{
#'     \item **simple**: Assigns the LCZ class based on the value of the raster cell in which the point falls. It often is used in low-density observational network.
#'     \item **two.step**: Assigns LCZs to stations while filtering out those located in heterogeneous LCZ areas. This method requires that at least 80% of the pixels within a 5 × 5 kernel match the LCZ of the center pixel (Daniel et al., 2017). Note that this method reduces the number of stations. It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
#'     \item **bilinear**: Interpolates the LCZ class values from the four nearest raster cells surrounding the point.
#'   }
#' @param method Method to calculate the UHI intensity. Options include: LCZ and manual. The LCZ method automatically identifies the LCZ build types, starting from LCZ 1 and progressing to LCZ 10, to represent the urban temperature, then it starts from LCZ natural LCZ (11-16) to represent the rural temperature. The manual method is a character string indicating the stations as references for the urban and rural areas.
#' @param Turban Urban station references, if the method "manual" is selected.
#' @param Trural Rural station references, if the method "manual" is selected.
#' @param group If TRUE urban, rural and UHI - related temperatures are grouped in the same plot.
#' @param time.freq The time period to average to. Default is \dQuote{hour}, but can also be \dQuote{day}, \dQuote{week}, \dQuote{month}, or \dQuote{year}.
#' @param by The time-series split option. Options include \dQuote{year}, \dQuote{season}, \dQuote{seasonyear}, \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend}, \dQuote{site}, \dQuote{daylight}, or \dQuote{dst} (daylight saving time). See the \code{type} argument in the \code{openair} package: \url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}.
#' @param impute Method to impute missing values. Options include \dQuote{mean}, \dQuote{median}, \dQuote{knn}, or \dQuote{bag}.
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series, station references) into your directory.
#' @param save_extension A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.
#' @param ylab The label for the y-axis.
#' @param ylab2 The label for the right y-axis.
#' @param xlab The label for the x-axis. Default is \dQuote{Time}.
#' @param title The title of the plot. Default is an empty string.
#' @param caption The source data caption.
#'
#' @return A visual representation of the time series of UHI in \code{ggplot} or data frame .csv format.
#'
#' @author
#' Max Anjos (\url{https://github.com/ByMaxAnjos})
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


lcz_uhi_intensity <- function(x,
                              data_frame = "",
                              var = "",
                              station_id = "",
                              ...,
                              time.freq = "hour",
                              extract.method = "simple",
                              method = "LCZ",
                              Turban = NULL,
                              Trural = NULL,
                              by = NULL,
                              impute = NULL,
                              group = FALSE,
                              iplot = TRUE,
                              isave = FALSE,
                              save_extension = "png",
                              ylab = "Air temperature [C]",
                              xlab = "Time",
                              ylab2 = "UHI",
                              title = "",
                              caption = "LCZ4r,2024") {
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

  if (!(extract.method %in% c("simple", "bilinear", "two.step"))) {
    stop("Invalid extract-based pixel model. Choose from 'simple', 'bilinear', or 'two.step'.")
  }

  # Pre-processing time series ----------------------------------------------

  #Latitude and Longitude
  df_processed <- data_frame %>%
    dplyr::rename(
      latitude = base::grep("(?i)^(lat|latitude)$", names(.), value = TRUE, perl = TRUE),
      longitude = base::grep("(?i)^(lon|long|longitude)$", names(.), value = TRUE, perl = TRUE),
      date = base::grep("(?i)^(date|time|timestamp|datetime)$", names(.), value = TRUE, perl = TRUE)
    )

  df_processed$latitude <- base::as.numeric(df_processed$latitude)
  df_processed$longitude <- base::as.numeric(df_processed$longitude)

  if (!("Latitude" %in% tolower(colnames(df_processed)) || "latitude" %in% tolower(colnames(df_processed)))) {
    stop("The 'latitude' input must be a column name in 'data_frame' representing each station's latitude.")
  }

  if (!("Longitude" %in% tolower(colnames(df_processed)) || "longitude" %in% tolower(colnames(df_processed)))) {
    stop("The 'longitude' input must be a column name in 'data_frame' representing each station's longitude")
  }

  # Rename and define my_id for each lat and long
  df_processed <- df_processed %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      station = base::as.factor(.data$station),
      my_id = dplyr::cur_group_id(),
      my_id = base::as.factor(.data$my_id),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    dplyr::select(.data$date, .data$var_interp, .data$station, .data$my_id, .data$longitude, .data$latitude) %>%
    openair::selectByDate(...)

  df_processed$var_interp <- base::as.numeric(df_processed$var_interp)

  # Impute missing values if necessary

  missing_values = c("NAN","NaN", "-9999", "-99", "NULL", "",
                     "NA", "N/A", "na", "missing", ".",
                     "inf", "-inf", 9999, 999, Inf, -Inf)
  df_processed <- df_processed %>%
    dplyr::mutate(var_interp = ifelse(.data$var_interp %in% missing_values, NA, .data$var_interp))

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

  # Convert lcz_map to polygon
  base::names(x) <- "lcz"
  # lcz_shp <- terra::as.polygons(x) %>%
  #   sf::st_as_sf() %>%
  #   sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  # Get shp LCZ stations from lat and long
  stations_mod <- df_processed %>%
    dplyr::distinct(.data$longitude, .data$latitude, .keep_all = T) %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (extract.method == "simple") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod))
    stations_lcz$ID <- NULL
    df_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz)
      )  %>%
      dplyr::inner_join(df_processed %>% dplyr::select(.data$my_id, .data$latitude, .data$longitude), by="my_id")
  }

  if (extract.method == "bilinear") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "bilinear")
    stations_lcz$ID <- NULL
    stations_lcz$lcz <- as.integer(stations_lcz$lcz)
    df_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz)
      ) %>%
      dplyr::inner_join(df_processed %>% dplyr::select(.data$my_id, .data$latitude, .data$longitude), by="my_id")
  }

  if (extract.method == "two.step") {
    # Step 2: Define a function to filter stations based on LCZ homogeneity
    filter_homogeneous_lcz <- function(df, raster, kernel_size = 5, threshold = 0.8) {
      # Convert stations to SpatVector
      stations_vect <- terra::vect(df)

      # Create a moving kernel to analyze LCZ homogeneity
      kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)

      # Extract LCZ values in the kernel around each station
      lcz_homogeneity <- terra::focal(raster, w = kernel, fun = function(values) {
        # Compute percentage of pixels matching the center pixel
        center_pixel <- values[ceiling(length(values) / 2)]
        if (is.na(center_pixel)) {
          return(NA)
        }
        mean(values == center_pixel, na.rm = TRUE)
      })

      # Extract homogeneity values at station locations
      homogeneity_values <- terra::extract(lcz_homogeneity, stations_vect)

      # Add homogeneity values to the station data frame
      df$homogeneity <- homogeneity_values[, 2]  # Second column contains the homogeneity value

      # Filter stations where homogeneity meets or exceeds the threshold
      df_filtered <- df %>%
        dplyr::filter(.data$homogeneity >= threshold)

      return(df_filtered)
    }

    # Step 3: Apply the function to filter stations
    df_homogeneous <- filter_homogeneous_lcz(stations_mod, x, kernel_size = 5, threshold = 0.8)
    stations_lcz <- terra::extract(x, terra::vect(df_homogeneous))
    stations_lcz$ID <- NULL
    df_model <- base::cbind(df_homogeneous, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz)
      )  %>%
      dplyr::inner_join(df_processed %>% dplyr::select(.data$my_id, .data$latitude, .data$longitude), by="my_id")
  }

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
      dplyr::filter(.data$station %in% c(urban_station$station, rural_station$station)) %>%
      dplyr::mutate(reference = ifelse(.data$station %in% c(urban_station$station), "urban", "rural"))
  }

  if (method == "manual") {
    urban_station <- Turban
    rural_station <- Trural

    lcz_model <- df_model %>%
      dplyr::filter(.data$station %in% c(urban_station, rural_station)) %>%
      dplyr::mutate(reference = ifelse(.data$station %in% c(urban_station), "urban", "rural"))

    if (is.null(lcz_model) || nrow(lcz_model) == 0) {
      stop("Unable to find suitable urban or rural station. Please, try the LCZ method")
    }
  }

  # Settings for plots ------------------------------------------------------

  lcz <- tibble::as_tibble(c(seq(1, 10, 1), seq(11, 17))) %>%
    dplyr::rename(lcz= .data$value)

  lcz.name <- tibble::as_tibble(c(
    "Compact highrise", "Compact midrise", "Compact lowrise",
    "Open highrise", "Open midrise", "Open lowrise",
    "Lightweight low-rise", "Large lowrise", "Sparsely built",
    "Heavy Industry", "Dense trees", "Scattered trees",
    "Bush, scrub", "Low plants", "Bare rock or paved",
    "Bare soil or sand", "Water"
  )) %>%
    dplyr::rename(lcz.name= .data$value)

  lcz.col <- tibble::as_tibble(c(
    "#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
    "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
    "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA"
  )) %>%
    dplyr::rename(lcz.col= .data$value)

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

  urban_col <- color_values %>%
    dplyr::filter(.data$reference == "urban") %>%
    dplyr::pull(.data$lcz.col)

  rural_col <- color_values %>%
    dplyr::filter(.data$reference == "rural") %>%
    dplyr::pull(.data$lcz.col)

  uhi_col <- c(uhi = "black")

  # Define the theme for graphics
  lcz_theme <- ggplot2::theme(
    legend.position = "top",
    plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(color = "black", size = 16, hjust = 0.5),
    plot.background = ggplot2::element_rect(fill="transparent", color=NA),
    legend.background = ggplot2::element_rect(fill="transparent", color=NA),
    panel.background = ggplot2::element_rect(color = NA, fill = "white"),
    panel.grid.minor = ggplot2::element_line(color = "white"),
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

  # Define hemisphere
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
  hemisphere <- extract_hemisphere(raster = {{ x }})

  my_latitude <- lcz_model$latitude[1]
  my_longitude <- lcz_model$longitude[1]

  lcz_model$latitude <- NULL
  lcz_model$longitude <- NULL

  # Define time series frequency with without "by"--------------------------------------------
  if (is.null(by)) {
    mydata <- openair::timeAverage(
      lcz_model,
      pollutant = "var_interp",
      avg.time = time.freq,
      type = c("reference")
    ) %>%
      dplyr::ungroup() %>%
      stats::na.omit() %>%
      tidyr::pivot_wider(id_cols = .data$date, names_from = .data$reference, values_from = .data$var_interp) %>%
      stats::na.omit() %>%
      dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2))

    if (group == TRUE) {
      # Make A Y-Axis Transformer ----
      train_sec <- function(primary, secondary, na.rm = TRUE) {
        # Thanks Henry Holm for including the na.rm argument!
        from <- base::range(secondary, na.rm = na.rm)
        to <- base::range(primary, na.rm = na.rm)
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
        ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature"), lwd = 1) +
        ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature"), lwd = 1) +
        ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI"), alpha = 0.5, lwd = 1) +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = "UHI"),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        ggplot2::scale_x_datetime(expand = c(0, 0)) +
        ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
        ggplot2::theme_bw() +
        lcz_theme
    } else {
      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI"), lwd = 1) +
        ggplot2::scale_x_datetime(expand = c(0, 0)) +
        ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
        ggplot2::theme_bw() +
        lcz_theme
    }

    if (isave == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
      ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
      file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
      utils::write.csv(mydata, file.2)
      file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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
    if ("day" %in% by) {
      stop("The 'day' does not work with the argument by")
    }

    if (length(by) < 2 && any(c("month", "year", "season", "seasonyear", "yearseason") %in% by)) {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())

      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit() %>%
        dplyr::mutate(date = base::as.factor(date))

      label_format <- switch(by,
        "season" = "%b %d",
        "seasonyear" = "%b %d",
        "yearseason" = "%b %d",
        "year" = "%b %d",
        "month" = "%d",
        "%b %d" # Default case if none of the above match
      )

      if (group == TRUE) {
        # Make A Y-Axis Transformer
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to <- base::range(primary, na.rm = na.rm)
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
          ggplot2::scale_x_discrete(
            expand = c(0, 0),
            breaks = function(x) x[seq(1, length(x), by = 4 * 24)],
            labels = function(x) base::format(lubridate::as_datetime(x), label_format),
            guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)
          ) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1),  lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group = 1), alpha = 0.5, lwd = 1) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~my_time, scales = "free_x", shrink = TRUE) +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines"),
            axis.ticks.x = ggplot2::element_blank()
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::scale_x_discrete(
            expand = c(0, 0),
            breaks = function(x) x[seq(1, length(x), by = 4 * 24)],
            labels = function(x) base::format(lubridate::as_datetime(x), label_format),
            guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)
          ) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group = 1), lwd = 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~my_time, scales = "free_x", shrink = TRUE) +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines"),
            axis.ticks.x = ggplot2::element_blank()
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

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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

    if (length(by) < 2 && "daylight" %in% by) {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())

      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

      rec_df <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$my_time)
        ) %>%
        dplyr::ungroup()

      if (group == TRUE) {
        # Make A Y-Axis Transformer
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to <- base::range(primary, na.rm = na.rm)
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
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf, fill = .data$labs),
            alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::scale_x_datetime(
            expand = c(0, 0),
            breaks = scales::date_breaks("3 hour"),
            labels = scales::date_format("%H"),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1), lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group = 1), alpha = 0.5, lwd = 1) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
      } else {
        final_graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf, fill = .data$labs),
            alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_x_datetime(
            expand = c(0, 0),
            breaks = scales::date_breaks("3 hour"),
            labels = scales::date_format("%H"),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group = 1), lwd = 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
      }


      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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

    if (length(by) > 1 && "daylight" %in% by) {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", by)
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, by), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

      new_var <- rlang::syms(by)

      mydata2 <- mydata %>%
        dplyr::mutate(hour = lubridate::hour(.data$date)) %>%
        dplyr::group_by(!!!new_var, .data$hour) %>%
        dplyr::summarize(
          rural = base::mean(.data$rural),
          urban = base::mean(.data$urban),
          uhi = base::mean(.data$uhi), .groups = "drop"
        ) %>%
        dplyr::ungroup()

      rec_df <- mydata %>%
        dplyr::group_by(!!!new_var) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$daylight), .groups = "drop"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          min_hour = lubridate::hour(.data$xmin),
          max_hour = lubridate::hour(.data$xmax)
        )

      # convert the string to a formula
      by_formula <- stats::as.formula(base::paste("~", base::paste(by[2], collapse = " + ")))

      if (group == TRUE) {
        # Make A Y-Axis Transformer
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to <- base::range(primary, na.rm = na.rm)
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
        sec <- train_sec(mydata2$urban, mydata2$uhi)

        graph <-
          ggplot2::ggplot(mydata2, ggplot2::aes(x = .data$hour)) +
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$min_hour, xmax = .data$max_hour, ymin = -Inf, ymax = Inf, fill = .data$labs),
            alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::scale_x_continuous(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1), lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group = 1), alpha = 0.5, lwd = 1) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(by_formula, scales = "free_x") +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(1, "lines"),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata2, ggplot2::aes(x = .data$hour)) +
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$min_hour, xmax = .data$max_hour, ymin = -Inf, ymax = Inf, fill = .data$labs),
            alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::scale_x_continuous(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group = 1), lwd = 1) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(by_formula, scales = "free_y") +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(1, "lines"),
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

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata2, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      if (iplot == FALSE) {
        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata2)
      } else {
        return(final_graph)
      }
    } else {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

      if (group == TRUE) {
        # Dual y-axis
        train_sec <- function(primary, secondary, na.rm = TRUE) {
          # Thanks Henry Holm for including the na.rm argument!
          from <- base::range(secondary, na.rm = na.rm)
          to <- base::range(primary, na.rm = na.rm)
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
          ggplot2::scale_x_datetime(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$urban, color = "Urban Temperature", group = 1), lwd = 1) +
          ggplot2::geom_line(ggplot2::aes(y = .data$rural, color = "Rural Temperature", group = 1), ldw = 1) +
          ggplot2::geom_line(ggplot2::aes(y = sec$fwd(.data$uhi), color = "UHI", group = 1), alpha = 0.5, lwd = 1) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(~ sec$rev(.), name = ylab2),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::scale_color_manual(name = "", values = c("Urban Temperature" = urban_col, "Rural Temperature" = rural_col, "UHI" = "black")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(~my_time, scales = "free_x") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )
      } else {
        graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(x = .data$date)) +
          ggplot2::scale_x_datetime(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::geom_line(ggplot2::aes(y = .data$uhi, color = "UHI", group = 1), lwd = 1) +
          ggplot2::scale_color_manual(name = "", values = c("UHI" = "black")) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme

        final_graph <-
          graph + ggplot2::facet_grid(~my_time, scales = "free_x") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
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

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 10, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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
