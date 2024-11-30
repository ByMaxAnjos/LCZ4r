#' Explore LCZ Thermal Anomalies
#'
#' This function generates a graphical representation of thermal anomaly for different Local Climate Zones (LCZs). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
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
#'   \item \strong{End date}: A string specifying the end date in either end="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#' }
#' @param time.freq Defines the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.
#' @param plot_type A character string indicating the type of plot to generate. Options include:
#' \itemize{
#'   \item \strong{diverging_bar}: A horizontal bar plot that diverges from the center (zero), with positive anomalies extending to the right and negative anomalies to the left. This plot is good for showing the extent and direction of anomalies in a compact format.
#'   \item \strong{bar}:  A bar plot showing the magnitude of the anomaly for each station, colored by whether the anomaly is positive or negative. This plot is good for comparing anomalies across stations.
#'   \item \strong{dot}: A dot plot that displays both the mean temperature values and the reference values, with lines connecting them. The size or color of the dots can indicate the magnitude of the anomaly. Ideal for showing both absolute temperature values and their anomalies.
#'   \item \strong{lollipop}: A lollipop plot where each "stick" represents an anomaly value and the dots at the top represent the size of the anomaly. Useful for clearly showing positive and negative anomalies in a minimalist way.
#' }
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time).See argument \emph{type} in openair package: \url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series) into your directory.
#' @param palette A character string specifying the color palette to use. The default is \dQuote{OKeeffe1}. You can choose from palettes available in \code{MetBrewer}: \url{https://github.com/BlakeRMills/MetBrewer?tab=readme-ov-file#palettes}.
#' @param save_extension A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.
#' @param inclusive Set to TRUE to a colorblind-friendly palette.
#' @param ylab y-axis name.
#' @param xlab y-axis name. Default is \dQuote{Station}
#' @param title y-axis name. Default is \dQuote{" "}.
#' @param caption source data. Default can be \dQuote{Source: LCZ4r, Stewart and Oke, 2012; Demuzere et al.2022."}.
#' @param legend_name Legend name for dot plot. Default is "Anomaly".
#'
#' @return A visual representation of the anomalies of air temperature of LCZ in \code{ggplot} or data frame .csv format.
#' @export
#'
#' @examples
#' \dontrun{
#' # Hourly air temperature values in 2019.
#' my_res <- lcz_anomaly(lcz_map, data_frame = lcz_data, var = "airT",
#'                        station_id = "station", year = 2020)
#' }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_anomaly <- function(x,
                        data_frame = "",
                        var = "",
                        station_id = "",
                        ...,
                        time.freq = "hour",
                        plot_type = "diverging_bar",
                        by = NULL,
                        impute = NULL,
                        iplot = TRUE,
                        isave = FALSE,
                        save_extension = "png",
                        inclusive = FALSE,
                        palette = "OKeeffe1",
                        ylab = "Air temperature anomaly",
                        xlab = "Stations",
                        title = "",
                        caption = "",
                        legend_name = "Anomaly") {
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

  # Rename and define lcz_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      lcz_id = dplyr::cur_group_id(),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    dplyr::ungroup() %>%
    openair::selectByDate(...)
  df_processed$var_interp <- base::as.numeric(df_processed$var_interp)
  df_processed$latitude <- base::as.numeric(df_processed$latitude)
  df_processed$longitude <- base::as.numeric(df_processed$longitude)

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

  # Convert lcz_map to polygon
  base::names(x) <- "lcz"
  lcz_shp <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)

  # Get shp LCZ stations from lat and long
  shp_stations <- df_processed %>%
    dplyr::distinct(.data$latitude, .data$longitude, .keep_all = T) %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Intersect poi shp stations with lcz shp
  lcz_stations <- sf::st_intersection(shp_stations, lcz_shp) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$lcz_id, .data$station, .data$lcz)

  # merge data-model with lcz_station to get lcz class
  lcz_model <-
    dplyr::inner_join(df_processed, lcz_stations, by = c("station", "lcz_id")) %>%
    dplyr::mutate(
      lcz = base::as.factor(.data$lcz),
      lcz_id = base::as.factor(.data$lcz_id),
      station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
    ) %>%
    dplyr::ungroup()

  # Settings for plots ------------------------------------------------------

  # Get id stations
  my_stations <- lcz_model %>%
    dplyr::distinct(.data$lcz_id, .data$lcz, .data$station)

  # Setting
  lcz <- c(base::seq(1, 10, 1), base::seq(11, 17)) %>%
    tibble::as_tibble() %>%
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

  lcz_colorblind <- c(
    "#E16A86", "#D8755E", "#C98027", "#B48C00",
    "#989600", "#739F00", "#36A631", "#00AA63",
    "#00AD89", "#00ACAA", "#00A7C5", "#009EDA",
    "#6290E5", "#9E7FE5", "#C36FDA", "#D965C6",
    "#E264A9"
  ) %>%
    tibble::as_tibble() %>%
    purrr::set_names("lcz_colorblind")

  lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col, lcz_colorblind) %>%
    dplyr::mutate(lcz = base::as.factor(.data$lcz)) %>%
    dplyr::inner_join(my_stations, by = "lcz")

  # Define qualitative palette
  if (inclusive == TRUE) {
    color_values <- lcz_df %>%
      dplyr::select(.data$lcz, .data$lcz_colorblind) %>%
      dplyr::pull(.data$lcz_colorblind, .data$lcz)
  } else {
    color_values <- lcz_df %>%
      dplyr::select(.data$lcz, .data$lcz.col) %>%
      dplyr::pull(.data$lcz.col, .data$lcz)
  }

  # Define LCZ labels
  lcz.lables <- lcz_df %>%
    dplyr::select(.data$lcz, .data$lcz.name) %>%
    dplyr::pull(.data$lcz.name, .data$lcz)

  nb.cols <- base::length(my_stations$station)
  mycolors <- MetBrewer::met.brewer(name = palette, nb.cols)


  lcz_theme <-
    ggplot2::theme(
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
      legend.title = ggplot2::element_text(size = 13),
      legend.key = ggplot2::element_blank(),
      legend.spacing.y = ggplot2::unit(0.02, "cm"),
      plot.margin = ggplot2::margin(25, 25, 10, 25),
      plot.caption = ggplot2::element_text(color = "grey40", hjust = 1, size = 10)
    )


  # Define time series frequency with argument "by"--------------------------------------------
  if (is.null(by)) {
    anomlay_lcz <- function(input = NULL) {
      mean_df <- lcz_model %>%
        dplyr::filter(.data$station == paste0(input)) %>%
        dplyr::group_by(.data$lcz_id, .data$lcz) %>%
        dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

      reference_df <- lcz_model %>%
        dplyr::filter(.data$station != paste0(input)) %>%
        dplyr::mutate(reference_value = mean(.data$var_interp))
      reference_df <- tibble::as.tibble(mean(reference_df$reference_value))

      merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
        dplyr::rename(reference_value = .data$value) %>%
        dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

      return(merged_data)
    }
    anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i) {
      anomlay_lcz(input = lcz_df$station[i])
    })

    anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

    anomaly_cal <- anomaly_cal %>%
      dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz_id, .data$station), by = "lcz_id")

    # Subset the data outside the ggplot call for clarity
    subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
    subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

    if(plot_type == "diverging_bar"){

      final_graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = stats::reorder(.data$station, .data$anomaly), y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat = "identity", width = .5) +
        ggplot2::geom_hline(data = anomaly_cal$anomaly, yintercept = 0, linetype = "dashed", color = "black") +
        ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, check_overlap = TRUE) +
        ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, check_overlap = TRUE) +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption) +
        ggplot2::theme_bw() +
        lcz_theme

    }

    if(plot_type == "bar") {

      final_graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$anomaly >0)) +
        ggplot2::geom_bar(stat = "identity", width = .5) +
        ggplot2::scale_fill_manual(values = c("#E44D3A", "#4A90E2"), name = "Anomaly", labels = c("Negative", "Positive")) +
        ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
        ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption) +
        ggplot2::theme_bw() +
        lcz_theme
    }

    if(plot_type == "dot") {

      final_graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station)) +
        ggplot2::geom_point(ggplot2::aes(y = .data$mean_value, color = .data$anomaly), size = 5) +
        ggplot2::geom_point(ggplot2::aes(y = .data$reference_value), color = "gray", size = 3) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$station, xend = .data$station, y = .data$reference_value, yend = .data$mean_value), linetype = "dotted") +
        MetBrewer::scale_color_met_c(name = palette, direction = -1) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
        ggplot2::labs(title = title, x = xlab, y = ylab, color = legend_name, caption = caption) +
        ggplot2::guides(
          color = ggplot2::guide_colorbar(
            title.position = "top",    # Move legend title to the top
            title.hjust = 0.5,         # Center align the title
            #barwidth = ggplot2::unit(10, "lines"),  # Set the width of the color bar
            barheight = ggplot2::unit(15, "lines"), # Set the height of the color bar
            ticks = TRUE,              # Show ticks on the color bar
            ticks.linewidth = 0.8     # Control the tick line width
          )
        ) +
        ggplot2::theme_bw() +
        lcz_theme
    }

    if(plot_type == "lollipop") {

      final_graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, color = .data$anomaly >0)) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$station, xend = .data$station, y = 0, yend = .data$anomaly), linetype = "solid") +
        ggplot2::geom_point(ggplot2::aes(size = base::abs(.data$anomaly))) +
        ggplot2::scale_color_manual(values = c("#E44D3A", "#4A90E2"), labels = c("Negative", "Positive")) +
        ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
        ggplot2::labs(title = title, x = xlab, y = ylab, color = "Anomaly", size = legend_name,caption = caption) +
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

      file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_plot.", save_extension)
      ggplot2::ggsave(file.1, final_graph, height = 7, width = 10, units = "in", dpi = 600)
      file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
      utils::write.csv(anomaly_cal, file.2)
      file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
      utils::write.csv(lcz_df, file.3)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    if (iplot == FALSE) {
      return(anomaly_cal)
    } else {
      return(final_graph)
    }
  }

  if (!is.null(by)) {
    if ("day" %in% by) {
      stop("The 'day' does not work with the argument by")
    }

    if (length(by) < 2 && any(c("daylight", "month", "year", "season", "seasonyear", "yearseason") %in% by)) {
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
      mydata <- openair::cutData(lcz_model,
        type = by, hemisphere = hemisphere,
        latitude = my_latitude, longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      anomlay_lcz <- function(input = NULL) {
        mean_df <- mydata %>%
          dplyr::filter(.data$station == paste0(input)) %>%
          dplyr::group_by(.data$my_time, .data$station) %>%
          dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

        reference_df <- mydata %>%
          dplyr::filter(.data$station != paste0(input)) %>%
          dplyr::group_by(.data$my_time) %>%
          dplyr::summarise(reference_value = mean(.data$var_interp))

        merged_data <- mean_df %>%
          dplyr::left_join(reference_df, by = "my_time") %>%
          dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

        return(merged_data)
      }
      anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i) {
        anomlay_lcz(input = lcz_df$station[i])
      })

      anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

      anomaly_cal <- anomaly_cal %>%
        dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz, .data$station), by = "station")
      # Subset the data outside the ggplot call for clarity
      subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
      subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

      if(plot_type == "diverging_bar"){

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = stats::reorder(.data$station, .data$anomaly), y = .data$anomaly, fill = .data$lcz)) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::geom_bar(stat = "identity", width = .4) +
          ggplot2::geom_hline(data = anomaly_cal$anomaly, yintercept = 0, linetype = "dashed", color = "black") +
          ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, check_overlap = TRUE) +
          ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, check_overlap = TRUE) +
          ggplot2::scale_fill_manual(
            values = color_values, name = "LCZ class", labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::coord_flip(clip = "off") +
          ggplot2::scale_y_continuous(
            limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~my_time, scales = "fixed") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
          )

      }

      if(plot_type == "bar") {

       graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$anomaly >0)) +
          ggplot2::geom_bar(stat = "identity", width = .5) +
          ggplot2::scale_fill_manual(values = c("#E44D3A", "#4A90E2"), name = "Anomaly", labels = c("Negative", "Positive")) +
          ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
       final_graph <-
         graph + ggplot2::facet_wrap(~my_time, scales = "fixed") +
         ggplot2::theme(
           strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
           strip.background = ggplot2::element_rect(linetype = "dotted"),
           legend.box.spacing = ggplot2::unit(20, "pt"),
           panel.spacing = ggplot2::unit(3, "lines")
         )
      }

      if(plot_type == "dot") {

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station)) +
          ggplot2::geom_point(ggplot2::aes(y = .data$mean_value, color = .data$anomaly), size = 5) +
          ggplot2::geom_point(ggplot2::aes(y = .data$reference_value), color = "gray", size = 3) +
          ggplot2::geom_segment(ggplot2::aes(x = .data$station, xend = .data$station, y = .data$reference_value, yend = .data$mean_value), linetype = "dotted") +
          MetBrewer::scale_color_met_c(name = palette, direction = -1) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = legend_name, caption = caption) +
          ggplot2::guides(
            color = ggplot2::guide_colorbar(
              title.position = "top",    # Move legend title to the top
              title.hjust = 0.5,         # Center align the title
              #barwidth = ggplot2::unit(10, "lines"),  # Set the width of the color bar
              barheight = ggplot2::unit(15, "lines"), # Set the height of the color bar
              ticks = TRUE,              # Show ticks on the color bar
              ticks.linewidth = 0.8     # Control the tick line width
            )
          ) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~my_time, scales = "fixed") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
          )
      }

      if(plot_type == "lollipop") {

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, color = .data$anomaly >0)) +
          ggplot2::geom_segment(ggplot2::aes(x = .data$station, xend = .data$station, y = 0, yend = .data$anomaly), linetype = "solid") +
          ggplot2::geom_point(ggplot2::aes(size = base::abs(.data$anomaly))) +
          ggplot2::scale_color_manual(values = c("#E44D3A", "#4A90E2"), labels = c("Negative", "Positive")) +
          ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = "Anomaly", size = legend_name,caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(~my_time, scales = "fixed") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
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

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      if (iplot == FALSE) {
        return(anomaly_cal)
      } else {
        return(final_graph)
      }
    }

    if (length(by) > 1 && "daylight" %in% by) {

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
      mydata <- openair::cutData(lcz_model,
        type = by, hemisphere = hemisphere,
        latitude = my_latitude, longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "daylight", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      anomlay_lcz <- function(input = NULL) {
        mean_df <- mydata %>%
          dplyr::filter(.data$station == paste0(input)) %>%
          dplyr::group_by(.data$daylight, .data$my_time, .data$station) %>%
          dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

        reference_df <- mydata %>%
          dplyr::filter(.data$station != paste0(input)) %>%
          dplyr::group_by(.data$daylight, .data$my_time) %>%
          dplyr::summarise(reference_value = mean(.data$var_interp), .groups = "drop")

        merged_data <- mean_df %>%
          dplyr::left_join(reference_df, by = c("daylight", "my_time")) %>%
          dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

        return(merged_data)
      }

      anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i) {
        anomlay_lcz(input = lcz_df$station[i])
      })

      anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

      anomaly_cal <- anomaly_cal %>%
        dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz, .data$station), by = "station")

      # Subset the data outside the ggplot call for clarity
      subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
      subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::geom_bar(stat = "identity", width = .5) +
        ggplot2::geom_hline(data = anomaly_cal$anomaly, yintercept = 0, linetype = "dashed", color = "black") +
        ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, check_overlap = TRUE) +
        ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, check_overlap = TRUE) +
        ggplot2::scale_fill_manual(
          values = color_values, name = "LCZ class", labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_continuous(
          limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
        ggplot2::theme_bw() +
        lcz_theme
      final_graph <-
        graph + ggplot2::facet_grid(my_time ~ daylight, scales = "fixed") +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
          strip.background = ggplot2::element_rect(linetype = "dotted"),
          legend.box.spacing = ggplot2::unit(20, "pt"),
          panel.spacing = ggplot2::unit(3, "lines")
        )

      if(plot_type == "diverging_bar"){

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = stats::reorder(.data$station, .data$anomaly), y = .data$anomaly, fill = .data$lcz)) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::geom_bar(stat = "identity", width = .4) +
          ggplot2::geom_hline(data = anomaly_cal$anomaly, yintercept = 0, linetype = "dashed", color = "black") +
          ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, check_overlap = TRUE) +
          ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, check_overlap = TRUE) +
          ggplot2::scale_fill_manual(
            values = color_values, name = "LCZ class", labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::coord_flip(clip = "off") +
          ggplot2::scale_y_continuous(
            limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme

        final_graph <-
          graph + ggplot2::facet_grid(my_time ~ daylight, scales = "fixed") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
          )

      }

      if(plot_type == "bar") {

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$anomaly >0)) +
          ggplot2::geom_bar(stat = "identity", width = .5) +
          ggplot2::scale_fill_manual(values = c("#E44D3A", "#4A90E2"), name = "Anomaly", labels = c("Negative", "Positive")) +
          ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_grid(my_time ~ daylight, scales = "fixed") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
          )
      }

      if(plot_type == "dot") {

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station)) +
          ggplot2::geom_point(ggplot2::aes(y = .data$mean_value, color = .data$anomaly), size = 5) +
          ggplot2::geom_point(ggplot2::aes(y = .data$reference_value), color = "gray", size = 3) +
          ggplot2::geom_segment(ggplot2::aes(x = .data$station, xend = .data$station, y = .data$reference_value, yend = .data$mean_value), linetype = "dotted") +
          MetBrewer::scale_color_met_c(name = palette, direction = -1) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = legend_name, caption = caption) +
          ggplot2::guides(
            color = ggplot2::guide_colorbar(
              title.position = "top",    # Move legend title to the top
              title.hjust = 0.5,         # Center align the title
              #barwidth = ggplot2::unit(10, "lines"),  # Set the width of the color bar
              barheight = ggplot2::unit(15, "lines"), # Set the height of the color bar
              ticks = TRUE,              # Show ticks on the color bar
              ticks.linewidth = 0.8     # Control the tick line width
            )
          ) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_grid(my_time ~ daylight, scales = "free") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
          )
      }

      if(plot_type == "lollipop") {

        graph <-
          ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, color = .data$anomaly >0)) +
          ggplot2::geom_segment(ggplot2::aes(x = .data$station, xend = .data$station, y = 0, yend = .data$anomaly), linetype = "solid") +
          ggplot2::geom_point(ggplot2::aes(size = base::abs(.data$anomaly))) +
          ggplot2::scale_color_manual(values = c("#E44D3A", "#4A90E2"), labels = c("Negative", "Positive")) +
          ggplot2::scale_y_continuous(limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
          ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = "Anomaly", size = legend_name,caption = caption) +
          ggplot2::theme_bw() +
          lcz_theme
        final_graph <-
          graph + ggplot2::facet_grid(my_time ~ daylight, scales = "free") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
            strip.background = ggplot2::element_rect(linetype = "dotted"),
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines")
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

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      if (iplot == FALSE) {
        return(anomaly_cal)
      } else {
        return(final_graph)
      }
    } else {
      mydata <-
        openair::cutData(lcz_model, type = by) %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      anomlay_lcz <- function(input = NULL) {
        mean_df <- mydata %>%
          dplyr::filter(.data$station == paste0(input)) %>%
          dplyr::group_by(.data$my_time, .data$station) %>%
          dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

        reference_df <- mydata %>%
          dplyr::filter(.data$station != paste0(input)) %>%
          dplyr::group_by(.data$my_time) %>%
          dplyr::summarise(reference_value = mean(.data$var_interp))

        merged_data <- mean_df %>%
          dplyr::left_join(reference_df, by = "my_time") %>%
          dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

        return(merged_data)
      }
      anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i) {
        anomlay_lcz(input = lcz_df$station[i])
      })

      anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

      anomaly_cal <- anomaly_cal %>%
        dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz, .data$station), by = "station")

      # Subset the data outside the ggplot call for clarity
      subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
      subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat = "identity", width = .4) +
        ggplot2::geom_hline(data = anomaly_cal$anomaly, yintercept = 0, linetype = "dashed", color = "black") +
        ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, check_overlap = TRUE) +
        ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, check_overlap = TRUE) +
        ggplot2::scale_fill_manual(
          values = color_values, name = "LCZ class", labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_continuous(
          limits = c(-0.4 - max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
        ggplot2::theme_bw() +
        lcz_theme
      final_graph <-
        graph + ggplot2::facet_wrap(~my_time, scales = "free_x") +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
          strip.background = ggplot2::element_rect(linetype = "dotted"),
          legend.box.spacing = ggplot2::unit(20, "pt"),
          panel.spacing = ggplot2::unit(1, "lines")
        )


      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      if (iplot == FALSE) {
        return(anomaly_cal)
      } else {
        return(final_graph)
      }
    }
  }
}
