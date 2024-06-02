#' Explore LCZ Thermal Anomalies
#'
#' This function generates a graphical representation of thermal anomaly for different Local Climate Zones (LCZs). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
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
#' @param time.freq Defines the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time).See argument \emph{type} in openair package: \url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series) into your directory.
#' @param inclusive Set to TRUE to a colorblind-friendly palette.
#' @param ylab y-axis name.
#' @param xlab y-axis name. Default is \dQuote{Station}
#' @param title y-axis name. Default is \dQuote{" "}.
#' @param caption source data. Default can be \dQuote{Source: LCZ4r, Stewart and Oke, 2012; Demuzere et al.2022."}.
#'
#' @return A visual representation of the anomalies of air temperature of LCZ in \code{ggplot} or data frame .csv format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Hourly air temperature values in 2019.
#' my_res <- lcz_anomaly(lcz_map, data_frame = lcz_data, var = "airT",
#'                        station_id = "station", year = 2019)
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
                   by = NULL,
                   impute = NULL,
                   iplot = FALSE,
                   isave = FALSE,
                   inclusive = FALSE,
                   ylab = "Air temperature anomaly",
                   xlab = "Stations",
                   title = "",
                   caption = "") {

  # Check and validate inputs -----------------------------------------------
  if (is.null(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map( )")
  }

  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast(x)
  }

  if(terra::crs(x, proj=TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {

    # If not, project it to WGS84
    x <- terra::project(x, "+proj=longlat +datum=WGS84 +no_defs")

  }
  x<- x[[1]]
  # Validate the time series -----------------------------------------------

  # Pre-processing time series ----------------------------------------------

  #Rename and define lcz_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(lcz_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
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
    sf::st_transform(crs = 4326)

  #Get shp LCZ stations from lat and long
  shp_stations <- df_processed %>%
    dplyr::distinct(.data$latitude, .data$longitude, .keep_all = T) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  #Intersect poi shp stations with lcz shp
  lcz_stations <- sf::st_intersection(shp_stations, lcz_shp) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$lcz_id, .data$station, .data$lcz)

  #merge data-model with lcz_station to get lcz class
  lcz_model <-
    dplyr::inner_join(df_processed, lcz_stations, by = c("station", "lcz_id")) %>%
    dplyr::mutate(
      lcz = base::as.factor(.data$lcz),
      lcz_id = base::as.factor(.data$lcz_id),
      station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
    )

  # Settings for plots ------------------------------------------------------

  #Get id stations
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

  lcz_colorblind <- c("#E16A86", "#D8755E", "#C98027", "#B48C00",
                      "#989600", "#739F00", "#36A631", "#00AA63",
                      "#00AD89", "#00ACAA", "#00A7C5", "#009EDA",
                      "#6290E5", "#9E7FE5", "#C36FDA", "#D965C6",
                      "#E264A9") %>%
    tibble::as_tibble() %>%
    purrr::set_names("lcz_colorblind")

  lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col, lcz_colorblind) %>%
    dplyr::mutate(lcz = base::as.factor(.data$lcz)) %>%
    dplyr::inner_join(my_stations,  by = "lcz")

  # Define qualitative palette
  if(inclusive == TRUE) {
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

  lcz_theme <-
    ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(color = "black", size = 16, hjust = 0.5),
    panel.background = ggplot2::element_rect(color = NA, fill = "grey97"),
    panel.grid.minor = ggplot2::element_line(color = "grey90"),
    panel.grid.major.y = ggplot2::element_line(color = "grey90"),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
    axis.text.y = ggplot2::element_text(size = 12),
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

    anomlay_lcz <- function(input = NULL){

      mean_df <- lcz_model %>%
        dplyr::filter(.data$station == paste0(input)) %>%
        dplyr::group_by(.data$lcz_id, .data$lcz) %>%
        dplyr::summarise(mean_value = mean(.data$var_interp),.groups = "drop")

      reference_df <- lcz_model %>%
        dplyr::filter(.data$station != paste0(input)) %>%
        dplyr::mutate(reference_value = mean(.data$var_interp))
      reference_df <- tibble::as.tibble(mean(reference_df$reference_value))

      merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
        dplyr::rename(reference_value = .data$value) %>%
        dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

      return(merged_data)
    }
    anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i)
      anomlay_lcz(input = lcz_df$station[i]))

    anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

    anomaly_cal <- anomaly_cal %>%
      dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz_id, .data$station), by = "lcz_id")
    # Subset the data outside the ggplot call for clarity
    subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
    subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

    final_graph <-
      ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
      ggplot2::geom_bar(stat='identity', width=.5) +
      ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
      ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
      ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
      ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables) +
      ggplot2::coord_flip(clip = "off")+
      ggplot2::scale_y_continuous(limits = c(-0.4-max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
      ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
      ggplot2::theme_bw() + lcz_theme

    if (isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_anomaly_plot.png")
      ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
      file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_df.csv")
      utils::write.csv(anomaly_cal, file.2)
      file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_stations.csv")
      utils::write.csv(lcz_df, file.3)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    return(final_graph)

    if (iplot == FALSE) { return(anomaly_cal)}

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
      mydata <- openair::timeAverage(mydata, pollutant = "var_interp",
                                     avg.time = time.freq,
                                     type = c("station", "my_time")) %>%
        stats::na.omit()

      anomlay_lcz <- function(input = NULL){

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
      anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i)
        anomlay_lcz(input = lcz_df$station[i]))

      anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

      anomaly_cal <- anomaly_cal %>%
        dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz, .data$station), by = "station")
      # Subset the data outside the ggplot call for clarity
      subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
      subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)
      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat='identity', width=.4) +
        ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
        ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
        ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                   guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
        ggplot2::coord_flip(clip = "off")+
        ggplot2::scale_y_continuous(limits = c(-0.4 -max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::labs( title = title, x = xlab, y = ylab, fill = "LCZ",caption = caption) +
        ggplot2::theme_bw() + lcz_theme
      final_graph <-
        graph + ggplot2::facet_wrap(~ my_time, scales = "fixed") +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
                       strip.background = ggplot2::element_rect(linetype = "dotted"))

      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_anomaly_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      return(final_graph)

      if (iplot == FALSE) {return(anomaly_cal)}

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
      mydata <- openair::timeAverage(mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "daylight", "my_time")
      ) %>% stats::na.omit()

      anomlay_lcz <- function(input = NULL){

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

       anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i)
        anomlay_lcz(input = lcz_df$station[i]))

      anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

      anomaly_cal <- anomaly_cal %>%
        dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz, .data$station), by = "station")

      # Subset the data outside the ggplot call for clarity
      subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
      subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat='identity', width=.5) +
        ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
        ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
        ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                   guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
        ggplot2::coord_flip(clip = "off")+
        ggplot2::scale_y_continuous(limits = c(-0.4 -max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::labs( title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
        ggplot2::theme_bw() + lcz_theme
      final_graph <-
        graph + ggplot2::facet_grid(my_time ~ daylight, scales = "fixed") +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 9),
                       strip.background = ggplot2::element_rect(linetype = "dotted"))

      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_anomaly_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      return(final_graph)

      if (iplot == FALSE) {return(anomaly_cal)}

    }

    else {
      mydata <-
        openair::cutData(lcz_model, type = by) %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")) %>%
        stats::na.omit()

      anomlay_lcz <- function(input = NULL){

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
      anomaly_job <- base::lapply(1:length(lcz_df$station), FUN = function(i)
        anomlay_lcz(input = lcz_df$station[i]))

      anomaly_cal <- do.call(rbind.data.frame, anomaly_job)

      anomaly_cal <- anomaly_cal %>%
        dplyr::left_join(lcz_df %>% dplyr::select(.data$lcz, .data$station), by = "station")

      # Subset the data outside the ggplot call for clarity
      subset_high <- anomaly_cal %>% dplyr::filter(.data$anomaly > 0)
      subset_low <- anomaly_cal %>% dplyr::filter(.data$anomaly < 0)

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat='identity', width=.4) +
        ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
        ggplot2::geom_text(data = subset_high, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
        ggplot2::geom_text(data = subset_low, ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                   guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
        ggplot2::coord_flip(clip = "off")+
        ggplot2::scale_y_continuous(limits = c(-0.4 -max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
        ggplot2::theme_bw() + lcz_theme
      final_graph <-
        graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
                       strip.background = ggplot2::element_rect(linetype = "dotted"))


      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_anomaly_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      return(final_graph)

      if (iplot == FALSE) {return(anomaly_cal)}

    }


  }

}
