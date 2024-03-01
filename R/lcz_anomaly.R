#' Explore LCZ Thermal Anomalies
#'
#' This function generates a graphical representation of thermal anomaly for different Local Climate Zones (LCZs)
#'
#' @param x A SpatRaster object containing the LCZ map. The LCZ map can be obtained using the lcz_get_map() function.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var Name of the variable, e.g. air temperature, in the dataframe.
#' @param station_id Name of the station ID variable in the dataframe.
#' @param ... Utilities from \code{selectBydata} from \code{openair} package. A start date string in the form e.g. \dQuote{1/2/1999} or in format i.e. \dQuote{YYYY-mm-dd}, \dQuote{1999-02-01}.
#'            A year or years to select e.g. year = 1998:2004 to select 1998-2004 inclusive or year = c(1998, 2004) to select 1998 and 2004. A month or months to select.
#'            Can either be numeric e.g. month = 1:6 to select months 1-6 (January to June), or by name e.g. month = c(\dQuote{January}, \dQuote{December}).
#' @param time.freq Defines the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time).See argument \emph{type} in openair package: https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option
#' @param hemisphere Hemisphere \dQuote{northern} or \dQuote{southern} for splitting data into \dQuote{season}, \dQuote{seasonyear}, \dQuote{daylight}, and \dQuote{dst}.
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param iplot Set to TRUE if you want to save the plot in your working directory.
#' @param isave Save the plot into your directory.
#' @param inclusive Set to TRUE to a colorblind-friendly palette.
#' @param ylab y-axis name.
#' @param xlab y-axis name. Default is \dQuote{Station}
#' @param title y-axis name. Default is \dQuote{" "}.
#' @param caption source data. Default can be \dQuote{Source: LCZ4r, Stewart and Oke, 2012; Demuzere et al.2022."}.
#'
#' @return A visual representation of the time series of air temperature of LCZ in \code{ggplot} format
#'
#' @export
#'
#' @examples
#'
#' # Hourly air temperature values in 2019.
#' # my_ts <- lcz_anomaly(lcz_map, df = lcz_data, var = "airT", station_id = "station", year = 2019)
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_map() to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_anomaly <- function(x,
                   data_frame = "",
                   var = "",
                   station_id = "",
                   ...,
                   time.freq = "hour",
                   by = NULL,
                   hemisphere = "northern",
                   impute = NULL,
                   iplot = FALSE,
                   isave = FALSE,
                   inclusive = FALSE,
                   ylab = "Air temperature anomaly in Degree Celsius",
                   xlab = "Meterological station",
                   title = "",
                   caption = "") {

  # Check and validate inputs -----------------------------------------------
  if (is.null(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map( )")
  }

  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast({{x}})
  }

  if(terra::crs(x, proj=TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {

    # If not, project it to WGS84
    x <- terra::project(x, "+proj=longlat +datum=WGS84 +no_defs")

  }
  # Validate the time series -----------------------------------------------

  if ("date" %in% names(data_frame)) {
    if (!(inherits(data_frame$date, "POSIXct") ||
          inherits(data_frame$date, "Date"))) {
      converted_col <-
        as.POSIXct(data_frame$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      data_frame$date <- converted_col
    }
  } else {
    base::cat("Column 'date' not found in the data frame.\n")
  }

  # Pre-processing time series ----------------------------------------------

  #Rename and define lcz_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{var}}, station = {{station_id}}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(lcz_id = dplyr::cur_group_id()) %>%
    openair::selectByDate(...)

  #Impute missing values if it is necessary
  if (!is.null(impute)) {
    if (impute == "mean") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_processed) %>%
        recipes::step_impute_mean(.data$var_interp)

      df_processed <- lcz_recipe %>%
        recipes::prep(df_processed) %>%
        recipes::bake(new_data = NULL)
    }

    if (impute == "median") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_processed) %>%
        recipes::step_impute_median(.data$var_interp)

      df_imputed <- lcz_recipe %>%
        recipes::prep(df_processed) %>%
        recipes::bake(new_data = NULL)
    }

    if (impute == "knn") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_processed) %>%
        recipes::step_impute_knn(.data$var_interp)

      df_imputed <- lcz_recipe %>%
        recipes::prep(df_processed) %>%
        recipes::bake(new_data = NULL)
    }

    if (impute == "bag") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_processed) %>%
        recipes::step_impute_bag(.data$var_interp)

      df_imputed <- lcz_recipe %>%
        recipes::prep(df_processed) %>%
        recipes::bake(new_data = NULL)
    }

    base::cat("Hooray! The missing values have been imputed with ", impute,"\n")
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

  lcz.name <-
    c(
      "Compact highrise",
      "Compact midrise",
      "Compact lowrise",
      "Open highrise",
      "Open midrise",
      "Open lowrise",
      "Lightweight low-rise",
      "Large lowrise",
      "Sparsely built",
      "Heavy Industry",
      "Dense trees",
      "Scattered trees",
      "Bush, scrub",
      "Low plants",
      "Bare rock or paved",
      "Bare soil or sand",
      "Water"
    ) %>%
    tibble::as_tibble() %>%
    purrr::set_names("lcz.name")

  lcz.col <-
    c(
      "#910613",
      "#D9081C",
      "#FF0A22",
      "#C54F1E",
      "#FF6628",
      "#FF985E",
      "#FDED3F",
      "#BBBBBB",
      "#FFCBAB",
      "#565656",
      "#006A18",
      "#00A926",
      "#628432",
      "#B5DA7F",
      "#000000",
      "#FCF7B1",
      "#656BFA"
    ) %>%
    tibble::as_tibble() %>%
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
  lcz.lables <- lcz_df$lcz.name

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

    final_graph <-
      ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
      ggplot2::geom_bar(stat='identity', width=.5) +
      ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
      ggplot2::geom_text(data = anomaly_cal %>% base::subset(.data$anomaly >0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
      ggplot2::geom_text(data = anomaly_cal %>% base::subset(.data$anomaly <0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
      ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                 guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
      ggplot2::coord_flip(expand = FALSE, clip = "off") +
      ggplot2::scale_y_continuous(limits = c(-0.4-max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
      ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ",
        caption = caption
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        panel.background = ggplot2::element_rect(),
        # plot.background = ggplot2::element_rect(fill = "grey90"),
        # panel.grid.minor = ggplot2::element_line(color = "grey90"),
        # panel.grid.major.y = ggplot2::element_line(color = "grey90"),
        # panel.grid.major.x = ggplot2::element_line(color = "grey90"),
        axis.text.x = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
        legend.text = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 13),
        legend.key = ggplot2::element_blank(),
        legend.spacing.y = ggplot2::unit(0.02, "cm"),
        plot.margin = ggplot2::margin(25, 25, 10, 25),
        plot.caption = ggplot2::element_text(color = "grey30", hjust = 1, size = 9)
      )

    if(isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_anomaly.png")
      ggplot2::ggsave(file, final_graph, height = 9, width = 14, units="in", dpi=300)

    }

    base::cat("You've successfully generated anomalies based on LCZ classes.\n")
    return(final_graph)

    if (iplot == FALSE) { return(anomaly_cal)}

  }

  if (!is.null(by)) {

    if(length(by)<2 & by %in% c("daylight", "season", "seasonyear")) {
      my_latitude <- lcz_model$latitude[1]
      my_longitude <- lcz_model$longitude[1]
      mydata <- openair::cutData(lcz_model, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>% tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata, pollutant = "var_interp",
                                     avg.time = time.freq,
                                     type = c("station", "my_time")) %>%
        tidyr::drop_na()

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

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat='identity', width=.4) +
        ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
        ggplot2::geom_text(data = base::subset(anomaly_cal, anomaly >0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
        ggplot2::geom_text(data = base::subset(anomaly_cal, anomaly <0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                   guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
        ggplot2::coord_flip(expand = FALSE, clip = "off") +
        ggplot2::scale_y_continuous(limits = c(-0.4 -max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::labs( title = title, x = xlab, y = ylab, fill = "LCZ",
                       caption = caption
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
          panel.background = ggplot2::element_rect(),
          #plot.background = ggplot2::element_rect(fill = "grey90"),
          # panel.grid.minor = ggplot2::element_line(color = "grey90"),
          # panel.grid.major.y = ggplot2::element_line(color = "grey90"),
          # panel.grid.major.x = ggplot2::element_line(color = "grey90"),
          axis.text.x = ggplot2::element_text(size = 12),
          axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
          legend.text = ggplot2::element_text(size = 13),
          legend.title = ggplot2::element_text(size = 13),
          legend.key = ggplot2::element_blank(),
          legend.spacing.y = ggplot2::unit(0.02, "cm"),
          plot.margin = ggplot2::margin(25, 25, 10, 25),
          plot.caption = ggplot2::element_text(color = "grey30", hjust = 1, size = 9))
      final_graph <-
        graph + ggplot2::facet_wrap(~ my_time, scales = "fixed") +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
                       strip.background = ggplot2::element_rect(linetype = "dotted"))

      if(isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder,"lcz_anomaly.png")
        ggplot2::ggsave(file, final_graph, height = 9, width = 14, units="in", dpi=300)

      }

      base::cat("You've successfully generated anomalies by", by,"based on LCZ classes.\n")
      return(final_graph)

      if (iplot == FALSE) {return(mydata)}

    }

    if(length(by)>1 & by %in% "daylight") {
      my_latitude <- lcz_model$latitude[1]
      my_longitude <- lcz_model$longitude[1]
      mydata <- openair::cutData(lcz_model, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>% tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "daylight", "my_time")
      ) %>% tidyr::drop_na()

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

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat='identity', width=.5) +
        ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
        ggplot2::geom_text(data = base::subset(anomaly_cal, anomaly >0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
        ggplot2::geom_text(data = base::subset(anomaly_cal, anomaly <0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                   guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
        ggplot2::coord_flip(expand = FALSE, clip = "off") +
        ggplot2::scale_y_continuous(limits = c(-0.4 -max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::labs( title = title, x = xlab, y = ylab, fill = "LCZ",
                       caption = caption
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
          #panel.background = ggplot2::element_rect(),
          #plot.background = ggplot2::element_rect(fill = "grey90"),
          # panel.grid.minor = ggplot2::element_line(color = "grey90"),
          # panel.grid.major.y = ggplot2::element_line(color = "grey90"),
          # panel.grid.major.x = ggplot2::element_line(color = "grey90"),
          axis.text.x = ggplot2::element_text(size = 12),
          axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
          legend.text = ggplot2::element_text(size = 13),
          legend.title = ggplot2::element_text(size = 13),
          legend.key = ggplot2::element_blank(),
          legend.spacing.y = ggplot2::unit(0.02, "cm"),
          plot.margin = ggplot2::margin(25, 25, 10, 25),
          plot.caption = ggplot2::element_text(color = "grey30", hjust = 1, size = 8))
      final_graph <-
        graph + ggplot2::facet_grid(my_time ~ daylight, scales = "fixed") +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 9),
                       strip.background = ggplot2::element_rect(linetype = "dotted"))

      if(isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder,"lcz_anomaly.png")
        ggplot2::ggsave(file, final_graph, height = 9, width = 14, units="in", dpi=300)

      }

      base::cat("You've successfully generated anomalies by", by,"based on LCZ classes.\n")
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
        tidyr::drop_na()

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

      graph <-
        ggplot2::ggplot(anomaly_cal, ggplot2::aes(x = .data$station, y = .data$anomaly, fill = .data$lcz)) +
        ggplot2::geom_bar(stat='identity', width=.4) +
        ggplot2::geom_hline(data=anomaly_cal$anomaly, yintercept = 0, linetype="dashed", color = "black") +
        ggplot2::geom_text(data = base::subset(anomaly_cal, anomaly >0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = -0.1, size = 4, fontface = "bold") +
        ggplot2::geom_text(data = base::subset(anomaly_cal, anomaly <0), ggplot2::aes(label = round(.data$anomaly, 1)), hjust = 1.1, size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables,
                                   guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
        ggplot2::coord_flip(expand = FALSE, clip = "off") +
        ggplot2::scale_y_continuous(limits = c(-0.4 -max(anomaly_cal$anomaly), max(anomaly_cal$anomaly) + 0.4)) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ",
                       caption = caption
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
          panel.background = ggplot2::element_rect(),
          #plot.background = ggplot2::element_rect(fill = "grey90"),
          # panel.grid.minor = ggplot2::element_line(color = "grey90"),
          # panel.grid.major.y = ggplot2::element_line(color = "grey90"),
          # panel.grid.major.x = ggplot2::element_line(color = "grey90"),
          axis.text.x = ggplot2::element_text(size = 12),
          axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
          legend.text = ggplot2::element_text(size = 13),
          legend.title = ggplot2::element_text(size = 13),
          legend.key = ggplot2::element_blank(),
          legend.spacing.y = ggplot2::unit(0.02, "cm"),
          plot.margin = ggplot2::margin(25, 25, 10, 25),
          plot.caption = ggplot2::element_text(color = "grey30", hjust = 1, size = 9))
      final_graph <-
        graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 10),
                       strip.background = ggplot2::element_rect(linetype = "dotted"))


      if(isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder,"lcz_anomaly.png")
        ggplot2::ggsave(file, final_graph, height = 9, width = 14, units="in", dpi=300)

      }

      base::cat("You've successfully generated anomalies by", by, "based on LCZ classes.\n")
      return(final_graph)

      if (iplot == FALSE) {return(anomaly_cal)}

    }


  }

}
