
#' Analyze LCZ Time Series
#'
#' This function generates a graphical representation of time series air temperature data for different Local Climate Zones (LCZs). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
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
#' @param palette Default is "VanGogh2". Define your color palette from MetBrewer \url{https://github.com/BlakeRMills/MetBrewer?tab=readme-ov-file#palettes}.
#' @param ylab y-axis name.
#' @param xlab y-axis name. Default is \dQuote{Time}
#' @param title y-axis name. Default is \dQuote{" "}.
#' @param caption source data. Default can be \dQuote{Source:LCZ4r,2024"}.
#'
#' @return A visual representation of the time series of air temperature of LCZ in \code{ggplot} or data frame .csv format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Hourly air temperature values in 2019.
#' my_ts <- lcz_ts(lcz_map, df = lcz_data, var = "airT",
#'                  station_id = "station", year = 2019)
#'
#' }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for  \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_ts <- function(x,
                   data_frame = "",
                   var = "",
                   station_id = "",
                   ...,
                   time.freq = "hour",
                   by = NULL,
                   impute = NULL,
                   iplot = TRUE,
                   isave = FALSE,
                   palette = "VanGogh2",
                   ylab = "Air temperature [Degree Celsius]",
                   xlab = "Time",
                   title = "",
                   caption = "LCZ4r") {

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

  #Rename and define lcz_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(lcz_id = dplyr::cur_group_id(),
                  date = lubridate::as_datetime(.data$date)) %>%
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
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

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
    ) %>%
    dplyr::ungroup()

  # Settings for plots ------------------------------------------------------

  #Get id stations
  my_stations <- lcz_model %>%
    dplyr::distinct(.data$lcz_id, .data$lcz, .data$station)

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

  lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col) %>%
    dplyr::mutate(lcz = base::as.factor(.data$lcz)) %>%
    dplyr::inner_join(my_stations,  by = "lcz")

  # Define LCZ labels
  lcz.lables <- lcz_df$station
  nb.cols <- base::length(my_stations$station)
  mycolors <- MetBrewer::met.brewer(name = palette, nb.cols)

  lcz_theme <-
    ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(color = "black", size = 16, hjust = 0.5),
                   panel.background = ggplot2::element_rect(color = NA, fill = "white"),
                   panel.grid.minor = ggplot2::element_line(color = "white"),
                   panel.grid.major.y = ggplot2::element_line(color = "grey90"),
                   axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.5)),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.5)),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
                   legend.text = ggplot2::element_text(size = 13),
                   legend.title = ggplot2::element_text(size = 13, face = "bold"),
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

  lcz_model$latitude <- NULL
  lcz_model$longitude <- NULL

  # Define time series frequency with argument "by"--------------------------------------------
  if (is.null(by)) {
    mydata <- openair::timeAverage(
      lcz_model,
      pollutant = "var_interp",
      avg.time = time.freq,
      type = c("station")
    ) %>% stats::na.omit() %>%
      dplyr::ungroup()

    final_graph <-
      ggplot2::ggplot(mydata, ggplot2::aes(
                        x = .data$date,
                        y = .data$var_interp,
                        color = .data$station
                      )) +
      ggplot2::geom_line(lwd=1) +
      ggplot2::scale_x_datetime(expand = c(0,0)) +
      ggplot2::scale_color_manual(
        name = "Station (LCZ)",
        values = mycolors,
        labels = lcz.lables,
        guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
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

      file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_ts_plot.png")
      ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
      file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_df.csv")
      utils::write.csv(mydata, file.2)
      file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_stations.csv")
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

  if (!is.null(by)) {

    if ("day" %in% by ) {
      stop("The 'day' does not work with the argument by")
    }

    if (length(by) < 2 && c("month","year", "season", "seasonyear", "yearseason") %in% by) {

      mydata <- openair::cutData(lcz_model, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")) %>%
        stats::na.omit() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(date = base::as.factor(date))

      label_format <- switch(
        by,
        "season" = "%b %d",
        "seasonyear" = "%b %d",
        "yearseason" = "%b %d",
        "year" = "%b %d",
        "month" = "%d",
        "%b %d"  # Default case if none of the above match
      )

      graph <-
        ggplot2::ggplot(mydata,
                        ggplot2::aes(
                          x = .data$date,
                          y = .data$var_interp,
                          color = .data$station,
                          group = .data$station
                        )) +
        ggplot2::geom_line(lwd=1) +
        ggplot2::scale_color_manual(
          name = "Station (LCZ)",
          values = mycolors,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
        ggplot2::theme_bw() + lcz_theme
      final_graph <-
        graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
        ggplot2::scale_x_discrete(expand = c(0,0),
                                  breaks = function(x) x[seq(1, length(x), by = 4*24)],
                                  labels= function(x) base::format(lubridate::as_datetime(x), label_format),
                                  guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)) +
        ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::theme(
          legend.box.spacing = ggplot2::unit(20, "pt"),
          panel.spacing = ggplot2::unit(3, "lines"),
          axis.ticks.x = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(
            face = "bold",
            hjust = 0,
            size = 10
          ),
          strip.background = ggplot2::element_rect(linetype = "dotted")
        )


      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_ts_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_stations.csv")
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

    if (length(by) < 2 && c("daylight") %in% by) {

      mydata <- openair::cutData(lcz_model,
                                 type = by,
                                 hemisphere= hemisphere,
                                 latitude = my_latitude,
                                 longitude = my_longitude) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      rec_df <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$my_time)) %>%
        dplyr::ungroup()

      final_graph <-
        ggplot2::ggplot(mydata,
                        ggplot2::aes(
                          x = .data$date,
                          y = .data$var_interp,
                          color = .data$station,
                          group = .data$station
                        )) +
        ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf, fill = .data$labs),
                           alpha = 0.3, data = rec_df, inherit.aes = FALSE) +
        ggplot2::scale_fill_manual(name= "", values = c("lightblue", "grey")) +
        ggplot2::scale_x_datetime(expand = c(0, 0),
                                  breaks = scales::date_breaks("3 hour"),
                                  labels = scales::date_format("%H"),
                                  guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::geom_line(lwd=1) +
        ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(
          name = "Station (LCZ)",
          values = mycolors,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
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

        file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_ts_plot.png")
        ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
        file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_stations.csv")
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
                                 hemisphere= hemisphere,
                                 latitude = my_latitude,
                                 longitude = my_longitude) %>%
          stats::na.omit() %>%
          dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
          mydata,
          pollutant = "var_interp",
          avg.time = time.freq,
          type = c("station", by)) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      new_var <- rlang::syms(by)

      mydata2 <- mydata %>%
        dplyr::mutate(hour = lubridate::hour(.data$date)) %>%
        dplyr::group_by(.data$station, !!!new_var, .data$hour) %>%
        dplyr::summarize(var_interp= base::mean(.data$var_interp), .groups = "drop") %>%
        dplyr::ungroup()

      rec_df <- mydata %>%
        dplyr::group_by(!!!new_var) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$daylight), .groups = "drop") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(min_hour = lubridate::hour(.data$xmin),
               max_hour = lubridate::hour(.data$xmax))

      # convert the string to a formula
      by_formula <- stats::as.formula(base::paste("~", base::paste(by[2], collapse = " + ")))

      graph <-
          ggplot2::ggplot(mydata2, ggplot2::aes(x = .data$hour, y = .data$var_interp,
                            color = .data$station,
                            group = .data$station)) +
        ggplot2::geom_rect(ggplot2::aes(xmin = .data$min_hour, xmax = .data$max_hour, ymin = -Inf, ymax = Inf, fill = .data$labs),
                           alpha = 0.3, data = rec_df, inherit.aes = FALSE) +
        ggplot2::scale_fill_manual(name= "", values = c("lightblue", "grey")) +
        ggplot2::scale_x_continuous(expand = c(0,0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::geom_line(lwd=1) +
        ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = "Station (LCZ)",
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
          ggplot2::theme_bw() + lcz_theme
        final_graph <-
          graph + ggplot2::facet_wrap(by_formula, scales = "free_y") +
          ggplot2::theme(
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

        if (isave == TRUE){

          # Create a folder name using paste0
          folder <- base::paste0("LCZ4r_output/")

          # Check if the folder exists
          if (!base::dir.exists(folder)) {
            # Create the folder if it does not exist
            base::dir.create(folder)
          }

          file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_ts_plot.png")
          ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
          file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_df.csv")
          utils::write.csv(mydata2, file.2)
          file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_stations.csv")
          utils::write.csv(lcz_df, file.3)
          base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

        }

        if (iplot == FALSE) {

          mydata2$date <- lubridate::as_datetime(mydata2$date)

          return(mydata2)

        } else {

          return(final_graph)

        }

    }

    else {

      mydata <-
          openair::cutData(
            lcz_model,
            type = by,
            hemisphere = hemisphere,
            latitude = my_latitude,
            longitude = my_longitude) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
        mydata <- openair::timeAverage(
          mydata,
          pollutant = "var_interp",
          avg.time = time.freq,
          type = c("station", "my_time")) %>%
          dplyr::ungroup() %>%
          stats::na.omit()

      graph <-
          ggplot2::ggplot(mydata, ggplot2::aes(
                            x = .data$date,
                            y = .data$var_interp,
                            color = .data$station)) +
        ggplot2::geom_line(lwd=1) +
        ggplot2::scale_x_datetime(expand = c(0,0),guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(
            name = "Station (LCZ)",
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ",caption = caption) +
          ggplot2::theme_bw() + lcz_theme

        final_graph <-
          graph + ggplot2::facet_wrap(~ my_time, scales = "free_x") +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

        if (isave == TRUE){

          # Create a folder name using paste0
          folder <- base::paste0("LCZ4r_output/")

          # Check if the folder exists
          if (!base::dir.exists(folder)) {
            # Create the folder if it does not exist
            base::dir.create(folder)
          }

          file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_ts_plot.png")
          ggplot2::ggsave(file.1, final_graph, height = 7, width = 12, units="in", dpi=600)
          file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_df.csv")
          utils::write.csv(mydata, file.2)
          file.3 <- base::paste0(getwd(),"/", folder,"lcz4r_ts_stations.csv")
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
