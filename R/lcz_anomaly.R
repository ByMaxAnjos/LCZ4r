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
#' @param extract.method A character string specifying the method used to assign the LCZ class to each station point. The default is `"simple"`. Available methods are:
#'   \itemize{
#'     \item **simple**: Assigns the LCZ class based on the value of the raster cell in which the point falls. It often is used in low-density observational network.
#'     \item **two.step**: Assigns LCZs to stations while filtering out those located in heterogeneous LCZ areas. This method requires that at least 80% of the pixels within a 5 × 5 kernel match the LCZ of the center pixel (Daniel et al., 2017). Note that this method reduces the number of stations. It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
#'     \item **bilinear**: Interpolates the LCZ class values from the four nearest raster cells surrounding the point.
#'   }
#' @param plot_type A character string indicating the type of plot to generate. Options include:
#' \itemize{
#'   \item \strong{diverging_bar}: A horizontal bar plot that diverges from the center (zero), with positive anomalies extending to the right and negative anomalies to the left. This plot is good for showing the extent and direction of anomalies in a compact format.
#'   \item \strong{bar}:  A bar plot showing the magnitude of the anomaly for each station, colored by whether the anomaly is positive or negative. This plot is good for comparing anomalies across stations.
#'   \item \strong{dot}: A dot plot that displays both the mean temperature values and the reference values, with lines connecting them. The size or color of the dots can indicate the magnitude of the anomaly. Ideal for showing both absolute temperature values and their anomalies.
#'   \item \strong{lollipop}: A lollipop plot where each "stick" represents an anomaly value and the dots at the top represent the size of the anomaly. Useful for clearly showing positive and negative anomalies in a minimalist way.
#' }
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time).  Note that the **Daylight option may result in both daytime and nighttime hours being represented in UTC**.See NOAA \url{https://gml.noaa.gov/grad/solcalc/} and argument \emph{type} in openair package: \url{https://openair-project.github.io/book/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series) into your directory.
#' @param palette A character string specifying the color palette to use. The default is \dQuote{OKeeffe1}. You can choose from palettes available in \code{MetBrewer}: \url{https://github.com/BlakeRMills/MetBrewer?tab=readme-ov-file#palettes}.
#' @param save_extension A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.
#' @param inclusive Set to TRUE to a colorblind-friendly palette.
#' @param ylab y-axis label. `NULL` (default) uses a translated "Temperature anomaly (K)".
#' @param xlab x-axis label. `NULL` (default) uses a translated "Stations".
#' @param title y-axis name. Default is \dQuote{" "}.
#' @param caption source data. Default can be \dQuote{Source: LCZ4r, Stewart and Oke, 2012; Demuzere et al.2022."}.
#' @param legend_name Legend name. `NULL` (default) uses a translated "Temperature anomaly (K)".
#'
#' @param lang Language used for labels, messages and errors. Available codes:
#'   `"en"` (default), `"pt"`, `"es"`, `"fr"`, `"zh"`, `"de"`, `"it"`, `"ja"`,
#'   `"ko"`, `"ru"`, `"ar"`. See [lcz_langs()].
#'
#' @return A visual representation of the anomalies of air temperature of LCZ in \code{ggplot} or data frame .csv format.
#'
#' @author
#' Max Anjos (\url{https://github.com/ByMaxAnjos})
#'
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
                        extract.method = "simple",
                        plot_type = "diverging_bar",
                        by = NULL,
                        impute = NULL,
                        iplot = TRUE,
                        isave = FALSE,
                        save_extension = "png",
                        inclusive = FALSE,
                        palette = "okabe",
                        ylab = NULL,
                        xlab = NULL,
                        title = NULL,
                        caption = NULL,
                        legend_name = NULL,
                        lang = "en") {
  if (is.null(ylab)) ylab <- lcz_tr("anomaly", lang = lang)
  if (is.null(xlab)) xlab <- lcz_tr("stations", lang = lang)
  if (is.null(caption)) caption <- lcz_caption(lang)
  if (is.null(legend_name)) legend_name <- lcz_tr("anomaly", lang = lang)
  if (is.null(title)) title <- ""

  # Check and validate raster inputs -----------------------------------------------

  x <- lcz_check_raster(x, lang = lang)

  # Check data inputs -------------------------------------------------------
  lcz_check_data_frame(data_frame, var, station_id, lang = lang)

  if (!(extract.method %in% c("simple", "bilinear", "two.step"))) {
    lcz_abort("err_extract_method", lang = lang)
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

  if (!("latitude" %in% tolower(colnames(df_processed)))) {
    lcz_abort("err_col", lang = lang, arg = "latitude")
  }

  if (!("longitude" %in% tolower(colnames(df_processed)))) {
    lcz_abort("err_col", lang = lang, arg = "longitude")
  }

  # Rename and define lcz_id for each lat and long
  df_processed <- df_processed %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      lcz_id = dplyr::cur_group_id(),
      lcz_id = base::as.factor(.data$lcz_id),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    dplyr::ungroup() %>%
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
      lcz_abort("err_impute", lang = lang)
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
    lcz_msg("msg_imputed", lang = lang, method = impute)
  }

  # Geospatial operations ---------------------------------------------------

  # Convert lcz_map to polygon
  base::names(x) <- "lcz"
  # lcz_shp <- terra::as.polygons(x) %>%
  #   sf::st_as_sf() %>%
  #   sf::st_transform(crs = 4326)

  # Get shp LCZ stations from lat and long
  stations_mod <- df_processed %>%
    dplyr::distinct(.data$longitude, .data$latitude, .keep_all = T) %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (extract.method == "simple") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod))
    stations_lcz$ID <- NULL
    lcz_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      ) %>%
      dplyr::select(.data$station, .data$lcz, .data$lcz_id)

    lcz_model <- dplyr::inner_join(df_processed %>%
                                     dplyr::select(.data$date, .data$lcz_id, .data$var_interp, .data$latitude, .data$longitude),
                                   lcz_model, by="lcz_id")
  }

  if (extract.method == "bilinear") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "bilinear")
    stations_lcz$ID <- NULL
    stations_lcz$lcz <- as.integer(stations_lcz$lcz)
    lcz_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      ) %>%
      dplyr::select(.data$station, .data$lcz, .data$lcz_id)

    lcz_model <- dplyr::inner_join(df_processed %>%
                                     dplyr::select(.data$date, .data$lcz_id, .data$var_interp, .data$latitude, .data$longitude),
                                   lcz_model, by="lcz_id")
  }

  if (extract.method == "two.step") {
    # Step 2: Define a function to filter stations based on LCZ homogeneity
    filter_homogeneous_lcz <- function(df, raster, kernel_size = 5, threshold = 0.8) {
      # Exact k x k homogeneity at station locations (no per-cell R callback).
      df$homogeneity <- lcz_homogeneity(raster, df, kernel_size = kernel_size)
      df[!is.na(df$homogeneity) & df$homogeneity >= threshold, , drop = FALSE]
    }

    # Step 3: Apply the function to filter stations
    df_homogeneous <- filter_homogeneous_lcz(stations_mod, x, kernel_size = 5, threshold = 0.8)
    stations_lcz <- terra::extract(x, terra::vect(df_homogeneous))
    stations_lcz$ID <- NULL
    lcz_model <- base::cbind(df_homogeneous, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      ) %>%
      dplyr::select(.data$station, .data$lcz, .data$lcz_id)

    lcz_model <- dplyr::inner_join(df_processed %>%
                                     dplyr::select(.data$date, .data$lcz_id, .data$var_interp, .data$latitude, .data$longitude),
                                   lcz_model, by="lcz_id")
  }

  # Settings for plots ------------------------------------------------------

  # Get id stations
  my_stations <- lcz_model %>%
    dplyr::distinct(.data$lcz_id, .data$lcz, .data$station)

  # Setting
  lcz <- c(base::seq(1, 10, 1), base::seq(11, 17)) %>%
    tibble::as_tibble() %>%
    dplyr::rename(lcz= .data$value)

  lcz.name <- tibble::as_tibble(lcz_class_names(lang)) %>%
    dplyr::rename(lcz.name = .data$value)

  lcz.col <- tibble::as_tibble(lcz_palette()) %>%
    dplyr::rename(lcz.col = .data$value)

  lcz_colorblind <- tibble::as_tibble(lcz_palette(inclusive = TRUE)) %>%
    dplyr::rename(lcz_colorblind = .data$value)

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
  mycolors <- if (identical(palette, "okabe")) lcz_pal_categorical(nb.cols) else MetBrewer::met.brewer(name = palette, nb.cols)


  lcz_theme <- lcz_theme_series()

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
      reference_df <- tibble::as_tibble(mean(reference_df$reference_value))

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

    final_graph <- lcz_anomaly_figure(
      anomaly_cal, plot_type = plot_type, facet = FALSE,
      color_values = color_values, lcz_labels = lcz.lables,
      title = title, xlab = xlab, ylab = ylab,
      caption = caption, legend_name = legend_name, lang = lang
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
        n_st <- length(unique(anomaly_cal$station))
        fig_h <- max(95, min(240, 16 * n_st + 50))
        lcz_save_figure(final_graph,
          paste0("lcz4r_anomaly_plot.", save_extension),
          width = "double", height = fig_h, lang = lang, quiet = TRUE
        )
      file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
      utils::write.csv(anomaly_cal, file.2)
      file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
      utils::write.csv(lcz_df, file.3)
      lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
    }

    if (iplot == FALSE) {
      return(anomaly_cal)
    } else {
      return(final_graph)
    }
  }

  if (!is.null(by)) {
    if ("day" %in% by) {
      lcz_abort("err_by_day", lang = lang)
    }

    if (length(by) < 2 && any(c("daylight", "month", "year", "season", "seasonyear", "yearseason") %in% by)) {
      hemisphere <- lcz_hemisphere(x)

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

      final_graph <- lcz_anomaly_figure(
        anomaly_cal, plot_type = plot_type, facet = TRUE,
        color_values = color_values, lcz_labels = lcz.lables,
        title = title, xlab = xlab, ylab = ylab,
        caption = caption, legend_name = legend_name, lang = lang
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
        n_st <- length(unique(anomaly_cal$station))
        fig_h <- max(95, min(240, 16 * n_st + 50))
        lcz_save_figure(final_graph,
          paste0("lcz4r_anomaly_plot.", save_extension),
          width = "double", height = fig_h, lang = lang, quiet = TRUE
        )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      if (iplot == FALSE) {
        return(anomaly_cal)
      } else {
        return(final_graph)
      }
    }

    if (length(by) > 1 && "daylight" %in% by) {

      hemisphere <- lcz_hemisphere(x)

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

      final_graph <- lcz_anomaly_figure(
        anomaly_cal, plot_type = plot_type, facet = "time_daylight",
        color_values = color_values, lcz_labels = lcz.lables,
        title = title, xlab = xlab, ylab = ylab,
        caption = caption, legend_name = legend_name, lang = lang
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
        n_st <- length(unique(anomaly_cal$station))
        fig_h <- max(95, min(240, 16 * n_st + 50))
        lcz_save_figure(final_graph,
          paste0("lcz4r_anomaly_plot.", save_extension),
          width = "double", height = fig_h, lang = lang, quiet = TRUE
        )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
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

      final_graph <- lcz_anomaly_figure(
        anomaly_cal, plot_type = plot_type, facet = TRUE,
        color_values = color_values, lcz_labels = lcz.lables,
        title = title, xlab = xlab, ylab = ylab,
        caption = caption, legend_name = legend_name, lang = lang
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
        n_st <- length(unique(anomaly_cal$station))
        fig_h <- max(95, min(240, 16 * n_st + 50))
        lcz_save_figure(final_graph,
          paste0("lcz4r_anomaly_plot.", save_extension),
          width = "double", height = fig_h, lang = lang, quiet = TRUE
        )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_df.csv")
        utils::write.csv(anomaly_cal, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_anomaly_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      if (iplot == FALSE) {
        return(anomaly_cal)
      } else {
        return(final_graph)
      }
    }
  }
}

# Internal: one journal-grade builder shared by every `by` branch -------------
lcz_anomaly_figure <- function(data, plot_type,
                               facet = c("none", "time", "time_daylight"),
                               color_values, lcz_labels,
                               title, xlab, ylab, caption, legend_name,
                               lang = NULL) {
  if (is.logical(facet)) facet <- if (isTRUE(facet)) "time" else "none"
  facet <- match.arg(facet, c("none", "time", "time_daylight"))
  lim <- max(abs(data$anomaly), na.rm = TRUE)
  if (!is.finite(lim) || lim <= 0) lim <- 1
  pad <- lim * 0.14
  ylim <- c(-lim - pad, lim + pad)

  data$sign <- factor(
    data$anomaly >= 0,
    levels = c(FALSE, TRUE),
    labels = c(lcz_tr("negative", lang = lang), lcz_tr("positive", lang = lang))
  )
  sign_cols <- stats::setNames(
    c("#2166AC", "#B2182B"),
    c(lcz_tr("negative", lang = lang), lcz_tr("positive", lang = lang))
  )

  base <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = stats::reorder(.data$station, .data$anomaly))
  )

  if (identical(plot_type, "diverging_bar")) {
    p <- base +
      ggplot2::geom_col(ggplot2::aes(y = .data$anomaly, fill = .data$lcz),
                        width = 0.66) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey30") +
      ggplot2::geom_text(
        ggplot2::aes(y = .data$anomaly, label = sprintf("%.1f", .data$anomaly)),
        hjust = ifelse(data$anomaly >= 0, -0.15, 1.15),
        size = 2.9, colour = "grey20"
      ) +
      ggplot2::scale_fill_manual(
        values = color_values, labels = lcz_labels,
        name = lcz_tr("lcz_class", lang = lang),
        guide = ggplot2::guide_legend(ncol = if (length(color_values) > 10) 2 else 1)
      ) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(limits = ylim, expand = ggplot2::expansion(mult = c(0, 0)))
  } else if (identical(plot_type, "bar")) {
    p <- base +
      ggplot2::geom_col(ggplot2::aes(y = .data$anomaly, fill = .data$sign),
                        width = 0.66) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey30") +
      ggplot2::scale_fill_manual(
        values = sign_cols,
        name = lcz_tr("anomaly", lang = lang)
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = ylim, expand = ggplot2::expansion(mult = c(0, 0)))
  } else if (identical(plot_type, "dot")) {
    p <- base +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$station,
                     y = .data$reference_value, yend = .data$mean_value),
        linewidth = 0.25, colour = "grey65"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$reference_value),
        size = 2.1, colour = "grey45"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$mean_value, colour = .data$anomaly),
        size = 3.2
      ) +
      lcz_scale_colour_diverging(
        name = legend_name, limits = c(-lim, lim),
        guide = ggplot2::guide_colorbar(
          barwidth = ggplot2::unit(45, "mm"),
          barheight = ggplot2::unit(2.5, "mm"),
          title.position = "top", title.hjust = 0.5,
          frame.colour = "grey70", ticks.colour = "grey40"
        )
      ) +
      ggplot2::labs(x = xlab, y = lcz_tr("air_temp", lang = lang)) +
      ggplot2::coord_flip()
  } else {
    p <- base +
      ggplot2::geom_segment(
        ggplot2::aes(xend = .data$station, y = 0, yend = .data$anomaly,
                     colour = .data$sign),
        linewidth = 0.3
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$anomaly, colour = .data$sign,
                     size = abs(.data$anomaly))
      ) +
      ggplot2::scale_colour_manual(
        values = sign_cols,
        name = lcz_tr("anomaly", lang = lang)
      ) +
      ggplot2::scale_size_continuous(name = legend_name, guide = "none") +
      ggplot2::coord_flip()
  }

  p <- p +
    (if (identical(plot_type, "dot")) {
      ggplot2::labs(title = title, caption = caption)
    } else {
      ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption)
    }) +
    lcz_theme_series(
      legend_position = if (identical(plot_type, "dot")) "bottom" else "right",
      grid = "none"
    )

  if (identical(facet, "time")) {
    p <- p + ggplot2::facet_wrap(~my_time, scales = "fixed", ncol = 4)
  } else if (identical(facet, "time_daylight")) {
    p <- p + ggplot2::facet_grid(my_time ~ daylight, scales = "fixed")
  }
  p
}
