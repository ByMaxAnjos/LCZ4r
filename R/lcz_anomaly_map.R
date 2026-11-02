#' Interpolate spatial and temporal thermal anomaly using a Local Climate Zones (LCZ).
#'
#' Generates an interpolated saptiotemporal anomaly map using LCZ as background.
#' This function applies geostatistical interpolation techniques (e.g., kriging) to estimate anomaly values (e.g., air temperature) across the study area,
#' integrating LCZ classifications. The function allows for flexible time period selection, like hour, daytime, nighttime and so on.
#'
#' @param x A `SpatRaster` object representing the LCZ map, obtained via `lcz_get_map()` functions.
#' @param data_frame A data frame containing air temperature measurements with station metadata.
#' The data must include a date column in hourly or higher resolution.
#' @param var Character. The variable to interpolate (e.g., `"airT"` for air temperature).
#' @param station_id Character. The column name in `data_frame` identifying weather stations.
#' @param ... Additional arguments passed to `selectByDate` from the `openair` package
#' for flexible time selection. Options include:
#'   \itemize{
#'     \item **Year(s):** `year = 1998:2004` (selects 1998 to 2004) or `year = c(1998, 2004)`.
#'     \item **Month(s):** `month = 1:6` (January to June) or `month = c("January", "December")`.
#'     \item **Day(s):** `day = 1:30` (days 1 to 30) or `day = 15`.
#'     \item **Hour(s):** `hour = 0:23` (all hours) or `hour = 9`.
#'     \item **Start/End Date:** `"YYYY-mm-dd"` format (e.g., `"1999-02-01"`).
#'   }
#' @param extract.method Character. The method used to assign LCZ classes to stations:
#'   \itemize{
#'     \item **"simple"**: Assigns the LCZ based on the raster cell where the station is located. It often is used in low-density observational network.
#'     \item **"two.step"**: Filters out stations in heterogeneous LCZ areas (Daniel et al., 2017). It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
#'     \item **"bilinear"**: Uses bilinear interpolation to assign LCZ values.
#'   }
#' @param vg.model Character. Variogram model for kriging. Default is `"Sph"` (spherical).
#' Available models:
#'   \itemize{
#'     \item `"Sph"`: Spherical model.
#'     \item `"Exp"`: Exponential model.
#'     \item `"Gau"`: Gaussian model.
#'     \item `"Ste"`: M. Stein's parameterization.
#'   }
#' @param sp.res Numeric. Spatial resolution in meters (default: `100`).
#' @param tp.res Character. Temporal resolution for averaging (`"hour"`, `"day"`, etc.).
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time). Note that the **Daylight option may result in both daytime and nighttime hours being represented in UTC**. See NOAA \url{https://gml.noaa.gov/grad/solcalc/} and argument \emph{type} in openair package: \url{https://openair-project.github.io/book/sections/intro/openair-package.html#the-type-option}
#' @param impute Character. Method to impute missing values (`"mean"`, `"median"`, `"knn"`, `"bag"`).
#' @param isave Logical. If `TRUE`, saves the plot to the working directory.
#' @param LCZinterp Logical. If `TRUE` (default), applies LCZ-based interpolation.
#'
#' @param lang Language used for labels, messages and errors. Available codes:
#'   `"en"` (default), `"pt"`, `"es"`, `"fr"`, `"zh"`, `"de"`, `"it"`, `"ja"`,
#'   `"ko"`, `"ru"`, `"ar"`. See [lcz_langs()].
#'
#' @return A spatially interpolated map in \code{terra raster GeoTIFF} format.
#'
#' @export
#'
#' @author
#' Max Anjos (\url{https://github.com/ByMaxAnjos})
#'
#' @references
#' Anjos, M., Targino, A. C., Krecl, P., Oukawa, G. Y. & Braga, R. F. Analysis of the urban heat island under different synoptic patterns using local climate zones. Build. Environ. 185, 107268 (2020).
#' Fenner, D., Meier, F., Bechtel, B., Otto, M. & Scherer, D. (2017). Intra and inter ‘local climate zone’ variability of air temperature as observed by crowdsourced citizen weather stations in Berlin, Germany. *Meteorol. Z.*, 26, 525–547.
#' \url{http://www.gstat.org/}
#'
#' @examples
#' \dontrun{
#' # Load sample data and LCZ map
#' data("lcz_data")
#' lcz_map <- lcz_get_map_generator(ID = "8576bde60bfe774e335190f2e8fdd125dd9f4299")
#'
#' # Plot the LCZ map
#' lcz_plot_map(lcz_map)
#'
#' # Perform interpolation anomalies for 2020-01-02 at 04:00h
#' my_interp <- lcz_anomaly_map(
#'   lcz_map, data_frame = lcz_data, var = "airT",
#'   station_id = "station", year = 2020, month = 1, day = 2, hour = 4,
#'   sp.res = 100, tp.res = "hour", vg.model = "Sph",
#'   LCZinterp = TRUE, isave = TRUE
#' )
#'
#' # Perform interpolation for an entire day (returns a raster stack of 24 hours)
#' my_interp <- lcz_anomaly_map(
#'   lcz_map, data_frame = lcz_data, var = "airT",
#'   station_id = "station", year = 2020, month = 1, day = 2,
#'   sp.res = 100, tp.res = "hour", vg.model = "Sph",
#'   LCZinterp = TRUE, isave = TRUE
#' )
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See \code{lcz_anomaly()} for LCZ interpolation evaluation.
#'
#' @keywords Local Climate Zone, anomaly, interpolation, kriging.


lcz_anomaly_map <- function(x,
                            data_frame = "",
                            var = "",
                            station_id = "",
                            ...,
                            sp.res = 100,
                            tp.res = "hour",
                            vg.model = "Sph",
                            extract.method = "simple",
                            by = NULL,
                            impute = NULL,
                            isave = FALSE,
                            LCZinterp = TRUE,
                            lang = "en") {
  # Check and validate raster inputs -----------------------------------------------

  x <- lcz_check_raster(x, lang = lang)

  # Check data inputs -------------------------------------------------------
  lcz_check_data_frame(data_frame, var, station_id, lang = lang)

  if (!(vg.model %in% c("Sph", "Exp", "Gau", "Ste"))) {
    lcz_abort("err_variogram", lang = lang)
  }

  if (!(extract.method %in% c("simple", "bilinear", "two.step"))) {
    lcz_abort("err_extract_method", lang = lang)
  }

  target_crs_lonlat <- "EPSG:4326"

  # Projection used for interpolation: UTM zone of the raster centre
  bbox <- terra::ext(x)
  lon <- (bbox[1] + bbox[2]) / 2
  lat <- (bbox[3] + bbox[4]) / 2
  utm_zone <- floor((lon + 180) / 6) + 1
  target_crs <- if (lat >= 0) {
    paste0("EPSG:326", utm_zone)
  } else {
    paste0("EPSG:327", utm_zone)
  }
  # Pre-processing time series ----------------------------------------------

  #Latitude and Longitude
  df_variable <- data_frame %>%
    dplyr::rename(
      latitude = base::grep("(?i)^(lat|latitude)$", names(.), value = TRUE, perl = TRUE),
      longitude = base::grep("(?i)^(lon|long|longitude)$", names(.), value = TRUE, perl = TRUE),
      date = base::grep("(?i)^(date|time|timestamp|datetime)$", names(.), value = TRUE, perl = TRUE)
    )

  df_variable$latitude <- base::as.numeric(df_variable$latitude)
  df_variable$longitude <- base::as.numeric(df_variable$longitude)

  if (!("Latitude" %in% tolower(colnames(df_variable)) || "latitude" %in% tolower(colnames(df_variable)))) {
    lcz_abort("err_col", lang = lang, arg = "latitude")
  }

  if (!("Longitude" %in% tolower(colnames(df_variable)) || "longitude" %in% tolower(colnames(df_variable)))) {
    lcz_abort("err_col", lang = lang, arg = "longitude")
  }


  # Rename and define my_id for each lat and long
  df_variable <- df_variable %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      my_id = dplyr::cur_group_id(),
      my_id = base::as.factor(.data$my_id),
      station = base::as.factor(.data$station),
      var_interp = base::as.numeric(.data$var_interp),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$date, .data$station, .data$my_id, .data$var_interp, .data$latitude, .data$longitude)

  # Impute missing values if necessary
  missing_values = c("NAN","NaN", "-9999", "-99", "NULL", "",
                     "NA", "N/A", "na", "missing", ".",
                     "inf", "-inf", 9999, 999, Inf, -Inf)
  df_variable <- df_variable %>%
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
    lcz_recipe <- recipes::recipe(var_interp ~ ., data = df_variable) %>%
      impute_function(.data$var_interp)
    df_variable <- lcz_recipe %>%
      recipes::prep(df_variable) %>%
      recipes::bake(new_data = NULL)
    lcz_msg("msg_imputed", lang = lang, method = impute)
  }

  # Define the period
  df_period <- df_variable %>%
    dplyr::select(.data$date, .data$station, .data$my_id, .data$var_interp) %>%
    openair::selectByDate(...) %>%
    openair::timeAverage(avg.time = tp.res, type = c("station", "my_id"))

  df_processed <- dplyr::inner_join(df_period,
    df_variable %>% dplyr::select(-.data$station, -.data$var_interp),
    by = c("date", "my_id")
  )

  # Geospatial operations ---------------------------------------------------
  # Convert lcz_map to polygon
  lcz_shp <- terra::as.polygons({{ x }}) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = target_crs_lonlat)

  # Get id stations
  my_stations <- df_processed %>%
    dplyr::distinct(.data$station, .keep_all = F)

  stations_mod <- df_processed %>%
    #dplyr::distinct(.data$longitude, .data$latitude, .keep_all = T) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = target_crs_lonlat)

  if (extract.method == "simple") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod))
    stations_lcz$ID <- NULL
    df_interp_mod <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = target_crs) %>%
      stats::na.omit()
  }

  if (extract.method == "bilinear") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "bilinear")
    stations_lcz$ID <- NULL
    stations_lcz$lcz <- as.integer(stations_lcz$lcz)
    df_interp_mod <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = target_crs) %>%
      stats::na.omit()
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
    df_interp_mod <- base::cbind(df_homogeneous, stations_lcz) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = target_crs) %>%
      stats::na.omit()
  }

  # Re-project and make a grid to interpolation
  x <- terra::project(x, target_crs)

  if (is.na(terra::crs(x))) {
    terra::crs(x) <- target_crs
  }

  lcz_box <- sf::st_transform(lcz_shp, crs = target_crs)
  ras_resolution <- sf::st_bbox(lcz_box) %>%
    stars::st_as_stars(dx = sp.res)
  ras_resolution <- terra::rast(ras_resolution)

  ras_project <- terra::project(x, target_crs)
  ras_resample <- terra::resample(ras_project, ras_resolution, method = "mode")
  ras_grid <- stars::st_as_stars(ras_resample, dimensions = "XY")
  base::names(ras_grid) <- "lcz"


  # Calculate anomaly temporal resolution  ------------------------------------------------------
  if (is.null(by)) { # Downscale to hour or day

    iyear <- df_interp_mod %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(.data$date) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(iyear = lubridate::year(.data$date)) %>%
      dplyr::distinct(.data$iyear, .keep_all = FALSE) %>%
      base::expand.grid()

    model_year <- function(iyear) {

      myyear <- iyear[1]

      modelyear <- df_interp_mod %>%
        dplyr::mutate(year  = lubridate::year(.data$date)) %>%
        dplyr::filter(.data$year  == paste0(myyear))

      imonth <- modelyear %>%
        dplyr::group_by(.data$date) %>%
        dplyr::select(.data$date) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(imonth = lubridate::month(.data$date)) %>%
        dplyr::distinct(.data$imonth, .keep_all = FALSE) %>%
        base::expand.grid()

      model_month <- function(imonth) {

        mymonth <- imonth[1]

        modelmonth <- modelyear %>%
          dplyr::mutate(month = lubridate::month(.data$date)) %>%
          dplyr::filter(.data$month == paste0(mymonth))

        iday <- modelmonth %>%
          dplyr::group_by(.data$date) %>%
          dplyr::select(.data$date) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(iday = lubridate::day(.data$date)) %>%
          dplyr::distinct(.data$iday, .keep_all = FALSE) %>%
          base::expand.grid()

        model_day <- function(iday) {

          myday <- iday[1]

          modelday <- df_interp_mod %>%
            dplyr::mutate(day = lubridate::day(.data$date)) %>%
            dplyr::filter(.data$day == paste0(myday))

          # Downscale to hour
          ihour <- modelday %>%
            dplyr::group_by(.data$date) %>%
            dplyr::select(.data$date) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(ihour = lubridate::hour(date)) %>%
            dplyr::distinct(.data$ihour, .keep_all = FALSE) %>%
            base::expand.grid()

          model_hour <- function(ihour) {
            myhour <- ihour[1]

            data_model <- modelday %>%
              dplyr::mutate(
                hour = lubridate::hour(.data$date),
                my_id = base::as.factor(.data$my_id)
              ) %>%
              dplyr::filter(.data$hour == paste0(myhour))

            anomlay_lcz <- function(input = NULL) {
              mean_df <- data_model %>%
                dplyr::filter(.data$station == paste0(input)) %>%
                dplyr::group_by(.data$my_id, .data$lcz) %>%
                dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

              reference_df <- data_model %>%
                dplyr::filter(.data$station != paste0(input)) %>%
                dplyr::mutate(reference_value = mean(.data$var_interp))
              reference_df <- tibble::as_tibble(mean(reference_df$reference_value))

              merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
                dplyr::rename(reference_value = .data$value) %>%
                dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

              return(merged_data)
            }
            anomaly_job <- base::lapply(1:base::length(my_stations$station), FUN = function(i) {
              anomlay_lcz(input = my_stations$station[i])
            })

            anomaly_cal <- base::do.call(rbind.data.frame, anomaly_job)

            lcz_anomaly_mod <- sf::st_as_sf(anomaly_cal) %>%
              sf::st_transform(crs = target_crs)


            if (LCZinterp == TRUE) {
              # [using ordinary kriging]
              krige_vgm <- lcz_fit_variogram(anomaly ~ lcz, lcz_anomaly_mod, vg.model, lang = lang)
              krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = lcz_anomaly_mod)
            } else {
              krige_vgm <- lcz_fit_variogram(anomaly ~ 1, lcz_anomaly_mod, vg.model, lang = lang)
              krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = lcz_anomaly_mod)
            }

            krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
            krige_map <- krige_map["var1.pred", , ]
            interp_map <- terra::rast(krige_map)
            interp_map <- terra::focal(interp_map, w = 7, fun = mean)
            mydate <- data_model %>% dplyr::pull(.data$date)
            mydate <- base::gsub("[: -]", "", mydate[1], perl = TRUE)
            ras_name <- ifelse(LCZinterp, base::paste0("lcz_krige_", mydate), base::paste0("krige_", mydate))
            base::names(interp_map) <- ras_name
            return(interp_map)
          }

          MapHour <- base::apply(ihour, 1, model_hour)
          interp_hour <- base::unlist(MapHour)
          interp_hour <- terra::rast(interp_hour)
          return(interp_hour)
        }

        MapDay <- base::apply(iday, 1, model_day)
        interp_day <- base::unlist(MapDay)
        interp_day <- terra::rast(interp_day)

        return(interp_day)

      }

      MapMonth <- base::apply(imonth, 1, model_month)
      interp_Month <- base::unlist(MapMonth)
      interp_Month <- terra::rast(interp_Month)

      return(interp_Month)

    }

    MapYear <- base::apply(iyear, 1, model_year)
    interp_year <- base::unlist(MapYear)
    interp_stack <- terra::rast(interp_year)

    if (isave == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      # Save map as raster.tif
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
    }

    return(interp_stack)
  }

  if (!is.null(by)) {
    if (by %in% "day") {
      lcz_abort("err_by_day", lang = lang)
    }

    if (length(by) < 2 && any(c("hour","daylight", "month", "monthyear", "year", "season", "seasonyear", "yearseason") %in% by)) {

      stations_geometry <- dplyr::select(stations_mod, .data$my_id, .data$geometry) %>%
        dplyr::distinct(.data$my_id, .keep_all = TRUE) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = target_crs)

      hemisphere <- lcz_hemisphere(x)
      my_latitude <- df_processed$latitude[1]
      my_longitude <- df_processed$longitude[1]
      df_interp_mod$geometry <- NULL
      mydata <- openair::cutData(df_interp_mod,
                                 type = by, hemisphere = hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>%
        tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())

      iby <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {

        my_by <- iby[1]

        data_model <- mydata %>%
          dplyr::filter(.data$my_time == paste0(my_by)) %>%
          dplyr::group_by(.data$my_id, .data$station, .data$my_time) %>%
          dplyr::summarise(var_interp = mean(.data$var_interp), .groups = "drop") %>%
          dplyr::ungroup() %>%
          dplyr::inner_join(stations_geometry, by = "my_id") %>%
          sf::st_as_sf()

        anomlay_lcz <- function(input = NULL) {
          mean_df <- data_model %>%
            dplyr::filter(.data$station == paste0(input)) %>%
            dplyr::group_by(.data$my_id, .data$lcz) %>%
            dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

          reference_df <- data_model %>%
            dplyr::filter(.data$station != paste0(input)) %>%
            dplyr::mutate(reference_value = mean(.data$var_interp))
          reference_df <- tibble::as_tibble(mean(reference_df$reference_value))

          merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
            dplyr::rename(reference_value = .data$value) %>%
            dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

          return(merged_data)
        }
        anomaly_job <- base::lapply(1:base::length(my_stations$station), FUN = function(i) {
          anomlay_lcz(input = my_stations$station[i])
        })

        anomaly_cal <- base::do.call(rbind.data.frame, anomaly_job)

        lcz_anomaly_mod <- sf::st_as_sf(anomaly_cal) %>%
          sf::st_transform(crs = target_crs)


        if (LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- lcz_fit_variogram(anomaly ~ lcz, lcz_anomaly_mod, vg.model, lang = lang)
          krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = lcz_anomaly_mod)

        } else {
          krige_vgm <- lcz_fit_variogram(anomaly ~ 1, lcz_anomaly_mod, vg.model, lang = lang)
          krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = lcz_anomaly_mod)
        }

        krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred", , ]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w = 7, fun = mean)
        ras_name <- ifelse(LCZinterp, base::paste0("lcz_krige_", my_by), base::paste0("krige_", my_by))
        base::names(interp_map) <- ras_name
        return(interp_map)
      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        # Save map as raster.tif
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
        terra::writeRaster(interp_stack, file, overwrite = TRUE)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }
      return(interp_stack)
    }

    if (length(by) > 1 & by %in% "daylight") {

      stations_geometry <- dplyr::select(stations_mod, .data$my_id, .data$geometry) %>%
        dplyr::distinct(.data$my_id, .keep_all = TRUE) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = target_crs)

      hemisphere <- lcz_hemisphere(x)

      my_latitude <- df_processed$latitude[1]
      my_longitude <- df_processed$longitude[1]
      df_interp_mod$geometry <- NULL
      mydata <- openair::cutData(df_interp_mod,
                                 type = by, hemisphere = hemisphere,
                                 latitude = my_latitude, longitude = my_longitude
      ) %>%
        tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())

      iby <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {
        my_by <- iby$daylight[1]
        my_by2 <- iby$my_time[1]

        data_model <- mydata %>%
          dplyr::filter(.data$daylight == paste0(my_by)) %>%
          dplyr::filter(.data$my_time == paste0(my_by2)) %>%
          dplyr::group_by(.data$station, .data$my_id,.data$daylight, .data$my_time) %>%
          dplyr::summarise(var_interp = mean(.data$var_interp), .groups = "drop") %>%
          dplyr::ungroup() %>%
          dplyr::inner_join(stations_geometry, by = "my_id") %>%
          sf::st_as_sf()

        anomlay_lcz <- function(input = NULL) {
          mean_df <- data_model %>%
            dplyr::filter(.data$station == paste0(input)) %>%
            dplyr::group_by(.data$my_id, .data$lcz) %>%
            dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

          reference_df <- data_model %>%
            dplyr::filter(.data$station != paste0(input)) %>%
            dplyr::mutate(reference_value = mean(.data$var_interp))
          reference_df <- tibble::as_tibble(mean(reference_df$reference_value))

          merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
            dplyr::rename(reference_value = .data$value) %>%
            dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

          return(merged_data)
        }
        anomaly_job <- base::lapply(1:base::length(my_stations$station), FUN = function(i) {
          anomlay_lcz(input = my_stations$station[i])
        })

        anomaly_cal <- base::do.call(rbind.data.frame, anomaly_job)

        lcz_anomaly_mod <- sf::st_as_sf(anomaly_cal) %>%
          sf::st_transform(crs = target_crs)


        if (LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- lcz_fit_variogram(anomaly ~ lcz, lcz_anomaly_mod, vg.model, lang = lang)
          krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = lcz_anomaly_mod)
        } else {
          krige_vgm <- lcz_fit_variogram(anomaly ~ 1, lcz_anomaly_mod, vg.model, lang = lang)
          krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = lcz_anomaly_mod)
        }

        krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred", , ]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w = 7, fun = mean)
        ras_name <- ifelse(LCZinterp, base::paste0("lcz_krige_", my_by,"_", my_by2), base::paste0("krige_", my_by, "_", my_by2))
        base::names(interp_map) <- ras_name
        return(interp_map)
      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        # Save map as raster.tif
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
        terra::writeRaster(interp_stack, file, overwrite = TRUE)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      return(interp_stack)
    }
  }

}

