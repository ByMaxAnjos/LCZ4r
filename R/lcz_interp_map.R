#' Interpolate spatial and temporal data using a Local Climate Zones (LCZ)
#'
#' Generates an interpolated spatial map using LCZ as background.
#' This function applies geostatistical interpolation techniques (e.g., kriging) to estimate variable values (e.g., air temperature) across the study area,
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
#' # Perform interpolation for 2020-01-02 at 04:00h
#' my_interp <- lcz_interp_map(
#'   lcz_map, data_frame = lcz_data, var = "airT",
#'   station_id = "station", year = 2020, month = 1, day = 2, hour = 4,
#'   sp.res = 100, tp.res = "hour", vg.model = "Sph",
#'   LCZinterp = TRUE
#' )
#'
#' # Perform interpolation for an entire day (returns a raster stack of 24 hours)
#' my_interp <- lcz_interp_map(
#'   lcz_map, data_frame = lcz_data, var = "airT",
#'   station_id = "station", year = 2020, month = 1, day = 2,
#'   sp.res = 100, tp.res = "hour", vg.model = "Sph",
#'   LCZinterp = TRUE
#' )
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See \code{lcz_inter_eval()} for LCZ interpolation evaluation.
#'
#' @keywords Local Climate Zone, interpolation, kriging.


lcz_interp_map <- function(x,
                           data_frame = "",
                           var = "",
                           station_id = "",
                           ...,
                           extract.method = "simple",
                           sp.res = 100,
                           tp.res = "hour",
                           vg.model = "Sph",
                           by = NULL,
                           impute = NULL,
                           isave = FALSE,
                           LCZinterp = TRUE) {
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

  if (!(vg.model %in% c("Sph", "Exp", "Gau", "Ste"))) {
    stop("Invalid viogram model. Choose from 'Sph', 'Exp', 'Gau', or 'Ste'.")
  }

  if (!(extract.method %in% c("simple", "bilinear", "two.step"))) {
    stop("Invalid extract-based pixel model. Choose from 'simple', 'bilinear', or 'two.step'.")
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
    stop("The 'latitude' input must be a column name in 'data_frame' representing each station's latitude.")
  }

  if (!("Longitude" %in% tolower(colnames(df_variable)) || "longitude" %in% tolower(colnames(df_variable)))) {
    stop("The 'longitude' input must be a column name in 'data_frame' representing each station's longitude")
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
    dplyr::ungroup()

  # Impute missing values if necessary
  missing_values = c("NAN","NaN", "-9999", "-99", "NULL", "",
                     "NA", "N/A", "na", "missing", ".",
                     "inf", "-inf", 9999, 999, Inf, -Inf)
  df_variable <- df_variable %>%
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
    lcz_recipe <- recipes::recipe(var_interp ~ ., data = df_variable) %>%
      impute_function(.data$var_interp)
    df_variable <- lcz_recipe %>%
      recipes::prep(df_variable) %>%
      recipes::bake(new_data = NULL)
    base::message("The missing values have been imputed with ", impute)
  }

  # Define the period
  df_period <- df_variable %>%
    dplyr::select(.data$date, .data$station, .data$my_id, .data$var_interp) %>%
    openair::selectByDate(...) %>%
    openair::timeAverage(avg.time = tp.res, type = c("station", "my_id")) %>%
    dplyr::ungroup()

  get_lat <- df_variable %>%
    dplyr::distinct(.data$my_id, .keep_all = T) %>%
    dplyr::select(.data$my_id, .data$latitude, .data$longitude)

  df_processed <- dplyr::inner_join(df_period, get_lat, by = c("my_id")) %>%
    stats::na.omit()


  # Geospatial operations ---------------------------------------------------
  # Convert lcz_map to polygon
  lcz_shp <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)

  #Stratified splitting by LCZ)
  stations_mod <- df_processed %>%
    #dplyr::distinct(.data$longitude, .data$latitude, .keep_all = T) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (extract.method == "simple") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod))
    stations_lcz$ID <- NULL
    df_interp_mod <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = 3857) %>%
      stats::na.omit()
  }

  if (extract.method == "bilinear") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "bilinear")
    stations_lcz$ID <- NULL
    stations_lcz$lcz <- as.integer(stations_lcz$lcz)
    df_interp_mod <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = 3857) %>%
      stats::na.omit()
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
    df_interp_mod <- base::cbind(df_homogeneous, stations_lcz) %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = 3857) %>%
      stats::na.omit()
  }

  # Re-project and make a grid to interpolation
  lcz_box <- sf::st_transform(lcz_shp, crs = 3857)
  ras_resolution <- sf::st_bbox(lcz_box) %>%
    stars::st_as_stars(dx = sp.res)
  ras_resolution <- terra::rast(ras_resolution)

  ras_project <- terra::project(x, "EPSG:3857")
  ras_resample <- terra::resample(ras_project, ras_resolution, method = "mode")
  ras_grid <- stars::st_as_stars(ras_resample, dimensions = "XY")
  base::names(ras_grid) <- "lcz"

  # Calculate interp temporal resolution  ------------------------------------------------------

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

            if (LCZinterp == TRUE) {
              # [using ordinary kriging]
              krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, data_model, model = vg.model)
              krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = data_model)
            } else {
              krige_vgm <- automap::autofitVariogram(var_interp ~ 1, data_model, model = vg.model)
              krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = data_model)
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
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(interp_stack)
  }

  if (!is.null(by)) {
    if (by %in% "day") {
      stop("The 'day' does not work with the argument by")
    }

    if (length(by) < 2 && any(c("hour", "daylight", "month", "monthyear", "year", "season", "seasonyear", "yearseason") %in% by)) {

      stations_geometry <- dplyr::select(stations_mod, .data$my_id, .data$geometry) %>%
        dplyr::distinct(.data$my_id, .keep_all = TRUE) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = 3857)

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

        if (LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, data_model, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = data_model)

          } else {
          krige_vgm <- automap::autofitVariogram(var_interp ~ 1, data_model, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = data_model)
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
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }
      return(interp_stack)
    }

    if (length(by) > 1 & by %in% "daylight") {

      stations_geometry <- dplyr::select(stations_mod, .data$my_id, .data$geometry) %>%
        dplyr::distinct(.data$my_id, .keep_all = TRUE) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = 3857)

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
          dplyr::group_by(.data$my_id,.data$daylight, .data$my_time) %>%
          dplyr::summarise(var_interp = mean(.data$var_interp), .groups = "drop") %>%
          dplyr::ungroup() %>%
          dplyr::inner_join(stations_geometry, by = "my_id") %>%
          sf::st_as_sf()

        if (LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, data_model, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = data_model)
        } else {
          krige_vgm <- automap::autofitVariogram(var_interp ~ 1, data_model, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = data_model)
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
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      return(interp_stack)
    }
  }
}


