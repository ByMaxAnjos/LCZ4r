#' Evaluate the LCZ-Based Interpolated Map
#'
#' This function evaluates the variability of a spatial and temporal interpolation of a variable (e.g., air temperature) using LCZ as background. It supports both LCZ-based and conventional interpolation methods. The function allows for flexible time period selection, cross-validation, and station splitting for training and testing.
#'
#' @param x A `SpatRaster` object containing the LCZ map. The LCZ map can be obtained using the `lcz_get_map()` functions.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame must include a date field in hourly or higher resolution format.
#' @param var A character string specifying the name of the variable to interpolate (e.g., "airT" for air temperature).
#' @param station_id A character string specifying the name of the station ID variable in the data frame.
#' @param ... Additional arguments for the `selectByDate` function from the `openair` package. These arguments allow for flexible selection of specific time periods (year, month, day, hour). Examples include:
#'   \itemize{
#'     \item **Year(s)**: Numeric value(s) specifying the year(s) to select. For example, `year = 1998:2004` selects all years between 1998 and 2004 (inclusive), while `year = c(1998, 2004)` selects only the years 1998 and 2004.
#'     \item **Month(s)**: Numeric or character value(s) specifying the months to select. Numeric examples: `month = 1:6` (January to June), or character examples: `month = c("January", "December")`.
#'     \item **Day(s)**: Numeric value(s) specifying the days to select. For instance, `day = 1:30` selects days from 1 to 30, or `day = 15` selects only the 15th day of the month.
#'     \item **Hour(s)**: Numeric value(s) specifying the hours to select. For example, `hour = 0:23` selects all hours in a day, while `hour = 9` selects only the 9th hour.
#'     \item **Start date**: A string specifying the start date in either `start="DD/MM/YYYY"` (e.g., "1/2/1999") or `"YYYY-mm-dd"` format (e.g., "1999-02-01").
#'     \item **End date**: A string specifying the end date in either `end="DD/MM/YYYY"` (e.g., "1/2/1999") or `"YYYY-mm-dd"` format (e.g., "1999-02-01").
#'   }
#' @param extract.method A character string specifying the method used to assign the LCZ class to each station point. The default is `"simple"`. Available methods are:
#'   \itemize{
#'     \item **simple**: Assigns the LCZ class based on the value of the raster cell in which the point falls. It often is used in low-density observational network.
#'     \item **two.step**: Assigns LCZs to stations while filtering out those located in heterogeneous LCZ areas. This method requires that at least 80% of the pixels within a 5 × 5 kernel match the LCZ of the center pixel (Daniel et al., 2017). Note that this method reduces the number of stations. It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
#'     \item **bilinear**: Interpolates the LCZ class values from the four nearest raster cells surrounding the point.
#'   }
#' @param LOOCV A logical value. If `TRUE` (default), leave-one-out cross-validation (LOOCV) is used for kriging. If `FALSE`, the split method into training and testing stations is used.
#' @param split.ratio A numeric value representing the proportion of meteorological stations to be used for training (interpolation). The remaining stations will be used for testing (evaluation). For example, the default `0.8` indicates that 80% of the stations will be used for training and 20% for testing.
#' @param vg.model A character string specifying the variogram model for kriging. The default is `"Sph"`. Available models are:
#'   \itemize{
#'     \item **Sph**: Spherical model.
#'     \item **Exp**: Exponential model.
#'     \item **Gau**: Gaussian model.
#'     \item **Ste**: M. Stein's parameterization.
#'   }
#' @param sp.res A numeric value specifying the spatial resolution in meters for interpolation. The default is `100`.
#' @param tp.res A character string specifying the temporal resolution for averaging. The default is `"hour"`. Other options include `"day"`, `"week"`, `"month"`, `"year"`, `"season"`, `"seasonyear"`, `"monthyear"`, `"weekday"`, `"weekend"`, or custom intervals like `"2 day"`, `"2 week"`, `"3 month"`, etc.
#' @param by A character string specifying how to split the time series in the data frame. Options include `"year"`, `"season"`, `"seasonyear"`, `"month"`, `"monthyear"`, `"weekday"`, `"weekend"`, `"site"`, or `"daylight"` (daytime and nighttime). See the `type` argument in the `openair` package for more details: \url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}.
#' @param Anomaly If `TRUE`,the anomalies are calculated. If `FALSE` (default) the raw air temperatures are used.
#' @param impute A character string specifying the method to impute missing values. Options include `"mean"`, `"median"`, `"knn"`, or `"bag"`.
#' @param isave A logical value. If `TRUE`, the plot is saved to the working directory.
#' @param LCZinterp A logical value. If `TRUE` (default), the LCZ interpolation approach is used. If `FALSE`, conventional interpolation without LCZ is used.
#'
#' @return A summary table in CSV format containing time series of observed values, predicted values, and residuals. It also returns an ESRI shapefile (.gpkg) containing metadata of the interpolation.
#'
#' @author
#' Max Anjos (\url{https://github.com/ByMaxAnjos})
#'
#' @references
#' Anjos, M., Targino, A. C., Krecl, P., Oukawa, G. Y. & Braga, R. F. Analysis of the urban heat island under different synoptic patterns using local climate zones. Build. Environ. 185, 107268 (2020).
#' Fenner, D., Meier, F., Bechtel, B., Otto, M. & Scherer, D. Intra and inter ‘local climate zone’ variability of air temperature as observed by crowdsourced citizen weather stations in Berlin, Germany. Meteorol. Z. 26, 525–547 (2017).
#' \url{http://www.gstat.org/}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Evaluate air temperature interpolation values using Berlin data and LOOCV
#' data("lcz_data")
#' lcz_map <- lcz_get_map_generator(ID = "8576bde60bfe774e335190f2e8fdd125dd9f4299")
#' lcz_plot_map(lcz_map)
#' my_interp <- lcz_interp_eval(
#'   lcz_map, data_frame = lcz_data, var = "airT",
#'   station_id = "station", year = 2019, month = 9, day = 5,
#'   sp.res = 100, tp.res = "hour", LOOCV = TRUE,
#'   vg.model = "Sph", LCZinterp = TRUE, isave = TRUE
#' )
#'
#' # Evaluate LCZ-based interpolation using split station (80% training, 20% testing)
#' my_interp <- lcz_interp_eval(
#'   lcz_map, data_frame = lcz_data, var = "airT",
#'   station_id = "station", tp.res = "hour", sp.res = 100,
#'   year = 2019, month = 9, day = 5, LOOCV = FALSE,
#'   split.ratio = 0.8, vg.model = "Sph", LCZinterp = TRUE, isave = TRUE
#' )
#' }
#'
#' @importFrom rlang .data
#' @keywords Local Climate Zone, interpolation, kriging

lcz_interp_eval <- function(x,
                            data_frame = "",
                            var = "",
                            station_id = "",
                            ...,
                            extract.method = "simple",
                            LOOCV = TRUE,
                            split.ratio = 0.8,
                            sp.res = 100,
                            tp.res = "hour",
                            vg.model = "Sph",
                            by = NULL,
                            Anomaly = FALSE,
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
      var_interp = base::as.numeric(.data$var_interp),
      date = lubridate::as_datetime(.data$date),
      station= base::as.factor(.data$station)
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
    openair::timeAverage(avg.time = tp.res, type = c("station", "my_id"))%>%
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

  # Get id stations
  my_stations <- df_processed %>%
    dplyr::distinct(.data$station, .keep_all = F)

  #Stratified splitting by LCZ)
  stations_mod <- df_processed %>%
    dplyr::distinct(.data$longitude, .data$latitude, .keep_all = T) %>%
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

  if (isave == TRUE) {

    # Create a folder name using paste0
    folder <- base::paste0("LCZ4r_output/")

    # Check if the folder exists
    if (!base::dir.exists(folder)) {
      # Create the folder if it does not exist
      base::dir.create(folder)
    }

    if(LOOCV==FALSE) {
      #Export the metadata
      base::set.seed(123)
      split_data <- df_interp_mod %>%
        dplyr::distinct(.data$my_id, .data$lcz, .keep_all = T) %>%
        dplyr::group_by(.data$lcz) %>%
        dplyr::mutate(is_train = stats::runif(dplyr::n()) <= split.ratio) %>%
        dplyr::ungroup()
      training_set <- dplyr::filter(split_data, .data$is_train)
      testing_set <- dplyr::filter(split_data, !.data$is_train)

      eval_result_meta <- split_data %>%
        stats::na.omit() %>%
        dplyr::mutate(
          method_interp = ifelse(LCZinterp, "LCZ-Universal kriging", "Conventional-Ordinary kriging"),
          method_eval = ifelse(LOOCV, "LOOCV", "Split stations"),
          split = ifelse(.data$is_train, "training", "testing"),
          spatial_res = sp.res,
          temporal_res = tp.res,
          viogrammodel = vg.model,
        ) %>%
        dplyr::select(.data$my_id, .data$method_interp, .data$method_eval, .data$spatial_res, .data$temporal_res, .data$viogrammodel,.data$geometry) %>%
        sf::st_as_sf()
    } else {
      eval_result_meta <- df_interp_mod %>%
        stats::na.omit() %>%
        dplyr::distinct(.data$my_id, .keep_all = T) %>%
        dplyr::mutate(
          method_interp = ifelse(LCZinterp, "LCZ-Universal kriging", "Conventional-Ordinary kriging"),
          method_eval = ifelse(LOOCV, "LOOCV", "Split stations"),
          spatial_res = sp.res,
          temporal_res = tp.res,
          viogrammodel = vg.model,
        ) %>%
        dplyr::rename(observed = .data$var_interp) %>%
        dplyr::select(.data$my_id, .data$method_interp, .data$method_eval, .data$spatial_res, .data$temporal_res, .data$viogrammodel,.data$geometry) %>%
        sf::st_as_sf()
    }
    #Save as .shp
    file1 <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval_meta.gpkg")
    sf::st_write(eval_result_meta, file1, append = FALSE, quiet = TRUE)

    base::message("Looking at your metadata in the path:", base::paste0(getwd(), "/", folder))

  }

  # Calculate interp temporal resolution  ------------------------------------------------------
  if (is.null(by)) {
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

          modelday <- modelmonth %>%
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

            if(Anomaly == TRUE){
              anomlay_lcz <- function(input = NULL) {
                mean_df <- data_model %>%
                  dplyr::filter(.data$station == paste0(input)) %>%
                  dplyr::group_by(.data$my_id, .data$lcz) %>%
                  dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

                reference_df <- data_model %>%
                  dplyr::filter(.data$station != paste0(input)) %>%
                  dplyr::mutate(reference_value = mean(.data$var_interp))
                reference_df <- tibble::as.tibble(mean(reference_df$reference_value))

                merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
                  dplyr::rename(reference_value = .data$value) %>%
                  dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

                return(merged_data)
              }
              anomaly_job <- base::lapply(1:base::length(my_stations$station), FUN = function(i) {
                anomlay_lcz(input = my_stations$station[i])
              })

              anomaly_cal <- base::do.call(rbind.data.frame, anomaly_job)

              lcz_anomaly_mod <- sf::st_as_sf(anomaly_cal) %>% sf::st_transform(crs = 3857)

              if(LOOCV==FALSE) {
              base::set.seed(123)
              split_data <- lcz_anomaly_mod %>%
                dplyr::group_by(.data$lcz) %>%
                dplyr::mutate(is_train = stats::runif(dplyr::n()) <= split.ratio) %>%
                dplyr::ungroup()
              training_set <- dplyr::filter(split_data, .data$is_train)
              testing_set <- dplyr::filter(split_data, !.data$is_train)

              if (LCZinterp == TRUE) {
                krige_vgm <- automap::autofitVariogram(anomaly ~ lcz, training_set, model = vg.model)
                krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = training_set)
              } else {
                krige_vgm <- automap::autofitVariogram(anomaly ~ 1, training_set, model = vg.model)
                krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = training_set)
              }

              # Predict using kriging and optimize raster processing
              krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
              interp_map <- terra::rast(krige_map["var1.pred", , ])
              interp_map <- terra::focal(interp_map, w = 7, fun = mean)
              base::names(interp_map) <- "predicted"

              # Evaluate interpolation
              eval_df <- terra::extract(interp_map, terra::vect(testing_set))
              eval_df$ID <- NULL
              eval_df <- dplyr::bind_cols(testing_set, eval_df)
              eval_result <- eval_df %>%
                stats::na.omit() %>%
                dplyr::mutate(
                  lcz = base::as.factor(.data$lcz),
                  residual = .data$var_interp - .data$predicted) %>%
                dplyr::rename(observed = .data$var_interp) %>%
                dplyr::inner_join(data_model %>% dplyr::select(date, my_id, station) %>%
                                    sf::st_drop_geometry(), by = "my_id"
                ) %>%
                dplyr::select(.data$date, .data$station, .data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual)

              return(eval_result)

            } else {

              if (LCZinterp == TRUE) {
                krige_vgm <- automap::autofitVariogram(anomaly ~ lcz, lcz_anomaly_mod, model = vg.model)
                krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = lcz_anomaly_mod)
              } else {
                krige_vgm <- automap::autofitVariogram(anomaly ~ 1, lcz_anomaly_mod, model = vg.model)
                krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = lcz_anomaly_mod)
              }
              set.seed(123)
              lcz_cv <- gstat::gstat.cv(krige_mod, nfold = 5, debug.level = 0)
              lcz_cv <- sf::st_as_sf(lcz_cv)
              lcz_anomaly_mod$geometry <- NULL
              lcz_cv_result <- dplyr::bind_cols(lcz_anomaly_mod, lcz_cv) %>%
                stats::na.omit() %>%
                dplyr::mutate(
                  predicted = .data$var1.pred) %>%
                dplyr::inner_join(data_model %>% dplyr::select(date, my_id, station) %>%
                                  sf::st_drop_geometry(), by = "my_id"
                                  ) %>%
                dplyr::select(.data$date, .data$station, .data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual, .data$zscore)

              return(lcz_cv_result)
            }

            } else {

              if(LOOCV==FALSE) {
                base::set.seed(123)
                split_data <- data_model %>%
                  dplyr::group_by(.data$lcz) %>%
                  dplyr::mutate(is_train = stats::runif(dplyr::n()) <= split.ratio) %>%
                  dplyr::ungroup()
                training_set <- dplyr::filter(split_data, .data$is_train)
                testing_set <- dplyr::filter(split_data, !.data$is_train)

                if (LCZinterp == TRUE) {
                  krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, training_set, model = vg.model)
                  krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = training_set)
                } else {
                  krige_vgm <- automap::autofitVariogram(var_interp ~ 1, training_set, model = vg.model)
                  krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = training_set)
                }

                # Predict using kriging and optimize raster processing
                krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
                interp_map <- terra::rast(krige_map["var1.pred", , ])
                interp_map <- terra::focal(interp_map, w = 7, fun = mean)
                base::names(interp_map) <- "predicted"

                # Evaluate interpolation
                eval_df <- terra::extract(interp_map, terra::vect(testing_set))
                eval_df$ID <- NULL
                eval_df <- dplyr::bind_cols(testing_set, eval_df)
                eval_result <- eval_df %>%
                  stats::na.omit() %>%
                  dplyr::mutate(
                    lcz = base::as.factor(.data$lcz),
                    residual = .data$var_interp - .data$predicted) %>%
                  dplyr::rename(observed = .data$var_interp) %>%
                  dplyr::select(.data$date, .data$station, .data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual)

                return(eval_result)

              } else {

                if (LCZinterp == TRUE) {
                  krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, data_model, model = vg.model)
                  krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = data_model)
                } else {
                  krige_vgm <- automap::autofitVariogram(var_interp ~ 1, data_model, model = vg.model)
                  krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = data_model)
                }
                set.seed(123)
                lcz_cv <- gstat::gstat.cv(krige_mod, nfold = 5, debug.level = 0)
                lcz_cv <- sf::st_as_sf(lcz_cv)
                data_model$geometry <- NULL
                lcz_cv_result <- dplyr::bind_cols(data_model, lcz_cv) %>%
                  stats::na.omit() %>%
                  dplyr::mutate(
                    predicted = .data$var1.pred) %>%
                  dplyr::select(.data$date, .data$station,.data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual, .data$zscore)

                return(lcz_cv_result)
              }

            }


          }

          MapHour <- base::apply(ihour, 1, model_hour)
          interp_hour <- base::do.call(rbind.data.frame, MapHour)
          return(interp_hour)
        }

        MapDay <- base::apply(iday, 1, model_day)
        interp_day <- base::do.call(rbind.data.frame, MapDay)
        return(interp_day)
      }

      MapMonth <- base::apply(imonth, 1, model_month)
      interp_Month <- base::do.call(rbind.data.frame, MapMonth)
      return(interp_Month)
    }

    MapYear <- base::apply(iyear, 1, model_year)
    interp_year <- base::do.call(rbind.data.frame, MapYear)

    if (isave == TRUE) {

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }
      # Save as .csv
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval_result.csv")
      utils::write.csv(interp_year, file)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(interp_year)
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


        if(Anomaly == TRUE){
          anomlay_lcz <- function(input = NULL) {
            mean_df <- data_model %>%
              dplyr::filter(.data$station == paste0(input)) %>%
              dplyr::group_by(.data$my_id, .data$lcz) %>%
              dplyr::summarise(mean_value = mean(.data$var_interp), .groups = "drop")

            reference_df <- data_model %>%
              dplyr::filter(.data$station != paste0(input)) %>%
              dplyr::mutate(reference_value = mean(.data$var_interp))
            reference_df <- tibble::as.tibble(mean(reference_df$reference_value))

            merged_data <- dplyr::bind_cols(mean_df, reference_df) %>%
              dplyr::rename(reference_value = .data$value) %>%
              dplyr::mutate(anomaly = .data$mean_value - .data$reference_value)

            return(merged_data)
          }
          anomaly_job <- base::lapply(1:base::length(my_stations$station), FUN = function(i) {
            anomlay_lcz(input = my_stations$station[i])
          })

          anomaly_cal <- base::do.call(rbind.data.frame, anomaly_job)

          lcz_anomaly_mod <- sf::st_as_sf(anomaly_cal) %>% sf::st_transform(crs = 3857)

          if(LOOCV==FALSE) {
            base::set.seed(123)
            split_data <- lcz_anomaly_mod %>%
              dplyr::group_by(.data$my_time, .data$lcz) %>%
              dplyr::mutate(is_train = stats::runif(dplyr::n()) <= split.ratio) %>%
              dplyr::ungroup()
            training_set <- dplyr::filter(split_data, .data$is_train)
            testing_set <- dplyr::filter(split_data, !.data$is_train)

            if (LCZinterp == TRUE) {
              krige_vgm <- automap::autofitVariogram(anomaly ~ lcz, training_set, model = vg.model)
              krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = training_set)
            } else {
              krige_vgm <- automap::autofitVariogram(anomaly ~ 1, training_set, model = vg.model)
              krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = training_set)
            }

            # Predict using kriging and optimize raster processing
            krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
            interp_map <- terra::rast(krige_map["var1.pred", , ])
            interp_map <- terra::focal(interp_map, w = 7, fun = mean)
            base::names(interp_map) <- "predicted"

            # Evaluate interpolation
            eval_df <- terra::extract(interp_map, terra::vect(testing_set))
            eval_df$ID <- NULL
            eval_df <- dplyr::bind_cols(testing_set, eval_df)
            eval_result <- eval_df %>%
              stats::na.omit() %>%
              dplyr::mutate(
                lcz = base::as.factor(.data$lcz),
                residual = .data$var_interp - .data$predicted) %>%
              dplyr::rename(observed = .data$var_interp,
                            select_time = .data$my_time) %>%
              dplyr::select(.data$date, .data$station, .data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual)

            return(eval_result)

          } else {

            if (LCZinterp == TRUE) {
              krige_vgm <- automap::autofitVariogram(anomaly ~ lcz, lcz_anomaly_mod, model = vg.model)
              krige_mod <- gstat::gstat(formula = anomaly ~ lcz, model = krige_vgm$var_model, data = lcz_anomaly_mod)
            } else {
              krige_vgm <- automap::autofitVariogram(anomaly ~ 1, lcz_anomaly_mod, model = vg.model)
              krige_mod <- gstat::gstat(formula = anomaly ~ 1, model = krige_vgm$var_model, data = lcz_anomaly_mod)
            }
            set.seed(123)
            lcz_cv <- gstat::gstat.cv(krige_mod, nfold = 5, debug.level = 0)
            lcz_cv <- sf::st_as_sf(lcz_cv)
            lcz_anomaly_mod$geometry <- NULL
            lcz_cv_result <- dplyr::bind_cols(lcz_anomaly_mod, lcz_cv) %>%
              stats::na.omit() %>%
              dplyr::mutate(
                predicted = .data$var1.pred,
                select_time = .data$my_time) %>%
              dplyr::select(.data$date, .data$station,.data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual, .data$zscore)

            return(lcz_cv_result)
          }

        } else {

          if(LOOCV==FALSE) {
            base::set.seed(123)
            split_data <- data_model %>%
              dplyr::group_by(.data$my_time, .data$lcz) %>%
              dplyr::mutate(is_train = stats::runif(dplyr::n()) <= split.ratio) %>%
              dplyr::ungroup()
            training_set <- dplyr::filter(split_data, .data$is_train)
            testing_set <- dplyr::filter(split_data, !.data$is_train)

            if (LCZinterp == TRUE) {
              krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, training_set, model = vg.model)
              krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = training_set)
            } else {
              krige_vgm <- automap::autofitVariogram(var_interp ~ 1, training_set, model = vg.model)
              krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = training_set)
            }

            # Predict using kriging and optimize raster processing
            krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
            interp_map <- terra::rast(krige_map["var1.pred", , ])
            interp_map <- terra::focal(interp_map, w = 7, fun = mean)
            base::names(interp_map) <- "predicted"

            # Evaluate interpolation
            eval_df <- terra::extract(interp_map, terra::vect(testing_set))
            eval_df$ID <- NULL
            eval_df <- dplyr::bind_cols(testing_set, eval_df)
            eval_df$geometry <- NULL
            eval_result <- eval_df %>%
              stats::na.omit() %>%
              dplyr::mutate(
                residual = .data$var_interp - .data$predicted) %>%
              dplyr::rename(observed = .data$var_interp,
                            select_time = .data$my_time) %>%
              dplyr::select(.data$station, .data$my_id, .data$lcz, .data$select_time, .data$observed, .data$predicted, .data$residual)

            return(eval_result)

          } else {

            if (LCZinterp == TRUE) {
              krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, data_model, model = vg.model)
              krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = data_model)
            } else {
              krige_vgm <- automap::autofitVariogram(var_interp ~ 1, data_model, model = vg.model)
              krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = data_model)
            }
            set.seed(123)  # Ensure reproducibility
            lcz_cv <- gstat::gstat.cv(krige_mod, nfold = 5, debug.level = 0)
            lcz_cv <- sf::st_as_sf(lcz_cv)
            data_model$geometry <- NULL
            lcz_cv_result <- dplyr::bind_cols(data_model, lcz_cv) %>%
              stats::na.omit() %>%
              dplyr::mutate(
                predicted = .data$var1.pred,
                select_time = .data$my_time) %>%
              dplyr::select(.data$station, .data$my_id, .data$lcz, .data$select_time,.data$observed, .data$predicted, .data$residual, .data$zscore)

            return(lcz_cv_result)
          }
        }

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::do.call(rbind.data.frame, mapBy)

      if (isave == TRUE) {

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        # Save as .csv
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval_result.csv")
        utils::write.csv(interp_by, file)

        base::message("Looking at your results in the path:", base::paste0(getwd(), "/", folder))
      }
      return(interp_by)
    }

    if (length(by) > 1 & by %in% "daylight") {

      stations_geometry <- dplyr::select(stations_mod, .data$station, .data$my_id, .data$geometry) %>%
        dplyr::distinct(.data$station, .data$my_id, .keep_all = TRUE)  %>%
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
        dplyr::distinct(.data$daylight,.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {

        my_by <- iby$daylight[1]
        my_by2 <- iby$my_time[1]

        data_model <- mydata %>%
          dplyr::filter(.data$daylight == paste0(my_by)) %>%
          dplyr::filter(.data$my_time == paste0(my_by2)) %>%
          dplyr::group_by(.data$station, .data$daylight, .data$my_time) %>%
          dplyr::summarise(var_interp = mean(.data$var_interp), .groups = "drop") %>%
          dplyr::ungroup() %>%
          dplyr::inner_join(stations_geometry, by = "station") %>%
          sf::st_as_sf()


        if(LOOCV==FALSE) {

          base::set.seed(123)
          split_data <- data_model %>%
            dplyr::group_by(.data$lcz) %>%
            dplyr::mutate(is_train = stats::runif(dplyr::n()) <= split.ratio) %>%
            dplyr::ungroup()
          training_set <- dplyr::filter(split_data, .data$is_train)
          testing_set <- dplyr::filter(split_data, !.data$is_train)

          if (LCZinterp == TRUE) {
            krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, training_set, model = vg.model)
            krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = training_set)
          } else {
            krige_vgm <- automap::autofitVariogram(var_interp ~ 1, training_set, model = vg.model)
            krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = training_set)
          }

          # Predict using kriging and optimize raster processing
          krige_map <- terra::predict(krige_mod, newdata = ras_grid, debug.level = 0)
          interp_map <- terra::rast(krige_map["var1.pred", , ])
          interp_map <- terra::focal(interp_map, w = 7, fun = mean)
          base::names(interp_map) <- "predicted"

          # Evaluate interpolation
          eval_df <- terra::extract(interp_map, terra::vect(testing_set))
          eval_df$ID <- NULL
          eval_df <- dplyr::bind_cols(testing_set, eval_df)
          eval_result <- eval_df %>%
            stats::na.omit() %>%
            dplyr::mutate(residual = .data$var_interp - .data$predicted) %>%
            dplyr::rename(observed = .data$var_interp,
                          select_time = .data$my_time) %>%
            dplyr::select(.data$station, .data$my_id, .data$lcz, .data$daylight, .data$select_time, .data$observed, .data$predicted, .data$residual)


          return(eval_result)

        } else {

          if (LCZinterp == TRUE) {
            krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, data_model, model = vg.model)
            krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = data_model)
          } else {
            krige_vgm <- automap::autofitVariogram(var_interp ~ 1, data_model, model = vg.model)
            krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = data_model)
          }

          lcz_cv <- gstat::gstat.cv(krige_mod, nfold = 5, debug.level = 0)
          lcz_cv <- sf::st_as_sf(lcz_cv)
          data_model$geometry <- NULL
          lcz_cv_result <- dplyr::bind_cols(data_model, lcz_cv) %>%
            stats::na.omit() %>%
            dplyr::mutate(predicted = .data$var1.pred) %>%
            dplyr::select(.data$station, .data$my_id, .data$lcz, .data$observed, .data$predicted, .data$residual, .data$zscore)

          return(lcz_cv_result)
        }

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::do.call(rbind.data.frame, mapBy)

      if (isave == TRUE) {

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        # Save as .csv
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval_result.csv")
        utils::write.csv(interp_by, file)

        base::message("Looking at your results in the path:", base::paste0(getwd(), "/", folder))
      }

      return(interp_by)
      }
  }

}
