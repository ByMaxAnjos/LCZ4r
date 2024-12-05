#' Evaluate the LCZ - interpolated map
#'
#' This function evaluate the variability of a spatial and temporal LCZ-Kring related - interpolation. More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
#'
#' @param x A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()} function.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var Name of the variable for interpolation, e.g. air temperature, in the dataframe.
#' @param station_id Name of the station ID variable in the dataframe.
#' @param ... Additional arguments for the \code{selectByDate} function from the \code{openair} package. These arguments allow for flexible selection of specific time periods (year, month, day, hour). Examples of how to use these arguments include:
#' \itemize{
#'   \item \strong{Year(s)}: Numeric value(s) specifying the year(s) to select. For example, \code{year = 1998:2004} selects all years between 1998 and 2004 (inclusive), while \code{year = c(1998, 2004)} selects only the years 1998 and 2004.
#'   \item \strong{Month(s)}: Numeric or character value(s) specifying the months to select. Numeric examples: \code{month = 1:6} (January to June), or character examples: \code{month = c("January", "December")}.
#'   \item \strong{Day(s)}: Numeric value(s) specifying the days to select. For instance, \code{day = 1:30} selects days from 1 to 30, or \code{day = 15} selects only the 15th day of the month.
#'   \item \strong{Hour(s)}: Numeric value(s) specifying the hours to select. For example, \code{hour = 0:23} selects all hours in a day, while \code{hour = 9} selects only the 9th hour.
#'   \item \strong{Start date}: A string specifying the start date in either start="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#'   \item \strong{End date}: A string specifying the end date in either end="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#' }
#' @param split.ratio A numeric value representing the proportion of meteorological stations to be used for training (interpolation). The remaining stations will be used for testing (evaluation). For example, the default "0.8" indicates that 80% of the stations will be used for traninig and 20% for testing.
#' @param vg.model If kriging is selected, the list of viogrammodels that will be tested and interpolated with kriging. Default is "Sph". The model are "Sph", "Exp", "Gau", "Ste". They names respective shperical, exponential,gaussian,Matern familiy, Matern, M. Stein's parameterization.
#' @param sp.res Spatial resolution in unit of meters for interpolation. Default is 100.
#' @param tp.res Temporal resolution, the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{quater}, \dQuote{month}, \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{monthyear}, \dQuote{weekday}, or \dQuote{weekend}. It also include \dQuote{2 day}, \dQuote{2 week}, \dQuote{3 month} and so on.
#' @param by data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'           \dQuote{daylight}(daytime and nighttime).See argument \emph{type} in openair package:\url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param isave Save the plot into your directory.
#' @param LCZinterp If set to TRUE (default), the LCZ interpolation approach is used. If set to FALSE, conventional interpolation without LCZ is used.
#'
#' @return A summary table in CSV format containing evaluation metrics, including observed values, predicted values, and residuals.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Evaluate air temperature values
#'  my_interp <- lcz_interp_eval(lcz_map, data_frame = lcz_data, var = "airT",
#'                                station_id = "station", tp.res = "day", sp.res= 100,
#'                                year = 2019, month=9)
#'  }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_interp_eval <- function(x,
                            data_frame = "",
                            var = "",
                            station_id = "",
                            ...,
                            split.ratio = 0.8
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

  if (!("Latitude" %in% tolower(colnames(data_frame)) || "latitude" %in% tolower(colnames(data_frame)))) {
    stop("The 'latitude' input must be a column name in 'data_frame' representing each station's latitude.")
  }

  if (!("Longitude" %in% tolower(colnames(data_frame)) || "longitude" %in% tolower(colnames(data_frame)))) {
    stop("The 'longitude' input must be a column name in 'data_frame' representing each station's longitude")
  }

  if (!(vg.model %in% c("Sph", "Exp", "Gau", "Ste"))) {
    stop("Invalid viogram model. Choose from 'Sph', 'Exp', 'Gau', or 'Ste'.")
  }

  # Pre-processing time series ----------------------------------------------

  # Rename and define my_id for each lat and long
  df_variable <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      my_id = dplyr::cur_group_id(),
      my_id = base::as.factor(.data$my_id),
      var_interp = base::as.numeric(.data$var_interp),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    dplyr::ungroup()

  df_variable$latitude <- base::as.numeric(df_variable$latitude)
  df_variable$longitude <- base::as.numeric(df_variable$longitude)

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
    openair::timeAverage(avg.time = tp.res, type = c("station", "my_id"))

  df_processed <- dplyr::inner_join(df_period,
                                    df_variable %>% dplyr::select(-.data$station, -.data$var_interp),
                                    by = c("date", "my_id")
  ) %>%
    dplyr::ungroup()

  # Geospatial operations ---------------------------------------------------
  # Convert lcz_map to polygon
  lcz_shp <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)

  #Stratified splitting by LCZ)
  stations_mod <- df_processed %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  stations_lcz <- terra::extract(x, terra::vect(stations_mod))
  stations_lcz$ID <- NULL
  df_interp_mod <- base::cbind(stations_mod, stations_lcz) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 3857) %>%
    stats::na.omit()

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

            base::set.seed(123)
            split_data <- data_model %>%
              #dplyr::group_by(.data$lcz) %>%
              dplyr::mutate(is_train= dplyr::row_number() <= base::ceiling(split.ratio * dplyr::n())) %>%
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
            base::names(interp_map) <- "pred_values"
            # Evaluate interpolation
            eval_df <- terra::extract(interp_map, terra::vect(testing_set))
            eval_df$ID <- NULL
            eval_df <- dplyr::bind_cols(testing_set, eval_df)

            eval_result <- eval_df %>%
              dplyr::mutate(
                method = ifelse(LCZinterp, "LCZ-Ordinary kriging", "Conventional-Ordinary kriging"),
                residuals = .data$var_interp - .data$pred_values,
                spatial_res = sp.res,
                temporal_res = tp.res,
                viogrammodel = vg.model,
              ) %>%
              dplyr::rename(observed_values = .data$var_interp) %>%
              sf::st_drop_geometry() %>%
              dplyr::select(.data$date, .data$station, .data$lcz, .data$method, .data$spatial_res, .data$temporal_res, .data$viogrammodel, .data$observed_values, .data$pred_values, .data$residuals)

            return(eval_result)
          }

          MapHour <- base::apply(ihour, 1, model_hour)
          interp_hour <- base::do.call(rbind.data.frame, MapHour)
          return(interp_hour)
        }

        MapDay <- base::apply(iday, 1, model_day)
        interp_day <- base::do.call(rbind.data.frame, MapDay)
      }

      MapMonth <- base::apply(imonth, 1, model_month)
      interp_Month <- base::do.call(rbind.data.frame, MapMonth)
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

      # Save map as raster.tif
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval.csv")
      utils::write.csv(interp_day, file)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(interp_year)
  }

  if (!is.null(by)) {
    if (by %in% "day") {
      stop("The 'day' does not work with the argument by")
    }

    if (length(by) < 2 & by %in% c("daylight", "season", "seasonyear")) {
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
      mydata <- openair::cutData(df_processed,
                                 type = by, hemisphere = hemisphere,
                                 latitude = my_latitude, longitude = my_longitude
      ) %>%
        tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
                                     pollutant = "var_interp",
                                     avg.time = tp.res,
                                     type = c("station", "my_time")
      ) %>%
        tidyr::drop_na()
      iby <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {
        my_by <- iby[1]

        data_model <- mydata %>%
          dplyr::filter(.data$my_time == paste0(my_by))

        base::set.seed(123)
        split_data <- data_model %>%
          #dplyr::group_by(.data$lcz) %>%
          dplyr::mutate(is_train= dplyr::row_number() <= base::ceiling(split.ratio * dplyr::n())) %>%
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
        base::names(interp_map) <- "pred_values"
        # Evaluate interpolation
        eval_df <- terra::extract(interp_map, terra::vect(testing_set))
        eval_df$ID <- NULL
        eval_df <- dplyr::bind_cols(testing_set, eval_df)

        eval_result <- eval_df %>%
          dplyr::mutate(
            method = ifelse(LCZinterp, "LCZ-Ordinary kriging", "Conventional-Ordinary kriging"),
            residuals = .data$var_interp - .data$pred_values,
            spatial_res = sp.res,
            temporal_res = tp.res,
            viogrammodel = vg.model,
          ) %>%
          dplyr::rename(observed_values = .data$var_interp) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(.data$date, .data$station, .data$lcz, .data$method, .data$spatial_res, .data$temporal_res, .data$viogrammodel, .data$observed_values, .data$pred_values, .data$residuals)

        return(eval_result)
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

        # Save as table
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval.csv")
        utils::write.csv(interp_day, file)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }
      return(interp_by)
    }

    if (length(by) > 1 & by %in% "daylight") {
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
      mydata <- openair::cutData(df_processed,
                                 type = by, hemisphere = hemisphere,
                                 latitude = my_latitude, longitude = my_longitude
      ) %>%
        tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
                                     pollutant = "var_interp",
                                     avg.time = tp.res,
                                     type = c("station", "daylight", "my_time")
      ) %>% tidyr::drop_na()

      iby <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {
        my_by <- iby[1]

        data_model <- mydata %>%
          dplyr::filter(.data$my_time == paste0(my_by))

        base::set.seed(123)
        split_data <- data_model %>%
          #dplyr::group_by(.data$lcz) %>%
          dplyr::mutate(is_train= dplyr::row_number() <= base::ceiling(split.ratio * dplyr::n())) %>%
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
        base::names(interp_map) <- "pred_values"
        # Evaluate interpolation
        eval_df <- terra::extract(interp_map, terra::vect(testing_set))
        eval_df$ID <- NULL
        eval_df <- dplyr::bind_cols(testing_set, eval_df)

        eval_result <- eval_df %>%
          dplyr::mutate(
            method = ifelse(LCZinterp, "LCZ-Ordinary kriging", "Conventional-Ordinary kriging"),
            residuals = .data$var_interp - .data$pred_values,
            spatial_res = sp.res,
            temporal_res = tp.res,
            viogrammodel = vg.model,
          ) %>%
          dplyr::rename(observed_values = .data$var_interp) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(.data$date, .data$station, .data$lcz, .data$method, .data$spatial_res, .data$temporal_res, .data$viogrammodel, .data$observed_values, .data$pred_values, .data$residuals)

        return(eval_result)
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

        # Save as table
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval.csv")
        utils::write.csv(interp_day, file)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      return(interp_by)
    } else {
      mydata <-
        openair::cutData(df_processed, type = by) %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
                                     pollutant = "var_interp",
                                     avg.time = tp.res,
                                     type = c("station", "my_time")
      ) %>%
        tidyr::drop_na()
      iby <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {
        my_by <- iby[1]

        data_model <- mydata %>%
          dplyr::filter(.data$my_time == paste0(my_by))

        base::set.seed(123)
        split_data <- data_model %>%
          #dplyr::group_by(.data$lcz) %>%
          dplyr::mutate(is_train= dplyr::row_number() <= base::ceiling(split.ratio * dplyr::n())) %>%
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
        base::names(interp_map) <- "pred_values"
        # Evaluate interpolation
        eval_df <- terra::extract(interp_map, terra::vect(testing_set))
        eval_df$ID <- NULL
        eval_df <- dplyr::bind_cols(testing_set, eval_df)

        eval_result <- eval_df %>%
          dplyr::mutate(
            method = ifelse(LCZinterp, "LCZ-Ordinary kriging", "Conventional-Ordinary kriging"),
            residuals = .data$var_interp - .data$pred_values,
            spatial_res = sp.res,
            temporal_res = tp.res,
            viogrammodel = vg.model,
          ) %>%
          dplyr::rename(observed_values = .data$var_interp) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(.data$date, .data$station, .data$lcz, .data$method, .data$spatial_res, .data$temporal_res, .data$viogrammodel, .data$observed_values, .data$pred_values, .data$residuals)

        return(eval_result)
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

        # Save as table
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_eval.csv")
        utils::write.csv(interp_day, file)
        base::cat("The interp map rasters are saved into your pc. Look at 'LCZ4r_output' folder.\n")
      }

      return(interp_by)
    }
  }

}

