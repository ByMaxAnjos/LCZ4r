#' Visualize the LCZ - interpolated map
#'
#' This function generates a spatial interpolation of air temperature (or other variable) using Local Climate Zones (LCZs) and Kriging.  More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
#'
#' @param x A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()} function.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var Name of the variable for interpolation, e.g. air temperature, in the dataframe.
#' @param station_id Name of the station ID variable in the dataframe.
#' @param ... Additional arguments for the \code{selectBydata} from \code{openair} package. These include:
#' \itemize{
#'   \item A start date string in the form "1/2/1999" or in format "YYYY-mm-dd", e.g., "1999-02-01".
#'   \item A year or years to select, e.g., year = 1998:2004 to select 1998-2004 inclusive, or year = c(1998, 2004) to select 1998 and 2004.
#'   \item A month or months to select. Can either be numeric, e.g., month = 1:6 to select January to June, or by name, e.g., month = c("January", "December").
#' }
#' @param vg.model If kriging is selected, the list of viogrammodels that will be tested and interpolated with kriging. Default is "Sph". The model are "Sph", "Exp", "Gau", "Ste". They names respective shperical, exponential,gaussian,Matern familiy, Matern, M. Stein's parameterization.
#' @param sp.res Spatial resolution in unit of meters for interpolation. Default is 100.
#' @param tp.res Temporal resolution, the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.
#' @param by data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'           \dQuote{daylight}(daytime and nighttime).See argument \emph{type} in openair package:\url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param isave Save the plot into your directory.
#' @param LCZinterp If set to TRUE (default), the LCZ interpolation approach is used. If set to FALSE, conventional interpolation without LCZ is used.
#'
#' @return A map of LCZ-air temperature in \code{terra raster GeoTIF} format
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Daily air temperature values in September 2019 .
#'  my_interp <- lcz_interp_map(lcz_map, data_frame = lcz_data, var = "airT",
#'                                station_id = "station", tp.res = "day", sp.res= 100,
#'                                year = 2019, month=9)
#'  }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_interp_map <- function(x,
                            data_frame = "",
                            var = "",
                            station_id = "",
                            ...,
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

  #Rename and define my_id for each lat and long
  df_variable <- data_frame %>%
    dplyr::rename(var_interp = {{var}}, station = {{station_id}}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(my_id = dplyr::cur_group_id(),
                  my_id = base::as.factor(.data$my_id),
                  var_interp = base::as.numeric(.data$var_interp),
                  date = lubridate::as_datetime(.data$date)) %>%
    dplyr::ungroup()


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

  #Define the period
  df_period <- df_variable %>%
    dplyr::select(.data$date, .data$station, .data$my_id, .data$var_interp) %>%
    openair::selectByDate(...) %>%
    openair::timeAverage(avg.time = tp.res, type = c("station", "my_id"))

  df_processed <- dplyr::inner_join(df_period,
                                    df_variable %>% dplyr::select(-.data$station,-.data$var_interp),
                                    by = c("date", "my_id"))

  # Geospatial operations ---------------------------------------------------
  #Convert lcz_map to polygon
  lcz_shp <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)

  #Re-project and make a grid to interpolation
  lcz_box <- sf::st_transform(lcz_shp, crs = 3857)

  ras_resolution <- sf::st_bbox(lcz_box) %>%
    stars::st_as_stars(dx = sp.res)
  ras_resolution <- terra::rast(ras_resolution)

  ras_project <- terra::project(x, "EPSG:3857")
  ras_resample <- terra::resample(ras_project, ras_resolution, method = "mode")
  ras_grid <- stars::st_as_stars(ras_resample, dimensions = "XY")
  base::names(ras_grid) <- "lcz"


  # Calculate interp temporal resolution  ------------------------------------------------------
  if (is.null(by) & tp.res %in% c("hour", "day")) {  #Downscale to hour or day
    iday <- df_processed %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(.data$date) %>% dplyr::ungroup() %>%
      dplyr::mutate(iday = lubridate::day(.data$date)) %>%
      dplyr::distinct(.data$iday, .keep_all = FALSE) %>%
      base::expand.grid()

    model_day <- function(iday) {

      myday <- iday[1]

      modelday <- df_processed %>%
        dplyr::mutate(day = lubridate::day(.data$date)) %>%
        dplyr::filter(.data$day == paste0(myday))

      #Downscale to hour
      ihour <- modelday %>%
        dplyr::group_by(.data$date) %>%
        dplyr::select(.data$date) %>% dplyr::ungroup() %>%
        dplyr::mutate(ihour = lubridate::hour(date)) %>%
        dplyr::distinct(.data$ihour, .keep_all = FALSE) %>%
        base::expand.grid()

      model_hour <- function(ihour) {

        myhour <- ihour[1]

        data_model <- modelday %>%
          dplyr::mutate(hour = lubridate::hour(.data$date),
                        my_id = base::as.factor(.data$my_id)) %>%
          dplyr::filter(.data$hour == paste0(myhour))

        #merge data-model with lcz_station to get lcz class
        df_interp_mod <-
          sf::st_as_sf(data_model,
                       coords = c("longitude", "latitude"), crs = 4326) %>%
          sf::st_intersection(lcz_shp) %>%
          sf::st_transform(crs = 3857)

        if (LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name

          } else {
            krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
            krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
            krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
            krige_map <- krige_map["var1.pred",,]
            interp_map <- terra::rast(krige_map)
            interp_map <- terra::focal(interp_map, w=7, fun=mean)
            mydate <- data_model %>% dplyr::pull(.data$date)
            mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
            ras_name <- base::paste0("krige_", mydate)
            base::names(interp_map) <- ras_name
          }

          return(interp_map)

      }

      MapHour <- base::apply(ihour, 1, model_hour)
      interp_hour <- base::unlist(MapHour)
      return(interp_hour)

    }

    MapDay <- base::apply(iday, 1, model_day)
    interp_day <- base::unlist(MapDay)
    interp_stack <- terra::rast(interp_day)

    if (isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      #Save map as raster.tif
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    return(interp_stack)
    }

  if (is.null(by) & tp.res %in% c("week")) {  #Downscale to week

    iweek <- df_processed %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(.data$date) %>% dplyr::ungroup() %>%
      dplyr::mutate(iweek = lubridate::week(.data$date)) %>%
      dplyr::distinct(.data$iweek, .keep_all = FALSE) %>%
      base::expand.grid()

    model_week <- function(iweek) {

      my_week <- iweek[1]

      data_model <- df_processed %>%
        dplyr::mutate(iweek = lubridate::week(.data$date),
                      my_id = base::as.factor(.data$my_id)) %>%
        dplyr::filter(.data$iweek == paste0(my_week))

      #merge data-model with lcz_station to get lcz class
      df_interp_mod <-
        sf::st_as_sf(data_model,
                     coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = 3857)

      if (LCZinterp == TRUE) {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      } else {
        krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name
      }

      return(interp_map)

    }

    mapWeek <- base::apply(iweek, 1, model_week)
    interp_week <- base::unlist(mapWeek)
    interp_stack <- terra::rast(interp_week)

    if(isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      #Save map as raster.tif
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    return(interp_stack)

  }

  if (is.null(by) & tp.res %in% c("month")) {  #Downscale to month

    imonth <- df_processed %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(.data$date) %>% dplyr::ungroup() %>%
      dplyr::mutate(imonth = lubridate::month(.data$date)) %>%
      dplyr::distinct(.data$imonth, .keep_all = FALSE) %>%
      base::expand.grid()

    model_month <- function(imonth) {

      my_month <- imonth[1]

      data_model <- df_processed %>%
        dplyr::mutate(imonth = lubridate::month(.data$date),
                      my_id = base::as.factor(.data$my_id)) %>%
        dplyr::filter(.data$imonth == paste0(my_month))

      #merge data-model with lcz_station to get lcz class
      df_interp_mod <-
        sf::st_as_sf(data_model,
                     coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = 3857)

      if (LCZinterp == TRUE) {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      } else {
        krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name
      }

      return(interp_map)

    }

    mapMonth <- base::apply(imonth, 1, model_month)
    interp_month <- base::unlist(mapMonth)
    interp_stack <- terra::rast(interp_month)

    if(isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      #Save map as raster.tif
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    return(interp_stack)

  }

  if(is.null(by) & tp.res %in% c("quarter")) {  #Downscale to quarter

    iquarter <- df_processed %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(.data$date) %>% dplyr::ungroup() %>%
      dplyr::mutate(iquarter = lubridate::quarter(.data$date)) %>%
      dplyr::distinct(.data$iquarter, .keep_all = FALSE) %>%
      base::expand.grid()

    model_quarter <- function(iquarter) {

      my_quarter <- iquarter[1]

      data_model <- df_processed %>%
        dplyr::mutate(iquarter = lubridate::quarter(.data$date),
                      my_id = base::as.factor(.data$my_id)) %>%
        dplyr::filter(.data$iquarter == paste0(my_quarter))

      #merge data-model with lcz_station to get lcz class
      df_interp_mod <-
        sf::st_as_sf(data_model,
                     coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = 3857)

      if (LCZinterp == TRUE) {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      } else {
        krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name
      }

      return(interp_map)

    }

    mapQuarter <- base::apply(iquarter, 1, model_quarter)
    interp_quarter <- base::unlist(mapQuarter)
    interp_stack <- terra::rast(interp_quarter)

    if (isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      #Save map as raster.tif
      file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    return(interp_stack)

  }

  if (is.null(by) & tp.res %in% c("year")) {  #Downscale to year

    iyear <- df_processed %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(.data$date) %>% dplyr::ungroup() %>%
      dplyr::mutate(iyear = lubridate::year(.data$date)) %>%
      dplyr::distinct(.data$iyear, .keep_all = FALSE) %>%
      base::expand.grid()

    model_year <- function(iyear) {

      my_year <- iyear[1]

      data_model <- df_processed %>%
        dplyr::mutate(iyear = lubridate::year(.data$date),
                      my_id = base::as.factor(.data$my_id)) %>%
        dplyr::filter(.data$iyear == paste0(my_year))

      #merge data-model with lcz_station to get lcz class
      df_interp_mod <-
        sf::st_as_sf(data_model,
                     coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_intersection(lcz_shp) %>%
        sf::st_transform(crs = 3857)

      if (LCZinterp == TRUE) {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      } else {
        krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
        krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- data_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name
      }

      return(interp_map)


    }

    mapYear <- base::apply(iyear, 1, model_year)
    interp_year <- base::unlist(mapYear)
    interp_stack <- terra::rast(interp_year)

    if (isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      #Save map as raster.tif
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
      hemisphere <- extract_hemisphere(raster= {{ x }})


      my_latitude <- df_processed$latitude[1]
      my_longitude <- df_processed$longitude[1]
      mydata <- openair::cutData(df_processed, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>% tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata, pollutant = "var_interp",
                                     avg.time = tp.res,
                                     type = c("station", "my_time")) %>%
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

        #merge data-model with lcz_station to get lcz class
        df_interp_mod <-
          sf::st_as_sf(data_model,
                       coords = c("longitude", "latitude"), crs = 4326) %>%
          sf::st_intersection(lcz_shp) %>%
          sf::st_transform(crs = 3857)

        if(LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name

        } else {
          krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name
        }

        return(interp_map)

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        #Save map as raster.tif
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
        terra::writeRaster(interp_stack, file, overwrite = TRUE)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }
      return(interp_stack)

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
      hemisphere <- extract_hemisphere(raster= {{ x }})

      my_latitude <- df_processed$latitude[1]
      my_longitude <- df_processed$longitude[1]
      mydata <- openair::cutData(df_processed, type = by, hemisphere= hemisphere,
                                 latitude = my_latitude, longitude = my_longitude) %>% tidyr::drop_na() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
                                     pollutant = "var_interp",
                                     avg.time = tp.res,
                                     type = c("station", "daylight", "my_time")) %>% tidyr::drop_na()

      iby <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$my_time, .keep_all = FALSE) %>%
        base::expand.grid()

      model_by <- function(iby) {

        my_by <- iby[1]

        data_model <- mydata %>%
          dplyr::filter(.data$my_time == paste0(my_by))

        #merge data-model with lcz_station to get lcz class
        df_interp_mod <-
          sf::st_as_sf(data_model,
                       coords = c("longitude", "latitude"), crs = 4326) %>%
          sf::st_intersection(lcz_shp) %>%
          sf::st_transform(crs = 3857)

        if (LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name

        } else {
          krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name
        }

        return(interp_map)

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        #Save map as raster.tif
        file <- base::paste0(getwd(), "/", folder, "lcz4r_interp_map.tif")
        terra::writeRaster(interp_stack, file, overwrite = TRUE)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      return(interp_stack)

    }

    else {

      mydata <-
        openair::cutData(df_processed, type = by) %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(mydata,
                                     pollutant = "var_interp",
                                     avg.time = tp.res,
                                     type = c("station", "my_time")) %>%
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

        #merge data-model with lcz_station to get lcz class
        df_interp_mod <-
          sf::st_as_sf(data_model,
                       coords = c("longitude", "latitude"), crs = 4326) %>%
          sf::st_intersection(lcz_shp) %>%
          sf::st_transform(crs = 3857)

        if(LCZinterp == TRUE) {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name

        } else {
          krige_vgm <- automap::autofitVariogram(var_interp ~ 1, df_interp_mod, model = vg.model)
          krige_mod <- gstat::gstat(formula = var_interp ~ 1, model = krige_vgm$var_model, data = df_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid, debug.level = 0)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- data_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name
        }

        return(interp_map)

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if (isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        #Save map as raster.tif
        file <- base::paste0(folder,"lcz4r_interp_map.tif")
        terra::writeRaster(interp_stack, file, overwrite = TRUE)
        base::cat("The interp map rasters are saved into your pc. Look at 'LCZ4r_output' folder.\n")

      }

      return(interp_stack)


    }

  }

}


