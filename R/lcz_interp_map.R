#' Visualize the LCZ - interpolated map
#'
#' This function generates a spatial interpolation of any air temperature (or other variable) for different Local Climate Zones (LCZs). More details: https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html
#'
#' @param x A SpatRaster object containing the LCZ map. The LCZ map can be obtained using the lcz_get_map() function.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var Name of the variable for interpolation, e.g. air temperature, in the dataframe.
#' @param station_id Name of the station ID variable in the dataframe.
#' @param sp.res Spatial resolution in unit of meters for interpolation. Default is 100.
#' @param tp.res Temporal resolution, the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.
#' @param by data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'           \dQuote{daylight}(daytime and nighttime).See argument \emph{type} in openair package: https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option
#' @param method Statistical technique used in the interpolation, such as kriging and IDW. Default is "kriging".
#' @param ... Utilities from \code{selectBydata} from \code{openair} package. A start date string in the form e.g. \dQuote{1/2/1999} or in format i.e. \dQuote{YYYY-mm-dd}, \dQuote{1999-02-01}.
#'            A year or years to select e.g. year = 1998:2004 to select 1998-2004 inclusive or year = c(1998, 2004) to select 1998 and 2004. A month or months to select.
#'            Can either be numeric e.g. month = 1:6 to select months 1-6 (January to June), or by name e.g. month = c(\dQuote{January}, \dQuote{December}).
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param isave Save the plot into your directory.
#'
#' @return A map of LCZ-interpolatation in \code{terra raster.tif} format
#'
#' @export
#'
#' @examples
#'
#' # Daily air temperature values in September 2019 .
#'  #my_interp <- lcz_interp_map(lcz_map, data_frame = lcz_data, var = "airT",
#'  #                              station_id = "station", tp.res = "day", sp.res= "100",
#'  #                              year = 2019, month=9)
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_map() to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_interp_map <- function(x,
                            data_frame = "",
                            var = "",
                            station_id = "",
                            sp.res = 100,
                            tp.res = "hour",
                            method = "Kriging",
                            ...,
                            by = NULL,
                            impute = NULL,
                            isave = FALSE) {

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
  df_variable <- data_frame %>%
    dplyr::rename(var_interp = {{var}}, station = {{station_id}}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(lcz_id = dplyr::cur_group_id(),
                  lcz_id = base::as.factor(.data$lcz_id)) %>%
    dplyr::ungroup()

  #Impute missing values if it is necessary
  if (!is.null(impute)) {
    if (impute == "mean") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_variable) %>%
        recipes::step_impute_mean(.data$var_interp)

      df_variable <- lcz_recipe %>%
        recipes::prep(df_variable) %>%
        recipes::bake(new_data = NULL)
    }

    if (impute == "median") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_variable) %>%
        recipes::step_impute_median(.data$var_interp)

      df_variable <- lcz_recipe %>%
        recipes::prep(df_variable) %>%
        recipes::bake(new_data = NULL)
    }

    if (impute == "knn") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_variable) %>%
        recipes::step_impute_knn(.data$var_interp)

      df_variable <- lcz_recipe %>%
        recipes::prep(df_variable) %>%
        recipes::bake(new_data = NULL)
    }

    if (impute == "bag") {
      lcz_recipe <-
        recipes::recipe(.data$var_interp ~ ., data = df_variable) %>%
        recipes::step_impute_bag(.data$var_interp)

      df_variable <- lcz_recipe %>%
        recipes::prep(df_variable) %>%
        recipes::bake(new_data = NULL)
    }

    base::cat("Hooray! The missing values have been imputed with ", impute,"\n")
  }

  #Define the period
  df_period <- df_variable %>%
    dplyr::select(.data$date, .data$station, .data$lcz_id, .data$var_interp) %>%
    openair::selectByDate(...) %>%
    openair::timeAverage(avg.time = tp.res, type = c("station", "lcz_id"))

  df_processed <- dplyr::inner_join(df_period,
                                    df_variable %>% dplyr::select(-.data$station,-.data$var_interp),
                                    by = c("date", "lcz_id"))

  # Geospatial operations ---------------------------------------------------
  # Assuming your raster is named 'raster'

  unique_classes <- base::unique(base::as.integer(terra::values({{x}})))
  unique_classes <- unique_classes[!is.na(unique_classes) & !is.nan(unique_classes)]
  num_classes <- base::length(unique_classes)

  #Convert lcz_map to polygon
  lcz_shp <- terra::as.polygons({{x}}) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)
  #Convert to list of polygon
  poly_list <- base::lapply(sf::st_geometry(lcz_shp), function(x)
    sf::st_sfc(x, crs = 4326))
  #Calculate areas of each polygon
  lcz_areas <- base::lapply(1:base::length(poly_list), function(i)
    sf::st_area(poly_list[[i]]))
  #Sample the number of points according to area of the polygon and convert to data.frame
  lcz_poi <- base::lapply(base::seq_along(poly_list), function(i) {
    points <- sf::st_sample(poly_list[[i]], size = 2*num_classes, prob = lcz_areas[[i]])
    base::as.data.frame(points)
  })
  #Merge all dataframes and convert to sf points
  lcz_poi <- base::do.call(rbind.data.frame, lcz_poi) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)
  #Intersect lcz poi with lcz shp
  lcz_poi_get <- sf::st_intersection(lcz_poi, lcz_shp)

  #Get shp LCZ stations from lat and long
  shp_stations <- df_processed %>%
    dplyr::mutate(lcz_id = base::as.factor(.data$lcz_id)) %>%
    dplyr::distinct(.data$latitude, .data$longitude, .keep_all = T) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  #Intersect poi shp stations with lcz shp
  lcz_poi_get$lcz <- base::as.factor(lcz_poi_get$lcz)
  lcz_stations <- sf::st_intersection(shp_stations, lcz_shp) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$lcz_id, .data$station, .data$lcz) %>%
    dplyr::mutate(
      lcz = base::as.factor(.data$lcz),
      lcz_id = base::as.factor(.data$lcz_id))

  #Merge table to create a sf to model

  lcz_poi_mod <- dplyr::inner_join(lcz_poi_get, lcz_stations, by=c("lcz")) %>%
    sf::st_transform(crs = 3857)

  #Get id stations
  my_stations <- df_processed %>%
    dplyr::distinct(.data$station, .keep_all = F)

  #Re-project and make a grid to interpolation
  lcz_box <- sf::st_transform(lcz_shp, crs = 3857)

  ras_resolution <- sf::st_bbox(lcz_box) %>%
    stars::st_as_stars(dx = sp.res)
  ras_resolution <- terra::rast(ras_resolution)

  ras_project <- terra::project({{x}}, "EPSG:3857")
  ras_resample <- terra::resample(ras_project, ras_resolution, method = "mode")
  ras_grid <- stars::st_as_stars(ras_resample, dimensions = "XY")
  base::names(ras_grid) <- "lcz"


  # Calculate interp temporal resolution  ------------------------------------------------------
  if(is.null(by) & tp.res %in% c("hour", "day")) {  #Downscale to hour or day
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
                        lcz_id = base::as.factor(.data$lcz_id)) %>%
          dplyr::filter(.data$hour == paste0(myhour))

        #merge data-model with lcz_station to get lcz class
        lcz_model <-
          dplyr::inner_join(data_model, lcz_stations, by = c("station", "lcz_id")) %>%
          dplyr::mutate(
            lcz = base::as.factor(.data$lcz),
            lcz_id = base::as.factor(.data$lcz_id))

        lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
          sf::st_transform(crs = 3857) %>%
          dplyr::mutate(lcz = base::as.integer(.data$lcz))
        lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
        sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

        if(method == "kriging") {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate)
          base::names(interp_map) <- ras_name

        }

        if(method == "IDW") {

          # [inverse distance weighted interpolation]
          idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
          idw_map <- idw_map["var1.pred",,]
          interp_map <- terra::rast(idw_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("idw_", mydate)
          names(interp_map) <- ras_name
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

    if(isave == TRUE){

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

    base::cat("That's cool! Let's explore your LCZ interp map.\n")

    return(interp_stack) }

  if(is.null(by) & tp.res %in% c("week")) {  #Downscale to week

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
                      lcz_id = base::as.factor(.data$lcz_id)) %>%
        dplyr::filter(.data$iweek == paste0(my_week))

      #merge data-model with lcz_station to get lcz class
      lcz_model <-
        dplyr::inner_join(data_model, lcz_stations, by = c("station", "lcz_id")) %>%
        dplyr::mutate(
          lcz = base::as.factor(.data$lcz),
          lcz_id = base::as.factor(.data$lcz_id))
      lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
        sf::st_transform(crs = 3857) %>%
        dplyr::mutate(lcz = base::as.integer(.data$lcz))
      lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
      sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

      if(method == "kriging") {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate, "_", my_week)
        base::names(interp_map) <- ras_name

      }

      if(method == "IDW") {

        # [inverse distance weighted interpolation]
        idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
        idw_map <- idw_map["var1.pred",,]
        interp_map <- terra::rast(idw_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("idw_", mydate, "_", my_week)
        names(interp_map) <- ras_name
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
      file <- base::paste0(folder,"lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      base::cat("The interp map rasters are saved into your pc. Look at 'LCZ4r_output' folder.\n")

    }

    base::cat("That's cool! Let's explore your LCZ interp map.\n")

    return(interp_stack)

  }

  if(is.null(by) & tp.res %in% c("month")) {  #Downscale to month

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
                      lcz_id = base::as.factor(.data$lcz_id)) %>%
        dplyr::filter(.data$imonth == paste0(my_month))

      #merge data-model with lcz_station to get lcz class
      lcz_model <-
        dplyr::inner_join(data_model, lcz_stations, by = c("station", "lcz_id")) %>%
        dplyr::mutate(
          lcz = base::as.factor(.data$lcz),
          lcz_id = base::as.factor(.data$lcz_id))

      lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
        sf::st_transform(crs = 3857) %>%
        dplyr::mutate(lcz = base::as.integer(.data$lcz))
      lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
      sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

      if(method == "kriging") {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      }

      if(method == "IDW") {

        # [inverse distance weighted interpolation]
        idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
        idw_map <- idw_map["var1.pred",,]
        interp_map <- terra::rast(idw_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("idw_", mydate)
        names(interp_map) <- ras_name
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
      file <- base::paste0(folder,"lcz4r_interp_map.tif")
      terra::writeRaster(interp_stack, file, overwrite = TRUE)
      base::cat("The interp map rasters are saved into your pc. Look at 'LCZ4r_output' folder.\n")

    }

    base::cat("That's cool! Let's explore your LCZ interp map.\n")

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
                      lcz_id = base::as.factor(.data$lcz_id)) %>%
        dplyr::filter(.data$iquarter == paste0(my_quarter))

      #merge data-model with lcz_station to get lcz class
      lcz_model <-
        dplyr::inner_join(data_model, lcz_stations, by = c("station", "lcz_id")) %>%
        dplyr::mutate(
          lcz = base::as.factor(.data$lcz),
          lcz_id = base::as.factor(.data$lcz_id))

      lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
        sf::st_transform(crs = 3857) %>%
        dplyr::mutate(lcz = base::as.integer(.data$lcz))
      lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
      sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

      if(method == "kriging") {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      }

      if(method == "IDW") {

        # [inverse distance weighted interpolation]
        idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
        idw_map <- idw_map["var1.pred",,]
        interp_map <- terra::rast(idw_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("idw_", mydate)
        names(interp_map) <- ras_name
      }

      return(interp_map)

    }

    mapQuarter <- base::apply(iquarter, 1, model_quarter)
    interp_quarter <- base::unlist(mapQuarter)
    interp_stack <- terra::rast(interp_quarter)

    if(isave == TRUE){

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

    base::cat("That's cool! Let's explore your LCZ interp map.\n")

    return(interp_stack)

  }

  if(is.null(by) & tp.res %in% c("year")) {  #Downscale to year

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
                      lcz_id = base::as.factor(.data$lcz_id)) %>%
        dplyr::filter(.data$iyear == paste0(my_year))

      #merge data-model with lcz_station to get lcz class
      lcz_model <-
        dplyr::inner_join(data_model, lcz_stations, by = c("station", "lcz_id")) %>%
        dplyr::mutate(
          lcz = base::as.factor(.data$lcz),
          lcz_id = base::as.factor(.data$lcz_id))
      lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
        sf::st_transform(crs = 3857) %>%
        dplyr::mutate(lcz = base::as.integer(.data$lcz))
      lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
      sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

      if(method == "kriging") {
        # [using ordinary kriging]
        krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
        krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
        krige_map <- terra::predict(krige_mod, newdata=ras_grid)
        krige_map <- krige_map["var1.pred",,]
        interp_map <- terra::rast(krige_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("krige_", mydate)
        base::names(interp_map) <- ras_name

      }

      if(method == "IDW") {

        # [inverse distance weighted interpolation]
        idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
        idw_map <- idw_map["var1.pred",,]
        interp_map <- terra::rast(idw_map)
        interp_map <- terra::focal(interp_map, w=7, fun=mean)
        mydate <- lcz_model %>% dplyr::pull(.data$date)
        mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
        ras_name <- base::paste0("idw_", mydate)
        names(interp_map) <- ras_name
      }

      return(interp_map)


    }

    mapYear <- base::apply(iyear, 1, model_year)
    interp_year <- base::unlist(mapYear)
    interp_stack <- terra::rast(interp_year)

    if(isave == TRUE){

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

    base::cat("That's cool! Let's explore your LCZ interp map.\n")

    return(interp_stack)

  }

  if (!is.null(by)) {

    if(length(by)<2 & by %in% c("daylight", "season", "seasonyear")) {

      # Extract AXIS information from CRS
      axis_matches <- terra::crs({{x}}, parse = TRUE)[14]
      # Extract hemisphere from AXIS definition
      hemisphere <- ifelse(grepl("north", axis_matches), "northern", "southern")

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
        lcz_model <-
          dplyr::inner_join(data_model, lcz_stations, by = c("station")) %>%
          dplyr::mutate(
            lcz = base::as.factor(.data$lcz),
            lcz_id = base::as.factor(.data$lcz_id))
        lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
          sf::st_transform(crs = 3857) %>%
          dplyr::mutate(lcz = base::as.integer(.data$lcz))
        lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
        sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

        if(method == "kriging") {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate, "_", my_by)
          base::names(interp_map) <- ras_name

        }

        if(method == "IDW") {

          # [inverse distance weighted interpolation]
          idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
          idw_map <- idw_map["var1.pred",,]
          interp_map <- terra::rast(idw_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("idw_", mydate, "_", my_by)
          names(interp_map) <- ras_name
        }

        return(interp_map)

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if(isave == TRUE){

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

      base::cat("That's cool! Let's explore your LCZ interp map.\n")

      return(interp_stack)

    }

    if(length(by)>1 & by %in% "daylight") {

      # Extract AXIS information from CRS
      axis_matches <- terra::crs(x, parse = TRUE)[14]
      # Extract hemisphere from AXIS definition
      hemisphere <- ifelse(grepl("north", axis_matches), "northern", "southern")

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
        lcz_model <-
          dplyr::inner_join(data_model, lcz_stations, by = c("station")) %>%
          dplyr::mutate(
            lcz = base::as.factor(.data$lcz),
            lcz_id = base::as.factor(.data$lcz_id))

        lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
          sf::st_transform(crs = 3857) %>%
          dplyr::mutate(lcz = base::as.integer(.data$lcz))
        lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
        sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

        if(method == "kriging") {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate, "_", my_by)
          base::names(interp_map) <- ras_name

        }

        if(method == "IDW") {

          # [inverse distance weighted interpolation]
          idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
          idw_map <- idw_map["var1.pred",,]
          interp_map <- terra::rast(idw_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("idw_", mydate, "_", my_by)
          names(interp_map) <- ras_name
        }

        return(interp_map)

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if(isave == TRUE){

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

      base::cat("That's cool! Let's explore your LCZ interp map.\n")

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
        lcz_model <-
          dplyr::inner_join(data_model, lcz_stations, by = c("station")) %>%
          dplyr::mutate(
            lcz = base::as.factor(.data$lcz),
            lcz_id = base::as.factor(.data$lcz_id))
        lcz_interp_mod <- dplyr::inner_join(lcz_poi_mod, lcz_model, by=c("lcz_id","lcz")) %>%
          sf::st_transform(crs = 3857) %>%
          dplyr::mutate(lcz = base::as.integer(.data$lcz))
        lcz_interp_mod <-lcz_interp_mod[!is.na(lcz_interp_mod$lcz),]
        sf::st_crs(ras_grid) <- sf::st_crs(lcz_interp_mod)

        if(method == "kriging") {
          # [using ordinary kriging]
          krige_vgm <- automap::autofitVariogram(var_interp ~ lcz, lcz_interp_mod)$var_model
          krige_mod <- gstat::gstat(formula = var_interp ~ lcz, model = krige_vgm$var_model, data = lcz_interp_mod)
          krige_map <- terra::predict(krige_mod, newdata=ras_grid)
          krige_map <- krige_map["var1.pred",,]
          interp_map <- terra::rast(krige_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("krige_", mydate, "_", my_by)
          base::names(interp_map) <- ras_name

        }

        if(method == "IDW") {

          # [inverse distance weighted interpolation]
          idw_map <- gstat::idw(var_interp~1, lcz_interp_mod, ras_grid)
          idw_map <- idw_map["var1.pred",,]
          interp_map <- terra::rast(idw_map)
          interp_map <- terra::focal(interp_map, w=7, fun=mean)
          mydate <- lcz_model %>% dplyr::pull(.data$date)
          mydate <- base::gsub("[: -]", "" , mydate[1], perl=TRUE)
          ras_name <- base::paste0("idw_", mydate, "_", my_by)
          names(interp_map) <- ras_name
        }

        return(interp_map)

      }

      mapBy <- base::apply(iby, 1, model_by)
      interp_by <- base::unlist(mapBy)
      interp_stack <- terra::rast(interp_by)

      if(isave == TRUE){

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

      base::cat("That's cool! Let's explore your LCZ interp map.\n")

      return(interp_stack)


    }

  }

}


