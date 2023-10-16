
#' Get LCZ Map for Your Area
#'
#' This function imports the LCZ (Local Climate Zone) global mapping dataset
#' performed by Demuzere et al. (2022), available at https://doi.org/10.5194/essd-14-3835-2022.
#' It automatically clips the LCZ map to your specified area of interest, which can be a city, state, region, or neighborhood.
#'
#' @param city The name of your city or the area you want to focus on. It is based on Open Street Map project.
#' @param roi Optionally, you can provide a Region of Interest (ROI) in ESRI shapefile format to clip the LCZ map.
#' @param isave_map Set to TRUE if you want to save the your clipped resulting map as a raster.tiff file on your local machine.
#' @return A Terra raster.tiff file containing the LCZ classes for your specified area.
#'
#' @export
#'
#' @examples
#'
#' # Load the LCZ map
#' #my_lcz_city <- lcz_get_map(city = "ExampleCity")
#'
#' #Get LCZ map for a custom region of interest
#' #custom_roi <- sf::st_read("custom_roi.shp")
#' #roi_lcz <- lcz_get_map(roi = custom_roi, isave_map = TRUE)
#'
#' #For National scale
#' #my_lcz_country <- lcz_get_map(city = "Brazil", roi = NULL, isave_map = TRUE)

lcz_get_map <- function(city=NULL, roi = NULL, isave_map = FALSE) {

  # # Validate inputs
  # if (is.null(city) & is.null(roi)) {
  #   stop("Error: provide either a city name or a roi polygon")
  # } else if (!is.null(city) & !is.character(city)) {
  #   stop("Error: city input must be a character string")
  # } else if (!is.null(roi) & !inherits(roi, "sf")) {
  #   stop("Error: ROI input must be a polygon object of class sf")
  # }

  if(!is.null(city)) {
    # Get study area polygon from OpenStreetMap data
    shp_verify <- osmdata::getbb({{city}}, format_out = "sf_polygon", limit = 1)

    if(is.null(shp_verify)){
      stop(paste0("No polygonal boundary for", city, ".See https://nominatim.openstreetmap.org"))
    }
    # Check if polygon was obtained successfully
    if(!is.null(shp_verify$geometry) & !inherits(shp_verify, "list")) {
      study_area <- shp_verify$geometry
      study_area <- sf::st_make_valid(study_area) %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
    } else {
      study_area <- shp_verify$multipolygon
      study_area <- sf::st_make_valid(study_area) %>%
        sf::st_as_sf() %>%
        sf::st_transform(crs="+proj=longlat +datum=WGS84 +no_defs")
    }

    # Download the LCZ global map

    # rgdal::setCPLConfigOption(ConfigOption ="GDAL_HTTP_UNSAFESSL",value ="YES")
    # rgdal::setCPLConfigOption(ConfigOption ="GDAL_HTTP_COOKIEFILE",value =".rcookies")
    # rgdal::setCPLConfigOption(ConfigOption ="GDAL_HTTP_COOKIEJAR",value =".rcookies")
    # rgdal::setCPLConfigOption(ConfigOption ="GDAL_DISABLE_READDIR_ON_OPEN",value ="EMPTY_DIR")
    # rgdal::setCPLConfigOption(ConfigOption ="CPL_VSIL_CURL_ALLOWED_EXTENSIONS",value ="TIF")

    lcz_url <- "https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1"
    lcz_download <- terra::rast(base::paste0("/vsicurl/", lcz_url))

    if(base::is.null(lcz_download)) {
      stop("This error might be due to server restrictions. In such cases, you may need to download the file manually using this link:
           https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1.
           Then read it using the terra package, eg., my_map <- rast('path/lcz_filter_v3.tif')")
    }

    lcz_ras <- terra::crop(lcz_download, terra::ext(study_area), snap = "out", mask= TRUE)
    lcz_ras <- terra::mask(lcz_ras, terra::vect(study_area))
    base::names(lcz_ras) <- "lcz"

    if(isave_map==TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_map.tif")

      terra::writeRaster(lcz_ras, file, overwrite = TRUE)
    }

    return(lcz_ras)

  } else {
    # Download the LCZ global map from https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1
    lcz_url <- "https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1"
    lcz_download <- terra::rast(base::paste0("/vsicurl/", lcz_url))
    roi_crs <- roi %>%
      sf::st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
    lcz_ras <- terra::crop(lcz_download, terra::ext(roi_crs), mask = TRUE)
    lcz_ras <- terra::mask(lcz_ras, terra::vect(roi_crs))
    base::names(lcz_ras) <- "lcz"

    if(isave_map==TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_map.tif")

      terra::writeRaster(lcz_ras, file, overwrite = TRUE)
    }

    if(isave_global==TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_globalmap.tif")

      terra::writeRaster(lcz_download, file, overwrite = TRUE)
    }

    return(lcz_ras)
  }

}

