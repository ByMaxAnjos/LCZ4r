
#' Get LCZ Map for Your Area
#'
#' This function imports the LCZ (Local Climate Zone) global mapping dataset
#' performed by Demuzere et al. (2022), available at https://doi.org/10.5194/essd-14-3835-2022.
#' It automatically clips the LCZ map to your specified area of interest, which can be a city, state, region, or neighborhood.
#'
#' @param city The name of your city or the area you want to focus on. It is based on Open Street Map project.
#' @param roi Optionally, you can provide a Region of Interest (ROI) in ESRI shapefile format to clip the LCZ map.
#' @param isave_map Set to TRUE if you want to save the your clipped resulting map as a raster.tiff file on your local machine.
#' @param isave_global Set to TRUE if you want to save the global resulting map as a raster.tiff file on your local machine.
#' @return A Terra raster.tiff file containing the LCZ classes for your specified area.
#'
#' @export
#'
#' @examples
#' For city
#' myLczmap <- getLCZmap(city = "Berlin", roi = NULL, isave_map = TRUE, isave_global = TRUE)
#'
#' For neighborhood.
#' myLczmap <- getLCZmap(city = "Berlin, Mitte", roi = NULL, isave_map = TRUE, isave_global = TRUE)
#'
#' Get LCZ map for a custom region of interest
#' custom_roi <- st_read("custom_roi.shp")
#' roi_lcz <- getLCZmap(roi = custom_roi, isave_map = TRUE, isave_global = TRUE)

getLCZmap <- function(city=NULL, roi = NULL, isave_map = TRUE, isave_global = TRUE) {
  # Validate inputs
  if (is.null(city) & is.null(roi)) {
    stop("Error: provide either a city name or a roi polygon")
  } else if (!is.null(city) & !is.character(city)) {
    stop("Error: city input must be a character string")
  } else if (!is.null(roi) & !inherits(roi, "sf")) {
    stop("Error: ROI input must be a polygon object of class sf")
  }

  if(!is.null(city)) {
    # Get study area polygon from OpenStreetMap data
    shp_verify <- osmdata::getbb(city, format_out = "sf_polygon", limit = 1)

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
    lcz_url <- "https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1"
    lcz_download <- terra::rast(paste0("/vsicurl/", lcz_url))
    lcz_ras <- terra::crop(lcz_download, terra::ext(study_area))
    lcz_ras <- terra::mask(lcz_ras, terra::vect(study_area))
    names(lcz_ras) <- "lcz"
    return(lcz_ras)
  } else {
    # Download the LCZ global map from https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1
    lcz_url <- "https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1"
    lcz_download <- terra::rast(paste0("/vsicurl/", lcz_url))
    roi_crs <- roi %>%
      sf::st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
    lcz_ras <- terra::crop(lcz_download, terra::ext(roi_crs))
    lcz_ras <- terra::mask(lcz_ras, terra::vect(roi_crs))
    names(lcz_ras) <- "lcz"

    return(lcz_ras)

    if(isave_map==TRUE){

      # Create a folder name using paste0
      folder <- paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        dir.create(folder)
      }

      file <- paste0(folder,"lcz_map.tif")

      raster::writeRaster(lcz_ras, file, format="GTiff", overwrite = TRUE)
    }

    if(isave_global==TRUE){

      # Create a folder name using paste0
      folder <- paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        dir.create(folder)
      }

      file <- paste0(folder,"lcz_globalmap.tif")

      raster::writeRaster(lcz_download, file, format="GTiff", overwrite = TRUE)
    }

  }

}
