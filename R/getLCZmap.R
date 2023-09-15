
#' Get your LCZ map
#'
#' @param city
#' @param roi
#' @param isave
#'
#' @return
#' @export
#'
#' @examples
getLCZmap <- function(city=NULL, roi = NULL, isave = TRUE) {
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
    # Download the LCZ global map from https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1
    lcz_download <- terra::rast("/vsicurl/https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1")
    lcz_ras <- terrra::crop(lcz_download, terra::ext(study_area))
    lcz_ras <- terra::mask(lcz_ras, terra::vect(study_area))
    names(lcz_ras) <- paste0("LCZ")
    return(lcz_ras)
  } else {
    # Download the LCZ global map from https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1
    lcz_download <- terra::rast("/vsicurl/https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1")
    roi_crs <- roi %>%
      sf::st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
    lcz_ras <- terra::crop(lcz_download, terra::ext(roi_crs))
    lcz_ras <- terra::mask(lcz_ras, terra::vect(roi_crs))
    names(lcz_ras) <- "LCZ"
    return(lcz_ras)

    if(isave==TRUE){
      raster::writeRaster(lcz_download, paste0(getwd(), "/lcz_Globalmap.tif"), format="GTiff", overwrite = TRUE)
    }
  }

}
