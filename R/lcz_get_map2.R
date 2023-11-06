
#' Get Local Climate Zone (LCZ) Map for a Specified Area 2
#'
#' This function retrieves the Local Climate Zone (LCZ) global mapping dataset
#' created by Demuzere et al. (2022) and available at https://doi.org/10.5194/essd-14-3835-2022.
#' It allows you to obtain the LCZ map for a specific area of interest, which can be a city, state, region, or custom-defined shape.
#'
#' @param x A SpatRaster object containing the LCZ map. It can be download using this link: https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1"
#' @param city A character string specifying the name of your target area based on the OpenStreetMap project.
#' @param roi Optionally, you can provide a Region of Interest (ROI) in ESRI shapefile format to clip the LCZ map to a custom area.
#' @param isave_map Logical. Set to TRUE if you wish to save the resulting clipped map as a raster TIFF file on your local machine.
#' @return A Terra raster TIFF file containing LCZ classes for the specified area of interest.
#'
#' @export
#'
#' @examples
#'
#' # Load the LCZ map from you pc
#' # my_map <- terra::rast(path/lcz_filter_v3.tif")
#'
#' # Example 2: Get LCZ map for a custom region of interest
#' # custom_roi <- sf::st_read("custom_roi.shp")
#' # roi_lcz <- lcz_get_map2(my_map, roi = custom_roi)
#'
#' # Example 3: Retrieve the LCZ map for a country (no custom ROI specified)
#' # my_lcz_country <- lcz_get_map2(my_map, city = "Brazil")

lcz_get_map2 <- function(x, city=NULL, roi = NULL, isave_map = FALSE) {

  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster object. To download the map, use this link: https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1")
  }

  if (is.null(city) & is.null(roi)) {
    stop("Error: provide either a city name or a roi polygon")
  }

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
    options(warn=-1)
    # Download the LCZ global map
    lcz_ras <- terra::crop(x, terra::ext(study_area))

    if(is.null(lcz_ras)) {
      stop("Large Data: If you are working with very large raster datasets, consider working on a
           subset of the data to reduce the memory and processing requirements.
           You can crop a smaller region first to see if the operation succeeds.")
    }

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
    # ROI
    roi_crs <- roi %>%
      sf::st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

    lcz_ras <- terra::crop(x, terra::ext(roi_crs))

    if(is.null(lcz_ras)) {
      stop("Large Data: If you are working with very large raster datasets, consider working on a
           subset of the data to reduce the memory and processing requirements.
           You can crop a smaller region first to see if the operation succeeds.")

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

      return(lcz_ras)
    }

  }

}

