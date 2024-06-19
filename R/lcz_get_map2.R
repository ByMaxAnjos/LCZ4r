#' Obtain the LCZ map from LCZ generator or with downloaded global LCZ map
#'
#' This function retrieves the Local Climate Zone (LCZ) global mapping dataset
#' created by Demuzere et al. (2022) and available at https://doi.org/10.5194/essd-14-3835-2022.
#' It allows you to obtain the LCZ map for a specific area of interest, which can be a city, state, region, or custom-defined shape.
#'
#' @param x A SpatRaster object containing the LCZ map. It can be download using these links: https://lcz-generator.rub.de/ , https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1".
#'          Then, it suggested use terra::rast(path/name.tif") to import the raster .tif into R.
#' @param city A character string specifying the name of your target area based on the OpenStreetMap project.
#' @param roi Optionally, you can provide a Region of Interest (ROI) in ESRI shapefile format to clip the LCZ map to a custom area.
#' @param isave_map Logical. Set to TRUE if you wish to save the resulting clipped map as a raster TIFF file on your local machine.
#' @return A Terra raster TIFF file containing LCZ classes for the specified area of interest.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Go to https://lcz-generator.rub.de/
#' my_map <- terra::rast("path/name.tif")
#' my_lcz_map <- lcz_get_map2(my_map, city = "Name")
#'
#' # Load the LCZ map from your PC
#' my_map <- terra::rast("path/lcz_filter_v3.tif")
#'
#' # Get LCZ map for a custom region of interest
#' custom_roi <- sf::st_read("custom_roi.shp")
#' roi_lcz <- lcz_get_map2(my_map, roi = custom_roi)
#'
#' # Retrieve the LCZ map for a country (no custom ROI specified)
#' my_lcz_country <- lcz_get_map2(my_map, city = "Lisbon")
#' }
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_get_map2 <- function(x, city=NULL, roi = NULL, isave_map = FALSE) {

  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster object. To download the map, use this link: https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1")
  }

  if(!inherits(x, "SpatRaster")) {
    x <- terra::rast(x)
  }

  if (terra::nlyr(x) > 1) {
    x <- x[[2]]
  }

  if(terra::crs(x, proj=TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {

    # If not, project it to WGS84
    x <- terra::project(x, "+proj=longlat +datum=WGS84 +no_defs")

  }

  if (is.null(city) & is.null(roi)) {
    stop("Error: provide either a city name or a roi polygon")
  }

  if (!is.null(city)) {
    # Get study area polygon from OpenStreetMap data
    shp_verify <- osmdata::getbb({{city}}, format_out = "sf_polygon", limit = 1)

    if (is.null(shp_verify)){
      stop(paste0("No polygonal boundary for", city, ".See https://nominatim.openstreetmap.org"))
    }
    # Check if polygon was obtained successfully
    if (!is.null(shp_verify$geometry) & !inherits(shp_verify, "list")) {
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

      # Crop the raster to the study area extent
      lcz_ras <- terra::crop(x, terra::ext(study_area))

      # Check if the cropped raster is null
      if (is.null(lcz_ras)) {
        stop("Oops! If you are working with very large raster datasets, consider working on a
           smaller area to reduce the memory and processing requirements.
           You can crop a smaller region first to see if the operation succeeds.")
      }

      # Mask the cropped raster
      lcz_ras <- terra::mask(lcz_ras, terra::vect(study_area))

      # Rename the raster layer to "lcz"
      base::names(lcz_ras) <- "lcz"

    if (isave_map==TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder,"lcz_map.tif")
      terra::writeRaster(lcz_ras, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(lcz_ras)


  } else {
    # ROI
    roi_crs <- roi %>%
      sf::st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

    #Crop the raster
      lcz_ras <- terra::crop(x, terra::ext(roi_crs))
      if (is.null(lcz_ras)) {
        stop("oops! If you are working with very large raster datasets, consider working on a
           smaller area to reduce the memory and processing requirements.
           You can crop a smaller region first to see if the operation succeeds.")
      }
      lcz_ras <- terra::mask(lcz_ras, terra::vect(roi_crs))
      base::names(lcz_ras) <- "lcz"
    }

    if (isave_map==TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder,"lcz_map.tif")
      terra::writeRaster(lcz_ras, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(lcz_ras)

}

