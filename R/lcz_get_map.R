
#' Download the LCZ map
#'
#' This function retrieves the Local Climate Zone (LCZ) global mapping dataset
#' created by Demuzere et al. (2022) and available at https://doi.org/10.5194/essd-14-3835-2022.
#' It allows you to obtain the LCZ map for a specific area of interest, which can be a city, state, region, or custom-defined shape.
#'
#' @param city A character string specifying the name of your target area based on the OpenStreetMap project.
#' @param roi Optionally, you can provide a Region of Interest (ROI) in ESRI shapefile format to clip the LCZ map to a custom area.
#' @param isave_map Logical. Set to TRUE if you wish to save the resulting clipped map as a raster TIFF file on your local machine.
#' @param isave_global Logical. Set to TRUE if you wish to save the global LCZ map as a raster TIFF file on your local machine.
#' @return A Terra raster TIFF file containing LCZ classes for the specified area of interest.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the LCZ map for a city
#' my_lcz_city <- lcz_get_map(city = "ExampleCity")
#'
#' # Get LCZ map for a custom region of interest
#' custom_roi <- sf::st_read("custom_roi.shp")
#' roi_lcz <- lcz_get_map(roi = custom_roi, isave_map = TRUE)
#'
#' # Retrieve the LCZ map for a country (no custom ROI specified)
#' my_lcz_country <- lcz_get_map(city = "Brazil", roi = NULL, isave_map = TRUE)
#' }
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_get_map <- function(city = NULL, roi = NULL, isave_map = FALSE, isave_global = FALSE) {
  # Validate inputs
  if (is.null(city) & is.null(roi)) {
    stop("Error: provide either a city name or a roi polygon")
  }

  if (!is.null(city)) {
    # Get study area polygon from OpenStreetMap data
    shp_verify <- osmdata::getbb({{ city }}, format_out = "sf_polygon", limit = 1)

    if (is.null(shp_verify)) {
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
        sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
    }
    options(warn = -1)
    # Download the LCZ global map
    lcz_url <- "https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1"
    lcz_download <- terra::rast(base::paste0("/vsicurl/", lcz_url))

    if (base::is.null(lcz_download)) {
      stop("This error might be due to server restrictions. In such cases, you may need to download the file manually using this link:
           https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1.
           Then read it using the terra package, eg., my_map <- rast('path/lcz_filter_v3.tif'), and use the function lcz_get_map2()")
    }

    lcz_ras <- terra::crop(lcz_download, terra::ext(study_area))

    if (is.null(lcz_ras)) {
      stop("Large Data: If you are working with very large raster datasets, consider working on a
           subset of the data to reduce the memory and processing requirements.
           You can crop a smaller region first to see if the operation succeeds.")
    }

    lcz_ras <- terra::mask(lcz_ras, terra::vect(study_area))
    base::names(lcz_ras) <- "lcz"

    if (isave_map == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder, "lcz_map.tif")
      terra::writeRaster(lcz_ras, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    if (isave_global == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder, "lcz_global_map.tif")
      terra::writeRaster(lcz_download, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(lcz_ras)
  } else {
    # Download the LCZ global map from https://zenodo.org/record/6364594/files/lcz_filter_v1.tif?download=1
    lcz_url <- "https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1"
    lcz_download <- terra::rast(base::paste0("/vsicurl/", lcz_url))
    roi_crs <- roi %>%
      sf::st_as_sf() %>%
      sf::st_make_valid() %>%
      sf::st_transform(crs = 4326)

    lcz_ras <- terra::crop(lcz_download, terra::ext(roi_crs))

    if (is.null(lcz_ras)) {
      stop("Large Data: If you are working with very large raster datasets, consider working on a
           subset of the data to reduce the memory and processing requirements.
           You can crop a smaller region first to see if the operation succeeds.")
    }

    lcz_ras <- terra::mask(lcz_ras, terra::vect(roi_crs))
    base::names(lcz_ras) <- "lcz"

    if (isave_map == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder, "lcz_map.tif")
      terra::writeRaster(lcz_ras, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    if (isave_global == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder, "lcz_global_map.tif")
      terra::writeRaster(lcz_download, file, overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(lcz_ras)
  }
}

