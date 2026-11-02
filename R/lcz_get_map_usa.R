#' Download the CONUS LCZ map
#'
#' Retrieves the Local Climate Zone map for the Continental United States
#' (Demuzere et al., 2020) for a city or a custom region of interest.
#'
#' @param city A character string with the name of the target area, as used by
#'   OpenStreetMap/Nominatim.
#' @param roi Optional `sf`/`sfc` polygon used to clip the LCZ map.
#' @param isave_map Logical. Save the clipped map as a GeoTIFF.
#' @param isave_usa Logical. Save the full CONUS LCZ map as a GeoTIFF.
#' @param lang Language used for console messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return A `SpatRaster` with LCZ classes (1-17).
#'
#' @export
#'
#' @references
#' Demuzere, M., Hankey, S., Mills, G., Zhang, W., Lu, T., and Bechtel, B.
#' (2020). Combining expert and crowd-sourced training data to map urban form
#' and functions for the continental US. Scientific Data 7(1), 264.
#' \doi{10.1038/s41597-020-00605-z}
#'
#' Demuzere, M., Hankey, S., Mills, G., Zhang, W., Lu, T., and Bechtel, B.
#' (2020). CONUS-wide LCZ map and training areas. Figshare.
#' \doi{10.6084/m9.figshare.11416950}
#'
#' @examples
#' \dontrun{
#' my_lcz_city <- lcz_get_map_usa(city = "New York")
#' custom_roi <- sf::st_read("custom_roi.shp")
#' roi_lcz <- lcz_get_map_usa(roi = custom_roi)
#' }
#'
#' @seealso [lcz_get_map()], [lcz_get_map_euro()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_get_map_usa <- function(city = NULL, roi = NULL, isave_map = FALSE,
                            isave_usa = FALSE, lang = "en") {
  lcz_clip_lcz_map(
    city = city, roi = roi,
    url = "https://zenodo.org/records/10835692/files/CONUS_LCZ_map_NLCD_v1.0_epsg4326.tif",
    crs = 4326,
    isave_map = isave_map,
    isave_full = isave_usa,
    full_name = "lcz_usa_map.tif",
    lang = lang
  )
}
