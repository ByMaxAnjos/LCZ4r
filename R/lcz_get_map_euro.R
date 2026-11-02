#' Download the European LCZ map
#'
#' Retrieves the European Local Climate Zone dataset (Demuzere et al., 2019)
#' for a city or a custom region of interest.
#'
#' @param city A character string with the name of the target area, as used by
#'   OpenStreetMap/Nominatim.
#' @param roi Optional `sf`/`sfc` polygon used to clip the LCZ map.
#' @param isave_map Logical. Save the clipped map as a GeoTIFF.
#' @param isave_euro Logical. Save the full European LCZ map as a GeoTIFF.
#' @param lang Language used for console messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return A `SpatRaster` with LCZ classes (1-17), in EPSG:3035.
#'
#' @export
#'
#' @references
#' Demuzere, M., Bechtel, B., Middel, A., and Mills, G. (2019). Mapping Europe
#' into local climate zones. PLOS ONE 14(4), e0214474.
#' \doi{10.1371/journal.pone.0214474}
#'
#' Demuzere, M., Bechtel, B., Middel, A., and Mills, G. (2019). European LCZ
#' map. Figshare. \doi{10.6084/m9.figshare.13322450}
#'
#' @examples
#' \dontrun{
#' my_lcz_city <- lcz_get_map_euro(city = "Berlin")
#' custom_roi <- sf::st_read("custom_roi.shp")
#' roi_lcz <- lcz_get_map_euro(roi = custom_roi)
#' }
#'
#' @seealso [lcz_get_map()], [lcz_get_map_usa()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_get_map_euro <- function(city = NULL, roi = NULL, isave_map = FALSE,
                             isave_euro = FALSE, lang = "en") {
  lcz_clip_lcz_map(
    city = city, roi = roi,
    url = "https://zenodo.org/records/10835692/files/EU_LCZ_map.tiff",
    crs = 3035,
    isave_map = isave_map,
    isave_full = isave_euro,
    full_name = "lcz_euro_map.tif",
    lang = lang
  )
}
