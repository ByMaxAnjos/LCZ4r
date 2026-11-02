#' Clip a local LCZ map to a city or region of interest
#'
#' Use this function when you already have the global (or continental) LCZ
#' GeoTIFF on disk, e.g. after a manual download from Zenodo.
#'
#' @param x A `SpatRaster` (or path to a raster) with LCZ classes.
#' @param city A character string with the name of the target area, as used by
#'   OpenStreetMap/Nominatim.
#' @param roi Optional `sf`/`sfc` polygon used to clip the map.
#' @param isave_map Logical. Save the clipped map as a GeoTIFF.
#' @param lang Language used for console messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return A `SpatRaster` clipped to the requested area.
#'
#' @export
#'
#' @references
#' Demuzere, M., Kittner, J., Martilli, A., Mills, G., Moede, C., Stewart, I.
#' D., van Vliet, J., and Bechtel, B. (2022). A global map of Local Climate
#' Zones. Earth Syst. Sci. Data 14(8) 3835-3873.
#' \doi{10.5194/essd-14-3835-2022}
#'
#' @examples
#' \dontrun{
#' my_map <- terra::rast("lcz_filter_v3.tif")
#' my_lcz_country <- lcz_get_map2(my_map, city = "Lisbon")
#' }
#'
#' @seealso [lcz_get_map()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_get_map2 <- function(x, city = NULL, roi = NULL, isave_map = FALSE,
                         lang = "en") {
  x <- lcz_check_raster(x, lang = lang)

  area <- lcz_boundary(city = city, roi = roi, crs = 4326, lang = lang)

  out <- terra::crop(x, terra::ext(area), snap = "out")
  out <- terra::mask(out, terra::vect(area))
  names(out) <- "lcz"

  if (isTRUE(isave_map)) {
    dir <- lcz_output_dir()
    terra::writeRaster(out, file.path(dir, "lcz_map.tif"), overwrite = TRUE)
    lcz_msg("msg_files_saved", lang = lang, path = dir)
  }

  out
}
