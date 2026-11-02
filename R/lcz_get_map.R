#' Download the LCZ map
#'
#' Retrieves the global Local Climate Zone (LCZ) mapping dataset (Demuzere et
#' al., 2022) for a city name or a custom region of interest.
#'
#' @param city A character string with the name of the target area, as used by
#'   OpenStreetMap/Nominatim.
#' @param roi Optional `sf`/`sfc` polygon used to clip the LCZ map to a custom
#'   area. Ignored when `city` is supplied.
#' @param isave_map Logical. Save the clipped map as a GeoTIFF in
#'   `LCZ4r_output/`.
#' @param isave_global Logical. Save the full global LCZ map as a GeoTIFF.
#' @param lang Language used for console messages and errors. One of
#'   `"en"`, `"pt"`, `"es"`, `"fr"`, `"zh"`, `"de"`, `"it"`, `"ja"`, `"ko"`,
#'   `"ru"`, `"ar"`. Defaults to `"en"`.
#'
#' @return A `SpatRaster` with LCZ classes (1-17).
#'
#' @export
#'
#' @references
#' Demuzere, M., Kittner, J., Martilli, A., Mills, G., Moede, C., Stewart, I.
#' D., van Vliet, J., and Bechtel, B. (2022). A global map of Local Climate
#' Zones to support earth system modelling and urban scale environmental
#' science. Earth Syst. Sci. Data 14(8) 3835-3873.
#' \doi{10.5194/essd-14-3835-2022}
#'
#' Stewart, I. D., and Oke, T. R. (2012). Local Climate Zones for urban
#' temperature studies. Bull. Amer. Meteor. Soc. 93(12) 1879-1900.
#' \doi{10.1175/BAMS-D-11-00019.1}
#'
#' @examples
#' \dontrun{
#' # Load the LCZ map for a city
#' my_lcz_city <- lcz_get_map(city = "Juiz de Fora")
#'
#' # Get the LCZ map for a custom region of interest
#' custom_roi <- sf::st_read("custom_roi.shp")
#' roi_lcz <- lcz_get_map(roi = custom_roi, isave_map = TRUE)
#'
#' # Retrieve the LCZ map for a country
#' my_lcz_country <- lcz_get_map(city = "Brazil")
#' }
#'
#' @seealso [lcz_get_map_euro()], [lcz_get_map_usa()], [lcz_plot_map()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_get_map <- function(city = NULL, roi = NULL, isave_map = FALSE,
                        isave_global = FALSE, lang = "en") {
  lcz_clip_lcz_map(
    city = city, roi = roi,
    url = "https://zenodo.org/records/8419340/files/lcz_filter_v3.tif",
    crs = 4326,
    isave_map = isave_map,
    isave_full = isave_global,
    full_name = "lcz_global_map.tif",
    lang = lang
  )
}
