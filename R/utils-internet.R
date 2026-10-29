# Open a remote raster through GDAL's /vsicurl/ driver. Returns NULL (with an
# informative message) instead of an error when the resource cannot be reached,
# so that callers can fail gracefully as required by CRAN policy.
lcz_read_remote <- function(url) {
  base::tryCatch(
    terra::rast(base::paste0("/vsicurl/", url)),
    error = function(e) {
      base::message("Could not access ", url, ": ", base::conditionMessage(e))
      NULL
    }
  )
}

# Query the OpenStreetMap Nominatim service for a city boundary. Returns NULL
# (with an informative message) if the service cannot be reached.
lcz_get_osm_boundary <- function(city) {
  base::tryCatch(
    osmdata::getbb(city, format_out = "sf_polygon", limit = 1),
    error = function(e) {
      base::message("Could not access OpenStreetMap Nominatim: ", base::conditionMessage(e))
      NULL
    }
  )
}
