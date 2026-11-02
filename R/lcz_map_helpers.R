# Shared download / clip pipeline for the LCZ map getters ---------------------
#
# lcz_get_map(), lcz_get_map_euro() and lcz_get_map_usa() were three copies of
# the same 150-line function (the only differences being the Zenodo URL and the
# CRS). They now delegate here, which also removes the old
# `options(warn = -1)` call that permanently silenced warnings for the session.

# Resolve a city name or ROI polygon to a validated sf object.
lcz_boundary <- function(city = NULL, roi = NULL, crs = 4326, lang = NULL) {
  if (is.null(city) && is.null(roi)) {
    lcz_abort("err_city_roi", lang = lang)
  }

  if (!is.null(city)) {
    boundary <- try(
      osmdata::getbb(city, format_out = "sf_polygon", limit = 1),
      silent = TRUE
    )
    if (inherits(boundary, "try-error") || is.null(boundary) ||
        (inherits(boundary, "data.frame") && nrow(boundary) == 0L)) {
      lcz_abort("err_no_boundary", lang = lang, city = city)
    }
    boundary <- sf::st_as_sf(boundary)
  } else {
    boundary <- sf::st_as_sf(roi)
  }

  boundary <- sf::st_make_valid(boundary)
  if (is.na(sf::st_crs(boundary))) {
    sf::st_crs(boundary) <- 4326
  }
  sf::st_transform(boundary, crs)
}

# Open a remote COG through GDAL's /vsicurl/ driver with a readable error.
lcz_remote_raster <- function(url, lang = NULL) {
  r <- try(terra::rast(paste0("/vsicurl/", url)), silent = TRUE)
  if (inherits(r, "try-error") || is.null(r)) {
    lcz_abort("err_download", lang = lang, url = url)
  }
  r
}

# Download, clip and optionally persist a global/continental LCZ product.
lcz_clip_lcz_map <- function(city = NULL, roi = NULL, url,
                             crs = 4326, isave_map = FALSE,
                             isave_full = FALSE, full_name = "lcz_global_map.tif",
                             lang = NULL) {
  area <- lcz_boundary(city = city, roi = roi, crs = crs, lang = lang)
  global <- lcz_remote_raster(url, lang = lang)

  out <- terra::crop(global, terra::ext(area), snap = "out")
  out <- terra::mask(out, terra::vect(area))
  names(out) <- "lcz"

  if (isTRUE(isave_map)) {
    dir <- lcz_output_dir()
    terra::writeRaster(out, file.path(dir, "lcz_map.tif"), overwrite = TRUE)
    lcz_msg("msg_files_saved", lang = lang, path = dir)
  }

  if (isTRUE(isave_full)) {
    dir <- lcz_output_dir()
    terra::writeRaster(global, file.path(dir, full_name), overwrite = TRUE)
    lcz_msg("msg_files_saved", lang = lang, path = dir)
  }

  out
}
