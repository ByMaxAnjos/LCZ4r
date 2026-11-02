# Shared internal utilities ----------------------------------------------------
#
# These helpers exist to remove ~1,500 lines of duplicated logic spread across
# the exported functions and to give every plot the same visual grammar.

# --- input validation ---------------------------------------------------------

# Validate/coerce a raster input. Mirrors the historical behaviour (coerce with
# a warning, keep the first layer, reproject to WGS84) but in one place.
lcz_check_raster <- function(x, lang = NULL, arg = "x",
                             require_wgs84 = TRUE, layer = "auto") {
  if (is.null(x)) {
    lcz_abort("err_raster", lang = lang)
  }
  if (is.character(x) && length(x) == 1L && is.na(x)) {
    lcz_abort("err_raster", lang = lang)
  }
  if (!inherits(x, "SpatRaster")) {
    lcz_warn("warn_coerce_raster", lang = lang)
    x <- try(terra::rast(x), silent = TRUE)
    if (inherits(x, "try-error")) {
      lcz_abort("err_coerce_raster", lang = lang)
    }
  }
  if (terra::nlyr(x) > 1L && !is.null(layer)) {
    if (identical(layer, "auto")) {
      nm <- tolower(names(x))
      hit <- which(nm %in% c("lcz", "lcz_filter", "lczfilter", "class"))
      x <- x[[if (length(hit)) hit[1L] else 1L]]
    } else {
      x <- x[[layer]]
    }
  }
  if (require_wgs84) {
    is_wgs84 <- isTRUE(terra::same.crs(x, "EPSG:4326"))
    if (!is_wgs84) {
      lcz_warn("warn_reproject", lang = lang)
      x <- terra::project(x, "EPSG:4326")
    }
  }
  x
}

# Validate the tabular input shared by lcz_ts(), lcz_uhi_intensity(),
# lcz_anomaly() and the interpolation functions.
lcz_check_data_frame <- function(data_frame, var, station_id, lang = NULL) {
  if (!is.data.frame(data_frame)) {
    lcz_abort("err_df", lang = lang)
  }
  if (missing(var) || !is.character(var) || length(var) != 1L || !var %in% names(data_frame)) {
    lcz_abort("err_col", lang = lang, arg = "var")
  }
  if (missing(station_id) || !is.character(station_id) || length(station_id) != 1L ||
      !station_id %in% names(data_frame)) {
    lcz_abort("err_col", lang = lang, arg = "station_id")
  }

  # Coordinates are required by every local function; validating them here
  # avoids cryptic "replacement has 0 rows" errors during dplyr::rename().
  lat <- grep("(?i)^(lat|latitude)$", names(data_frame), value = TRUE, perl = TRUE)
  lon <- grep("(?i)^(lon|long|longitude)$", names(data_frame), value = TRUE, perl = TRUE)
  if (length(lat) != 1L) {
    lcz_abort("err_col", lang = lang, arg = "latitude")
  }
  if (length(lon) != 1L) {
    lcz_abort("err_col", lang = lang, arg = "longitude")
  }
  invisible(TRUE)
}

# --- output helpers -----------------------------------------------------------

# Create (if needed) and return the standard output directory.
lcz_output_dir <- function(folder = "LCZ4r_output") {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(folder, winslash = "/", mustWork = FALSE)
}

# Save a data frame as CSV and report the location once.
lcz_save_table <- function(data, filename, folder = "LCZ4r_output", lang = NULL) {
  dir <- lcz_output_dir(folder)
  path <- file.path(dir, filename)
  utils::write.csv(data, path, row.names = FALSE)
  lcz_msg("msg_results_saved", lang = lang, path = dir)
  invisible(path)
}

# --- geography ----------------------------------------------------------------

# Hemisphere from a raster extent; terra-only (no raster:: dependency).
lcz_hemisphere <- function(x) {
  e <- terra::ext(x)
  if (as.numeric(e$ymin) >= 0) "northern" else "southern"
}

# --- LCZ extraction -----------------------------------------------------------

# Assign an LCZ class to each station. Replaces three near-identical blocks
# (`simple`, `bilinear`, `two.step`) duplicated across the package.
#
# For `two.step`, homogeneity is computed exactly on a k x k window using cell
# arithmetic instead of an R callback inside terra::focal(), which is orders of
# magnitude faster and gives identical results.
lcz_assign_lcz <- function(x, points, method = "simple",
                           kernel_size = 5L, threshold = 0.8, lang = NULL) {
  method <- match.arg(method, c("simple", "bilinear", "two.step"))
  pts <- if (inherits(points, "sf")) sf::st_as_sf(points) else sf::st_as_sf(points)
  if (is.na(sf::st_crs(pts))) sf::st_crs(pts) <- 4326

  if (method == "two.step") {
    hom <- lcz_homogeneity(x, pts, kernel_size = kernel_size)
    pts <- pts[!is.na(hom) & hom >= threshold, , drop = FALSE]
    if (nrow(pts) == 0L) {
      lcz_abort("err_no_urban_rural", lang = lang)
    }
  }

  vals <- terra::extract(
    x, terra::vect(pts),
    method = if (identical(method, "bilinear")) "bilinear" else "simple"
  )
  vals$ID <- NULL
  pts$lcz <- vals[[1]]
  if (identical(method, "bilinear")) {
    pts$lcz <- as.integer(round(pts$lcz))
  }
  pts <- pts[!is.na(pts$lcz), , drop = FALSE]
  pts
}

# Fraction of cells in a k x k window that match the centre cell, evaluated
# only at the supplied points. Returns one value per point (NA when the centre
# cell is NA). Uses coordinate arithmetic + a single terra::extract() call, so
# memory stays proportional to n_points * k^2 instead of the whole raster.
lcz_homogeneity <- function(x, points, kernel_size = 5L) {
  stopifnot(kernel_size %% 2L == 1L)
  half <- (kernel_size - 1L) / 2L
  res <- terra::res(x)
  xy <- sf::st_coordinates(points)[, 1:2, drop = FALSE]
  off <- expand.grid(dc = -half:half, dr = -half:half)
  n <- nrow(xy)
  k <- nrow(off)
  idx <- rep(seq_len(n), each = k)
  coords <- cbind(
    x = xy[idx, 1L] + rep(off$dc, times = n) * res[1L],
    y = xy[idx, 2L] + rep(off$dr, times = n) * res[2L]
  )
  vals <- terra::extract(x, coords)
  m <- matrix(vals[[1L]], nrow = n, ncol = k, byrow = TRUE)
  centre <- m[, (k + 1L) / 2L]
  frac <- rowMeans(m == centre, na.rm = TRUE)
  frac[is.na(centre)] <- NA_real_
  frac
}

# Standard caption/labels helper (keeps strings in sync across plots).
lcz_caption <- function(lang = NULL) lcz_tr("caption", lang = lang)

# --- geostatistics ------------------------------------------------------------

# Robust variogram fitting.
#
# automap::autofitVariogram() aborts with "bounds must be strictly increasing"
# (or "missing values in object") whenever a time step has too few stations per
# LCZ class -- a common situation for small networks and hourly data. Rather
# than losing the whole run, we degrade gracefully:
#   1. try the requested formula (with LCZ as trend);
#   2. try an ordinary variogram (~ 1);
#   3. fall back to a spherical variogram with an empirical range.
# The returned object always exposes `$var_model`, so call sites do not change.
lcz_fit_variogram <- function(formula, data, model = "Sph", lang = NULL) {
  fit <- try(automap::autofitVariogram(formula, data, model = model),
             silent = TRUE)
  if (!inherits(fit, "try-error")) {
    return(fit)
  }

  fit <- try(
    automap::autofitVariogram(stats::as.formula(paste(all.vars(formula)[1], "~ 1")),
                              data, model = model),
    silent = TRUE
  )
  if (!inherits(fit, "try-error")) {
    lcz_warn("warn_variogram_fallback", lang = lang)
    return(fit)
  }

  y <- try(data[[all.vars(formula)[1]]], silent = TRUE)
  psill <- if (inherits(y, "try-error")) 1 else stats::var(y, na.rm = TRUE)
  if (!is.finite(psill) || psill <= 0) psill <- 1

  xy <- try(sf::st_coordinates(data), silent = TRUE)
  rng <- if (!inherits(xy, "try-error") && nrow(xy) > 1L) {
    max(stats::dist(xy), na.rm = TRUE) / 3
  } else {
    1
  }

  lcz_warn("warn_variogram_fallback", lang = lang)
  list(var_model = gstat::vgm(psill = psill, model = model,
                              range = rng, nugget = 0))
}

# Collapse multiple stations to one value per timestamp and group.
#
# `openair::timeAverage()` warns when a group has duplicate dates, because it
# expects one row per site. For UHI we deliberately average stations within
# each reference group first, so the time aggregation sees one row per group.
lcz_collapse_stations <- function(data, by = NULL) {
  keys <- intersect(c("date", "reference", by), names(data))
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      var_interp = base::mean(.data$var_interp, na.rm = TRUE),
      .groups = "drop"
    )
}
