make_zonal_raster <- function() {
  r <- terra::rast(
    nrows = 9, ncols = 9,
    xmin = -43.4, xmax = -43.3, ymin = -21.8, ymax = -21.7,
    crs = "EPSG:4326"
  )
  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  terra::values(r) <- ifelse(xy[, 1] < -43.36, 2,
                             ifelse(xy[, 1] < -43.33, 6, 14))
  names(r) <- "lcz"
  r
}

make_zonal_df <- function(n_hours = 12) {
  stations <- data.frame(
    station = c("urban1", "urban2", "rural1"),
    lat = -21.75,
    lon = c(-43.38, -43.35, -43.31),
    stringsAsFactors = FALSE
  )
  out <- stations[rep(seq_len(nrow(stations)), each = n_hours), ]
  set.seed(7)
  out$airT <- stats::rnorm(nrow(out), 25, 2)
  out$date <- rep(
    seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour", length.out = n_hours),
    times = nrow(stations)
  )
  out
}

test_that("lcz_homogeneity() computes the exact k x k fraction", {
  r <- make_lcz_raster(nrows = 5, ncols = 5, classes = 2)
  pts <- sf::st_as_sf(
    data.frame(lon = -43.35, lat = -21.75),
    coords = c("lon", "lat"), crs = 4326
  )
  expect_equal(lcz_homogeneity(r, pts, kernel_size = 5), 1)

  # Flip the centre cell: only the centre itself matches now (1 of 25)
  centre <- terra::cellFromXY(r, sf::st_coordinates(pts))
  vals <- terra::values(r, mat = FALSE)
  vals[centre] <- 6
  terra::values(r) <- vals
  expect_equal(lcz_homogeneity(r, pts, kernel_size = 5), 1 / 25)

  # Flip one neighbour instead: 24 of 25 cells still match
  vals[centre] <- 2
  vals[centre + 1L] <- 6
  terra::values(r) <- vals
  expect_equal(lcz_homogeneity(r, pts, kernel_size = 5), 24 / 25)
})

test_that("lcz_ts() works in both plot and table modes", {
  r <- make_zonal_raster()
  df <- make_zonal_df()

  p <- lcz_ts(r, df, var = "airT", station_id = "station", lang = "pt")
  expect_s3_class(p, "ggplot")

  tbl <- lcz_ts(r, df, var = "airT", station_id = "station", iplot = FALSE)
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("date", "station", "lcz", "var_interp") %in% names(tbl)))
})

test_that("lcz_ts() validates inputs in the requested language", {
  r <- make_zonal_raster()
  df <- make_zonal_df()
  expect_error(
    lcz_ts(r, df, var = "missing", station_id = "station", lang = "pt"),
    "coluna"
  )
  expect_error(
    lcz_ts(r, data.frame(x = 1), var = "x", station_id = "x", lang = "es"),
    "latitude"
  )
})

test_that("lcz_uhi_intensity() separates urban and rural references", {
  r <- make_zonal_raster()
  df <- make_zonal_df()

  out <- lcz_uhi_intensity(
    r, df, var = "airT", station_id = "station", iplot = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_true(all(c("urban", "rural", "uhi") %in% names(out)))
  expect_equal(out$uhi, round(out$urban - out$rural, 2))
})

test_that("lcz_uhi_intensity() validates the manual method", {
  r <- make_zonal_raster()
  df <- make_zonal_df()
  expect_error(
    lcz_uhi_intensity(r, df, var = "airT", station_id = "station",
                      method = "manual"),
    "Turban"
  )
})

test_that("lcz_anomaly() returns a plot and a table", {
  r <- make_zonal_raster()
  df <- make_zonal_df()

  p <- lcz_anomaly(r, df, var = "airT", station_id = "station", lang = "pt")
  expect_s3_class(p, "ggplot")

  tbl <- lcz_anomaly(r, df, var = "airT", station_id = "station", iplot = FALSE)
  expect_s3_class(tbl, "data.frame")
  expect_true("anomaly" %in% names(tbl))
})

test_that("lcz_check_raster() selects the LCZ layer in a named stack", {
  r <- make_zonal_raster()
  stack <- c(r, r * 0)
  names(stack) <- c("SVFmin", "lcz")
  out <- lcz_check_raster(stack)
  expect_identical(names(out), "lcz")
})

test_that("lcz_uhi_intensity() reports stations outside the map", {
  r <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 0.1, ymin = 0, ymax = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 2
  names(r) <- "lcz"
  df <- make_zonal_df()
  expect_error(
    lcz_uhi_intensity(r, df, var = "airT", station_id = "station", lang = "en"),
    "No station could be assigned"
  )
})

test_that("lcz_uhi_intensity() diagnoses missing rural stations", {
  r <- make_zonal_raster()
  terra::values(r) <- 2  # urban only
  df <- make_zonal_df()
  expect_warning(
    expect_error(
      lcz_uhi_intensity(r, df, var = "airT", station_id = "station", lang = "en"),
      "urban"
    ),
    "LCZ classes found"
  )
})

test_that("lcz_uhi_intensity() aggregates stations before time averaging", {
  r <- make_zonal_raster()
  df <- make_zonal_df()
  expect_no_warning(
    lcz_uhi_intensity(r, df, var = "airT", station_id = "station", group = TRUE)
  )
})
