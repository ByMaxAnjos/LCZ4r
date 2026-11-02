# Shared fixtures -------------------------------------------------------------

make_lcz_raster <- function(nrows = 10, ncols = 10, classes = c(2, 6, 11, 14)) {
  r <- terra::rast(
    nrows = nrows, ncols = ncols,
    xmin = -43.4, xmax = -43.3, ymin = -21.8, ymax = -21.7,
    crs = "EPSG:4326"
  )
  terra::values(r) <- rep(classes, length.out = terra::ncell(r))
  names(r) <- "lcz"
  r
}

make_station_df <- function(n_hours = 24, stations = c("A", "B", "C", "D")) {
  n <- n_hours * length(stations)
  set.seed(42)
  data.frame(
    station = rep(stations, each = n_hours),
    airT = stats::rnorm(n, 25, 3),
    lat = rep(seq(-21.78, -21.72, length.out = length(stations)), each = n_hours),
    lon = rep(seq(-43.38, -43.32, length.out = length(stations)), each = n_hours),
    date = rep(
      seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "hour",
          length.out = n_hours),
      times = length(stations)
    ),
    stringsAsFactors = FALSE
  )
}
