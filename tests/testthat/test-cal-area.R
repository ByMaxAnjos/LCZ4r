test_that("lcz_cal_area() returns a coherent summary", {
  r <- make_lcz_raster()
  tbl <- lcz_cal_area(r, iplot = FALSE)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 4L)
  expect_true(all(c("lcz", "lcz.name", "lcz.col", "count", "area_km2", "area_perc") %in% names(tbl)))
  expect_equal(sum(tbl$area_perc), 100, tolerance = 1e-6)
  expect_equal(sum(tbl$count), terra::ncell(r))
  expect_false(anyNA(tbl$area_km2))

  # The area is computed from cell size, not from a partial-match column
  expect_equal(sum(tbl$area_km2),
               sum(terra::values(terra::cellSize(r, unit = "km"))),
               tolerance = 0.1)
})

test_that("lcz_cal_area() localises class names", {
  r <- make_lcz_raster()
  en <- lcz_cal_area(r, iplot = FALSE, lang = "en")
  pt <- lcz_cal_area(r, iplot = FALSE, lang = "pt")
  expect_false(identical(en$lcz.name, pt$lcz.name))
  expect_identical(pt$lcz.name[1], lcz_tr("lcz_name_2", lang = "pt"))
})

test_that("lcz_cal_area() builds all plot types", {
  r <- make_lcz_raster()
  for (type in c("bar", "pie", "donut")) {
    p <- lcz_cal_area(r, plot_type = type)
    expect_s3_class(p, "ggplot")
  }
})

test_that("lcz_cal_area() recodes class 0 as water", {
  r <- make_lcz_raster(classes = c(0))
  tbl <- lcz_cal_area(r, iplot = FALSE)
  expect_equal(as.integer(as.character(tbl$lcz)), 17L)
})
