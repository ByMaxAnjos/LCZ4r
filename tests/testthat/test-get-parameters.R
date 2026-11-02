test_that("lcz_get_parameters() returns the requested layers", {
  r <- make_lcz_raster()

  stack <- lcz_get_parameters(r)
  expect_s4_class(stack, "SpatRaster")
  expect_equal(terra::nlyr(stack), 34L)
  expect_true(all(c("SVFmin", "SVFmean", "AHmean", "z0") %in% names(stack)))

  # Mean is exactly (min + max) / 2 for every cell
  min_r <- terra::values(stack[["SVFmin"]], mat = FALSE)
  max_r <- terra::values(stack[["SVFmax"]], mat = FALSE)
  mean_r <- terra::values(stack[["SVFmean"]], mat = FALSE)
  expect_equal(mean_r, (min_r + max_r) / 2)

  # iselect must work independently of istack (regression test)
  sel <- lcz_get_parameters(r, iselect = c("SVFmean", "AHmean"))
  expect_equal(terra::nlyr(sel), 2L)
  expect_identical(names(sel), c("SVFmean", "AHmean"))

  one <- lcz_get_parameters(r, iselect = "SVFmean")
  expect_equal(terra::nlyr(one), 1L)
})

test_that("lcz_get_parameters() validates iselect and returns lists", {
  r <- make_lcz_raster()
  expect_error(
    lcz_get_parameters(r, iselect = "not_a_parameter", lang = "pt"),
    "iselect"
  )

  lst <- lcz_get_parameters(r, istack = FALSE)
  expect_type(lst, "list")
  expect_length(lst, 34L)
  expect_s4_class(lst[[1]], "SpatRaster")
})

test_that("lcz_get_parameters() can return polygons", {
  r <- make_lcz_raster()
  shp <- lcz_get_parameters(r, ishp = TRUE)
  expect_s3_class(shp, "sf")
  expect_true("SVFmean" %in% names(shp))
  expect_equal(nrow(shp), 4L)  # dissolve = TRUE, one row per class
})

test_that("lcz_get_parameters() leaves z0 correct for compact mid-rise", {
  r <- make_lcz_raster(classes = c(2))
  stack <- lcz_get_parameters(r, iselect = "z0")
  expect_equal(unique(terra::values(stack, mat = FALSE)), 1)
})
