test_that("lcz_plot_map() returns a ggplot with a localised legend", {
  r <- make_lcz_raster()
  p <- lcz_plot_map(r, lang = "pt", title = "Mapa LCZ")
  expect_s3_class(p, "ggplot")

  labels <- ggplot2::get_labs(p)
  expect_identical(labels$title, "Mapa LCZ")
})

test_that("lcz_plot_map() honours inclusive palette and legend switch", {
  r <- make_lcz_raster()
  p_all <- lcz_plot_map(r, inclusive = TRUE)
  p_no <- lcz_plot_map(r, show_legend = FALSE)
  expect_s3_class(p_all, "ggplot")
  expect_identical(p_no$theme$legend.position, "none")
})

test_that("lcz_plot_map() downsamples large rasters", {
  r <- make_lcz_raster(nrows = 400, ncols = 400)
  p <- lcz_plot_map(r, max_pixels = 1000)
  # The layer behind geom_raster must hold at most ~1000 cells
  build <- ggplot2::ggplot_build(p)
  expect_lte(nrow(build$data[[1]]), 2000)
})

test_that("lcz_plot_parameters() handles iselect and validates input", {
  r <- make_lcz_raster()
  stack <- lcz_get_parameters(r)

  p <- lcz_plot_parameters(stack, iselect = "SVFmean", lang = "es")
  expect_s3_class(p, "ggplot")
  expect_identical(ggplot2::get_labs(p)$title, .lcz_param_label("SVFmean", "es"))

  expect_error(lcz_plot_parameters(r), "lcz_get_parameters")
  expect_error(lcz_plot_parameters(stack, iselect = "nope"), "iselect")
})

test_that("lcz_plot_interp() plots a single and a multi-layer raster", {
  r <- make_lcz_raster()
  p1 <- lcz_plot_interp(r)
  expect_s3_class(p1, "ggplot")

  multi <- c(r, r)
  names(multi) <- c("a", "b")
  p2 <- lcz_plot_interp(multi)
  expect_s3_class(p2, "ggplot")
})

test_that("journal figure system has consistent sizes and palettes", {
  th <- lcz_theme_journal()
  expect_s3_class(th, "theme")

  expect_length(lcz_pal_diverging(11), 11L)
  expect_length(lcz_pal_categorical(5), 5L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", lcz_pal_categorical(8))))

  expect_identical(lcz_date_labels(as.POSIXct("2020-01-01") + 0:5 * 3600), "%H:%M")
  expect_identical(
    lcz_date_labels(seq(as.POSIXct("2020-01-01"), by = "day", length.out = 20)),
    "%d %b"
  )
})

test_that("lcz_save_figure() writes a journal-sized file", {
  skip_if_not_installed("ragg")
  r <- make_lcz_raster()
  p <- lcz_plot_map(r)
  dir <- tempfile("lczfig")
  dir.create(dir)
  path <- lcz_save_figure(p, "test.png", width = "single", height = 80,
                          folder = dir, quiet = TRUE)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 1000)
  unlink(dir, recursive = TRUE)
})

test_that("lcz_plot_map() accepts cartographic furniture", {
  r <- make_lcz_raster()
  p <- lcz_plot_map(r, scalebar = TRUE, north_arrow = TRUE)
  expect_s3_class(p, "ggplot")
})
