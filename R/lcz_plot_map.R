#' Visualise an LCZ map
#'
#' Produces a publication-ready map of LCZ classes with the official colour
#' scheme (or a colour-vision-deficiency-safe alternative).
#'
#' @param x A `SpatRaster` (or path) with LCZ classes (1-17).
#' @param isave Logical. Save the plot in `LCZ4r_output/`.
#' @param show_legend Logical. Show the LCZ legend.
#' @param save_extension Image format: `"png"`, `"jpg"`, `"tif"`, `"pdf"` or
#'   `"svg"`.
#' @param inclusive Logical. Use the colour-vision-deficiency-safe palette.
#' @param max_pixels Maximum number of cells drawn. Larger rasters are
#'   aggregated with a modal filter first, which keeps memory bounded and the
#'   plot legible at print resolution.
#' @param legend_ncol Number of legend columns. Default `1`, which keeps the map
#'   panel as large as possible; use `2` for a shorter, wider legend.
#' @param scalebar Logical. Add a scale bar computed from the raster extent.
#' @param north_arrow Logical. Add a north arrow.
#' @param ... Passed to [ggplot2::labs()] (`title`, `subtitle`, `caption`).
#' @param lang Language used for labels, messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_lcz_map <- lcz_get_map(city = "Juiz de Fora")
#' lcz_plot_map(my_lcz_map, title = "LCZ - Juiz de Fora")
#' lcz_plot_map(my_lcz_map, inclusive = TRUE, lang = "pt")
#' }
#'
#' @seealso [lcz_get_map()], [lcz_cal_area()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_plot_map <- function(x,
                         isave = FALSE,
                         show_legend = TRUE,
                         save_extension = "png",
                         inclusive = FALSE,
                         max_pixels = 1e6,
                         legend_ncol = 1,
                         scalebar = FALSE,
                         north_arrow = FALSE,
                         ...,
                         lang = "en") {
  x <- lcz_check_raster(x, lang = lang)
  names(x) <- "class"

  # Class 0 in the global product is water (LCZ 17).
  x <- terra::subst(x, from = 0, to = 17)

  # Keep plotting fast and memory-bounded on national/continental rasters.
  if (!is.null(max_pixels) && terra::ncell(x) > max_pixels) {
    fact <- ceiling(sqrt(terra::ncell(x) / max_pixels))
    x <- terra::aggregate(x, fact = fact, fun = "modal", na.rm = TRUE)
  }

  ids <- c(1:10, 11:17)
  present <- sort(unique(stats::na.omit(terra::unique(x)[[1]])))
  present <- present[present %in% ids]
  names_all <- lcz_class_names(lang)
  cols_all <- lcz_palette(inclusive = inclusive)

  color_values <- stats::setNames(unname(cols_all[ids]), ids)
  lcz_labels <- stats::setNames(names_all, ids)

  data_plot <- terra::as.data.frame(x, xy = TRUE, na.rm = TRUE)
  names(data_plot)[3] <- "class"
  data_plot$class <- factor(data_plot$class, levels = ids)

  my_plot <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = data_plot,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$class)
    ) +
    ggplot2::scale_fill_manual(
      values = color_values,
      breaks = present,
      labels = unname(lcz_labels[as.character(present)]),
      name = lcz_tr("lcz_class", lang = lang),
      na.value = "transparent",
      guide = ggplot2::guide_legend(
        title.position = "top", reverse = FALSE,
        ncol = legend_ncol
      )
    ) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(...) +
    lcz_theme_map()

  if (isTRUE(scalebar)) {
    my_plot <- lcz_add_scalebar(my_plot, x)
  }
  if (isTRUE(north_arrow)) {
    my_plot <- lcz_add_north(my_plot, x)
  }

  if (!isTRUE(show_legend)) {
    my_plot <- my_plot + ggplot2::theme(legend.position = "none")
  }

  if (isTRUE(isave)) {
    lcz_save_figure(
      my_plot,
      filename = paste0("lcz_plot_map.", save_extension),
      width = "double", height = 150, lang = lang
    )
  }

  my_plot
}
