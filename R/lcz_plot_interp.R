#' Visualise interpolated LCZ maps
#'
#' Plots the rasters produced by [lcz_interp_map()] or [lcz_anomaly_map()].
#'
#' @param x A `SpatRaster` (single layer or stack).
#' @param palette A gradient palette from \pkg{tidyterra}
#'   (`scale_fill_whitebox_c`). Default `"muted"`.
#' @param direction Colour order: `1` (default) or `-1` (reversed).
#' @param isave Logical. Save the plot in `LCZ4r_output/`.
#' @param legend_name Legend title. `NULL` (default) uses a translated "Value".
#' @param save_extension Image format: `"png"`, `"jpg"`, `"tif"`, `"pdf"` or
#'   `"svg"`.
#' @param ... Passed to [ggplot2::labs()].
#' @param lang Language used for messages and errors. See [lcz_langs()].
#'   Defaults to `"en"`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_interp <- lcz_interp_map(lcz_map, df, var = "airT", station_id = "station")
#' lcz_plot_interp(my_interp)
#' }
#'
#' @seealso [lcz_interp_map()], [lcz_anomaly_map()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_plot_interp <- function(x,
                            palette = "muted",
                            direction = 1,
                            isave = FALSE,
                            save_extension = "png",
                            legend_name = NULL,
                            ...,
                            lang = "en") {
  if (is.null(x)) {
    lcz_abort("err_anomaly_input", lang = lang)
  }
  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast(x)
  }

  # terra::minmax() streams the raster; terra::values() would copy every cell.
  mm <- terra::minmax(x)
  color_limits <- c(floor(min(mm, na.rm = TRUE)), ceiling(max(mm, na.rm = TRUE)))

  if (is.null(legend_name)) legend_name <- lcz_tr("value", lang = lang)

  final_graph <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = x, na.rm = TRUE) +
    tidyterra::scale_fill_whitebox_c(
      palette = palette,
      direction = direction,
      limits = color_limits,
      na.value = "transparent"
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(
      title.position = "top", title.hjust = 0.5,
      barwidth = ggplot2::unit(70, "mm"),
      barheight = ggplot2::unit(3, "mm"),
      frame.colour = "grey70", ticks.colour = "grey40"
    )) +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    do.call(ggplot2::labs, c(list(...), list(fill = legend_name))) +
    lcz_theme_map(legend_position = "bottom")

  if (terra::nlyr(x) > 1L) {
    final_graph <- final_graph + ggplot2::facet_wrap(~lyr)
  }

  if (isTRUE(isave)) {
    lcz_save_figure(
      final_graph,
      filename = paste0("lcz_interp_map.", save_extension),
      width = "double", height = 150, lang = lang
    )
  }

  final_graph
}
