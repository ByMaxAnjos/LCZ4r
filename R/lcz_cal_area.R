#' Calculate LCZ areas
#'
#' Computes the area (km²) and the relative share (%) of each LCZ class, and
#' optionally draws a bar, pie or donut chart.
#'
#' @param x A `SpatRaster` (or path) with LCZ classes (1-17).
#' @param plot_type One of `"bar"` (default), `"pie"` or `"donut"`.
#' @param iplot Logical. Return the plot (`TRUE`) or the summary table
#'   (`FALSE`).
#' @param isave Logical. Save the plot and the summary table in
#'   `LCZ4r_output/`.
#' @param save_extension Image format: `"png"`, `"jpg"`, `"tif"`, `"pdf"` or
#'   `"svg"`.
#' @param show_legend Logical. Show the legend.
#' @param inclusive Logical. Use the colour-vision-deficiency-safe palette.
#' @param xlab,ylab Axis labels. Sensible translated defaults are used when
#'   `NULL`.
#' @param ... Passed to [ggplot2::labs()] (e.g. `title`, `subtitle`,
#'   `caption`).
#' @param lang Language used for labels, messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return A summary tibble (`iplot = FALSE`) or a `ggplot` object.
#'
#' @export
#'
#' @references
#' Stewart, I. D., and Oke, T. R. (2012). Local Climate Zones for urban
#' temperature studies. Bull. Amer. Meteor. Soc. 93(12) 1879-1900.
#' \doi{10.1175/BAMS-D-11-00019.1}
#'
#' @examples
#' \dontrun{
#' my_lcz_map <- lcz_get_map(city = "Juiz de Fora")
#' lcz_cal_area(my_lcz_map, plot_type = "bar")
#' lcz_cal_area(my_lcz_map, plot_type = "donut", lang = "pt")
#' lcz_cal_area(my_lcz_map, iplot = FALSE)
#' }
#'
#' @seealso [lcz_get_map()], [lcz_plot_map()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_cal_area <- function(x,
                         plot_type = c("bar", "pie", "donut"),
                         iplot = TRUE,
                         isave = FALSE,
                         save_extension = "png",
                         show_legend = TRUE,
                         inclusive = FALSE,
                         xlab = NULL,
                         ylab = NULL,
                         ...,
                         lang = "en") {
  plot_type <- match.arg(plot_type)
  x <- lcz_check_raster(x, lang = lang)

  if (is.null(xlab)) xlab <- lcz_tr("lcz_code", lang = lang)
  if (is.null(ylab)) ylab <- lcz_tr("area_km2", lang = lang)

  if (terra::nlyr(x) > 1L) x <- x[[1L]]
  names(x) <- "lcz"

  # Area per class: zonal sum of cell areas (no full-raster data.frame copy) ---
  area_raster <- terra::cellSize(x, unit = "km")
  area_tbl <- terra::zonal(area_raster, x, fun = "sum", na.rm = TRUE)
  names(area_tbl) <- c("lcz", "area_km2")

  freq_tbl <- terra::freq(x, bylayer = FALSE, usenames = TRUE)
  freq_tbl <- data.frame(lcz = freq_tbl$value, count = freq_tbl$count)

  # Class 0 in the global product means water (LCZ 17)
  area_tbl$lcz[area_tbl$lcz == 0] <- 17
  freq_tbl$lcz[freq_tbl$lcz == 0] <- 17

  area_tbl <- stats::aggregate(area_km2 ~ lcz, data = area_tbl, FUN = sum)
  freq_tbl <- stats::aggregate(count ~ lcz, data = freq_tbl, FUN = sum)

  summary_tbl <- merge(freq_tbl, area_tbl, by = "lcz", all = TRUE)
  summary_tbl$area_km2 <- round(summary_tbl$area_km2, 2)
  summary_tbl$area_perc <- round(
    summary_tbl$area_km2 / sum(summary_tbl$area_km2) * 100, 2
  )
  summary_tbl <- summary_tbl[order(summary_tbl$lcz), ]

  # Attach localised metadata ------------------------------------------------
  ids <- as.integer(summary_tbl$lcz)
  lcz_df <- tibble::tibble(
    lcz = factor(ids, levels = c(1:10, 11:17)),
    lcz.name = lcz_class_names(lang)[match(ids, c(1:10, 11:17))],
    lcz.col = unname(lcz_palette(inclusive = inclusive))[match(ids, c(1:10, 11:17))],
    count = summary_tbl$count,
    area_km2 = summary_tbl$area_km2,
    area_perc = summary_tbl$area_perc
  )

  if (!isTRUE(iplot)) {
    return(lcz_df)
  }

  color_values <- stats::setNames(lcz_df$lcz.col, lcz_df$lcz)
  lcz_labels <- lcz_df$lcz.name
  total_lab <- paste0("Total: ", round(sum(lcz_df$area_km2), 1), " km\u00b2")
  total_lab2 <- paste0("Total:\n", round(sum(lcz_df$area_km2), 1), " km\u00b2")

  if (plot_type == "bar") {
    graph <- ggplot2::ggplot(
      lcz_df,
      ggplot2::aes(x = .data$lcz, y = .data$area_km2, fill = .data$lcz)
    ) +
      ggplot2::geom_col(width = 0.85) +
      ggplot2::scale_fill_manual(
        values = color_values,
        name = lcz_tr("lcz_class", lang = lang),
        labels = lcz_labels,
        guide = ggplot2::guide_legend(
          title.position = "top",
          ncol = 1
        )
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.1f", .data$area_km2)),
        vjust = -0.4, size = 3.1
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.12)),
        labels = scales::label_comma()
      ) +
      ggplot2::labs(x = xlab, y = ylab, ...) +
      lcz_theme_series(legend_position = if (isTRUE(show_legend)) "right" else "none")
  } else {
    lcz_df$label <- paste0(round(lcz_df$area_perc, 1), "%\n")
    graph <- ggplot2::ggplot(
      lcz_df,
      ggplot2::aes(x = "", y = .data$area_perc, fill = .data$lcz)
    ) +
      ggplot2::geom_col(width = 1, colour = NA, linewidth = 0.2) +
      ggplot2::coord_polar("y") +
      ggplot2::scale_fill_manual(
        values = color_values,
        name = lcz_tr("lcz_class", lang = lang),
        labels = lcz_labels,
        guide = ggplot2::guide_legend(title.position = "top",
                                      ncol = 1)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$label),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 2.9, colour = "white", check_overlap = TRUE
      ) +
      ggplot2::labs(x = NULL, y = NULL, ...) +
      lcz_theme_map(legend_position = if (isTRUE(show_legend)) "right" else "none") +
      ggplot2::theme(axis.text = ggplot2::element_blank())

    if (plot_type == "pie") {
      graph <- graph +
        ggplot2::annotate(
          "text", x = 1, y = 1.8, label = total_lab,
          colour = "black", size = 5, fontface = "bold", vjust = 21
        )
    } else {
      graph <- graph +
        ggplot2::annotate(
          "text", x = -0.5, y = 0, label = total_lab2,
          colour = "black", size = 5, fontface = "bold", vjust = 0.5
        )
    }
  }

  if (isTRUE(isave)) {
    dir <- lcz_output_dir()
    ggplot2::ggsave(
      file.path(dir, paste0("lcz4r_area_", plot_type, ".", save_extension)),
      graph,
      width = if (plot_type == "bar") 190 else 170,
      height = if (plot_type == "bar") max(110, min(220, 7 * nrow(lcz_df) + 45)) else 150,
      units = "mm", dpi = 600
    )
    lcz_save_table(lcz_df, "lcz4r_area_df.csv", lang = lang)
  }

  graph
}
