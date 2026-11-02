# Journal-grade figure system ------------------------------------------------
#
# Design targets (Nature/Science figure guidelines):
#   * final widths of 89 mm (single column) or 183 mm (double column);
#   * sans-serif text, 5-7 pt at final size (base_size = 8 here);
#   * no chartjunk: no top/right spines, no vertical grid in time series;
#   * colour-blind-safe palettes, consistent semantic mapping;
#   * vector output (PDF/SVG) or 600 dpi raster, fonts embedded.
#
# All plotting functions in the package should build on these helpers instead
# of ad-hoc `theme()` calls.

.lcz_journal <- list(
  width_single_mm = 120,
  width_double_mm = 190,
  height_max_mm = 247,
  dpi = 600,
  base_size = 11
)

#' Journal-style ggplot theme
#'
#' A minimal theme sized for final publication (Nature/Science column widths):
#' left-aligned titles, no top/right spines, light horizontal grid, compact
#' legend and 5-7 pt equivalent text.
#'
#' @param base_size Base font size in points at final figure size.
#' @param base_family Font family. `"sans"` maps to Helvetica/Arial.
#' @param grid Grid lines: `"y"` (default), `"both"` or `"none"`.
#' @param legend Legend position: `"right"`, `"top"`, `"bottom"` or `"none"`.
#' @param border Draw a thin panel border (default `FALSE`).
#'
#' @return A `ggplot2` theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   lcz_theme_journal()
lcz_theme_journal <- function(base_size = 11, base_family = "sans",
                              grid = c("y", "both", "none"),
                              legend = c("right", "top", "bottom", "none"),
                              border = FALSE) {
  grid <- match.arg(grid)
  legend <- match.arg(legend)

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "grey10"),
      plot.title = ggplot2::element_text(
        size = base_size * 1.2, face = "bold", hjust = 0,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 1.02, hjust = 0, margin = ggplot2::margin(b = 7)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.8, colour = "grey40", hjust = 1,
        margin = ggplot2::margin(t = 7)
      ),
      plot.tag = ggplot2::element_text(
        size = base_size * 1.1, face = "bold"
      ),
      axis.title = ggplot2::element_text(size = base_size, colour = "grey15"),
      axis.text = ggplot2::element_text(size = base_size * 0.95, colour = "grey20"),
      axis.ticks = ggplot2::element_line(colour = "grey35", linewidth = 0.25),
      axis.ticks.length = ggplot2::unit(1.6, "pt"),
      axis.ticks.x = ggplot2::element_line(colour = "grey35", linewidth = 0.25),
      axis.ticks.y = ggplot2::element_line(colour = "grey35", linewidth = 0.25),
      panel.grid.major = if (identical(grid, "none")) {
        ggplot2::element_blank()
      } else if (identical(grid, "y")) {
        ggplot2::element_line(colour = "grey92", linewidth = 0.25)
      } else {
        ggplot2::element_line(colour = "grey92", linewidth = 0.25)
      },
      panel.grid.major.x = if (identical(grid, "both")) {
        ggplot2::element_line(colour = "grey92", linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = if (isTRUE(border)) {
        ggplot2::element_rect(colour = "grey30", fill = NA, linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      legend.position = legend,
      legend.title = ggplot2::element_text(size = base_size, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size * 0.95),
      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.42, "cm"),
      legend.spacing = ggplot2::unit(2, "pt"),
      legend.box.spacing = ggplot2::unit(3, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      strip.background = ggplot2::element_rect(fill = "grey96", colour = NA),
      strip.text = ggplot2::element_text(
        size = base_size * 0.9, face = "bold", hjust = 0,
        margin = ggplot2::margin(2, 3, 2, 3)
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 6)
    )
}

# Backwards-compatible aliases used by the exported functions.
lcz_theme_series <- function(base_size = 8, legend_position = "top", grid = "y") {
  lcz_theme_journal(
    base_size = base_size,
    legend = legend_position,
    grid = grid
  )
}

lcz_theme_map <- function(base_size = 11, legend_position = "right") {
  ggplot2::theme_void(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "grey10"),
      plot.title = ggplot2::element_text(
        size = base_size * 1.2, face = "bold", hjust = 0,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 1.02, hjust = 0, margin = ggplot2::margin(b = 7)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.75, colour = "grey45", hjust = 1,
        margin = ggplot2::margin(t = 4)
      ),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(size = base_size, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size * 0.95),
      legend.key.height = ggplot2::unit(0.42, "cm"),
      legend.key.width = ggplot2::unit(0.42, "cm"),
      legend.spacing.y = ggplot2::unit(0.05, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 6, 4, 4)
    )
}

# --- palettes ---------------------------------------------------------------

#' Colour-blind-safe palettes
#'
#' @param n Number of colours.
#' @return A character vector of hex colours.
#' @keywords internal
#' @noRd
lcz_pal_diverging <- function(n = 11) {
  grDevices::colorRampPalette(
    c("#2166AC", "#67A9CF", "#D1E5F0", "#F7F7F7", "#FDDBC7", "#EF8A62", "#B2182B")
  )(n)
}

lcz_pal_sequential <- function(n = 9, option = "viridis") {
  if (requireNamespace("viridisLite", quietly = TRUE)) {
    return(viridisLite::viridis(n, option = option))
  }
  grDevices::colorRampPalette(c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"))(n)
}

lcz_scale_fill_diverging <- function(name = NULL, limits = NULL, midpoint = 0, ...) {
  ggplot2::scale_fill_gradient2(
    low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
    midpoint = midpoint, limits = limits, name = name, ...
  )
}

lcz_scale_colour_diverging <- function(name = NULL, limits = NULL, midpoint = 0, ...) {
  ggplot2::scale_colour_gradient2(
    low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
    midpoint = midpoint, limits = limits, name = name, ...
  )
}

lcz_scale_fill_sequential <- function(name = NULL, option = "viridis",
                                      direction = 1, ...) {
  ggplot2::scale_fill_viridis_c(option = option, direction = direction, name = name, ...)
}

# --- export -----------------------------------------------------------------

#' Save a journal-sized figure
#'
#' Writes PDF/SVG as vector (fonts embedded) and PNG/TIFF at 600 dpi, using
#' Nature/Science column widths.
#'
#' @param plot A ggplot object.
#' @param filename File name (extension decides the device).
#' @param width `"single"` (89 mm), `"double"` (183 mm) or a number in mm.
#' @param height Height in mm. Defaults to a 0.62 aspect ratio.
#' @param dpi Raster resolution. Default 600.
#' @param bg Background colour.
#' @param folder Output folder.
#' @param lang Language for the confirmation message.
#' @param quiet Logical. Suppress the confirmation message.
#'
#' @return Invisibly, the file path.
#' @export
lcz_save_figure <- function(plot, filename,
                            width = c("double", "single"),
                            height = NULL,
                            dpi = 600, bg = "white",
                            folder = "LCZ4r_output", lang = NULL, quiet = FALSE) {
  width_mm <- if (is.numeric(width)) {
    width
  } else {
    switch(match.arg(width),
      single = .lcz_journal$width_single_mm,
      double = .lcz_journal$width_double_mm
    )
  }
  if (is.null(height)) height <- round(width_mm * 0.7)

  dir <- lcz_output_dir(folder)
  path <- file.path(dir, filename)
  ext <- tolower(tools::file_ext(filename))

  if (ext == "pdf") {
    if (isTRUE(capabilities("cairo"))) {
      try(suppressWarnings({
        grDevices::cairo_pdf(
          path,
          width = width_mm / 25.4, height = height / 25.4,
          bg = bg, family = "sans", onefile = TRUE
        )
        print(plot)
      }), silent = TRUE)
      if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    }
    # cairo may be reported as available but fail (e.g. missing X11 libs)
    if (!file.exists(path) || is.na(file.size(path)) || file.size(path) == 0) {
      grDevices::pdf(
        path,
        width = width_mm / 25.4, height = height / 25.4,
        bg = bg, family = "sans", onefile = TRUE
      )
      print(plot)
      grDevices::dev.off()
    }
  } else if (ext == "svg" && requireNamespace("svglite", quietly = TRUE)) {
    svglite::svglite(path, width = width_mm / 25.4, height = height / 25.4, bg = bg)
    print(plot)
    grDevices::dev.off()
  } else if (ext %in% c("png", "tif", "tiff") &&
             requireNamespace("ragg", quietly = TRUE)) {
    device <- if (ext == "png") ragg::agg_png else ragg::agg_tiff
    device(path, width = width_mm / 25.4, height = height / 25.4,
           units = "in", res = dpi, background = bg)
    print(plot)
    grDevices::dev.off()
  } else {
    ggplot2::ggsave(
      filename = path, plot = plot,
      width = width_mm, height = height, units = "mm",
      dpi = dpi, bg = bg
    )
  }

  if (!isTRUE(quiet)) lcz_msg("msg_files_saved", lang = lang, path = dir)
  invisible(path)
}

# Dependency-free cartographic furniture ---------------------------------------
#
# A scale bar and a north arrow drawn in data coordinates, so they work with
# `coord_equal()` (no coord_sf/ggspatial requirement). Lengths are chosen from
# a "nice" sequence and capped at 28% of the map width.

lcz_add_scalebar <- function(plot, x, north = TRUE, length_km = NULL,
                             colour = "grey15") {
  e <- terra::ext(x)
  lonlat <- isTRUE(terra::is.lonlat(x))
  if (lonlat) {
    lat_mid <- (e$ymin + e$ymax) / 2
    km_per_deg <- 111.32 * cos(lat_mid * pi / 180)
    width_km <- (e$xmax - e$xmin) * km_per_deg
  } else {
    width_km <- (e$xmax - e$xmin) / 1000
  }

  if (is.null(length_km) || !is.finite(length_km)) {
    candidates <- c(0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
    ok <- candidates[candidates <= width_km * 0.28]
    length_km <- if (length(ok)) max(ok) else max(candidates[candidates <= width_km * 0.6], 0.5)
  }

  x0 <- e$xmin + (e$xmax - e$xmin) * 0.05
  y0 <- e$ymin + (e$ymax - e$ymin) * 0.06
  dx <- if (lonlat) length_km / km_per_deg else length_km * 1000
  tick <- (e$ymax - e$ymin) * 0.012

  label <- if (length_km < 1) {
    paste0(round(length_km * 1000), " m")
  } else {
    paste0(format(length_km, trim = TRUE), " km")
  }

  plot +
    ggplot2::annotate(
      "segment", x = x0, xend = x0 + dx, y = y0, yend = y0,
      linewidth = 0.55, colour = colour
    ) +
    ggplot2::annotate(
      "segment", x = x0, xend = x0, y = y0 - tick, yend = y0 + tick,
      linewidth = 0.45, colour = colour
    ) +
    ggplot2::annotate(
      "segment", x = x0 + dx, xend = x0 + dx, y = y0 - tick, yend = y0 + tick,
      linewidth = 0.45, colour = colour
    ) +
    ggplot2::annotate(
      "text", x = x0 + dx / 2, y = y0 + tick * 1.8, label = label,
      size = 3.0, colour = colour, vjust = 0
    )
}

lcz_add_north <- function(plot, x, colour = "grey15") {
  e <- terra::ext(x)
  x0 <- e$xmin + (e$xmax - e$xmin) * 0.06
  y0 <- e$ymax - (e$ymax - e$ymin) * 0.05
  dy <- (e$ymax - e$ymin) * 0.07
  plot +
    ggplot2::annotate(
      "segment", x = x0, xend = x0, y = y0 - dy, yend = y0,
      linewidth = 0.55, colour = colour,
      arrow = grid::arrow(length = grid::unit(2.2, "mm"), type = "closed")
    ) +
    ggplot2::annotate(
      "text", x = x0, y = y0 + (e$ymax - e$ymin) * 0.012, label = "N",
      size = 3.4, fontface = "bold", colour = colour, vjust = 0
    )
}

# Date-axis labels adapted to the time span (hours -> days -> months -> years).
lcz_date_labels <- function(x) {
  x <- as.POSIXct(x)
  span <- as.numeric(difftime(max(x, na.rm = TRUE), min(x, na.rm = TRUE), units = "days"))
  if (!is.finite(span)) return("%d %b")
  if (span <= 2) {
    "%H:%M"
  } else if (span <= 60) {
    "%d %b"
  } else if (span <= 730) {
    "%b %Y"
  } else {
    "%Y"
  }
}

# Diverging colour steps for "warming stripes", centred on the series mean.
lcz_scale_stripes <- function(x, name = NULL) {
  rng <- range(x, na.rm = TRUE)
  mid <- mean(x, na.rm = TRUE)
  ggplot2::scale_fill_stepsn(
    colours = rev(lcz_pal_diverging(11)),
    values = scales::rescale(c(rng[1], mid, rng[2])),
    limits = rng,
    name = name,
    guide = ggplot2::guide_colorsteps(
      barwidth = ggplot2::unit(75, "mm"),
      barheight = ggplot2::unit(3, "mm"),
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "grey70",
      ticks.colour = "grey40"
    )
  )
}

# Okabe-Ito categorical palette (colour-blind safe).
lcz_pal_categorical <- function(n) {
  base <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#000000"
  )
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}
