#' Visualize Interpolated LCZ map
#'
#' This function plots the interpolated LCZ anomaly, LCZ air temperature, or other environmental variables.
#'
#' @param x The LCZ map in SpatRaster format, either as a single raster or a raster stack, obtained from functions like lcz_anomaly_map() or lcz_interp_map().
#' @param palette Gradient palettes available in the tidyterra package. Default is "muted". More palettes can be found in the tidyterra documentation: https://dieghernan.github.io/tidyterra/articles/palettes.html#scale_fill_whitebox_
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ncol Number of columns in the plot.
#' @param nrow Number of rows in the plot.
#' @param isave Logical, indicating whether to save the plot to your directory. Default is FALSE.
#' @param ... Additional arguments to modify axis, legend, and plot labels, including title, subtitle, and caption.
#'
#' @return A plot of the LCZ interpolated map in ggplot2 format.
#'
#' @export
#'
#' @examples
#'
#' # Interpol your air temperature
#' # lcz_plot_interp(my_interp_map, sp.res = 100, tp.res = "hour",
#' #                 year = 2019, month= 2, day = 6, hour = 5,
#' #                 caption = "LCZ4r, 2024", fill = "Degree Celsius")
#'
#' # Plot multiple interpolated rasters
#' # lcz_plot_interp(my_interp_map, sp.res = 100, tp.res = "hour",
#' #                 year = 2019, month= 2, day = 6, by = "daylight",
#' #                 caption = "LCZ4r, 2024", fill = "Degree Celsius")
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_anomaly_map() to obtain LCZ anomaly rasters and lcz_interp_map() for air temperature rasters.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_plot_interp <- function(x,
                            palette = "muted",
                            direction = 1,
                            ncol = NULL, nrow = NULL,
                            isave = FALSE, ...) {
  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster stack object of raster package. Please, use the lcz_anomaly_map() and lcz_interp_map()")
  } else if (!is.null(x) & !inherits(x, "SpatRaster")) {
    x <- terra::rast({{ x }})
  }

  if (terra::nlyr({{ x }}) > 1) {
    final_graph <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = {{ x }}) +
      # MetBrewer::scale_fill_met_c(name = palette, direction=direction)+
      tidyterra::scale_fill_whitebox_c(palette = palette, direction = direction) +
      ggplot2::facet_wrap(~lyr, ncol = {{ ncol }}, nrow = {{ nrow }}) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top")) +
      ggplot2::coord_sf(expand = FALSE, clip = "off") +
      ggplot2::labs(...) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.height = ggplot2::unit(40L, "pt"),
        legend.title = ggplot2::element_text(size = 15, hjust = 0.5),
        strip.text = ggplot2::element_text(hjust = 0, size = 14),
        legend.text = ggplot2::element_text(size = 12),
        # plot.background = ggplot2::element_rect(fill = "grey98", color = NA),
        plot.title = ggplot2::element_text(face = "bold", size = 20),
        plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(5, 0, 15, 0), size = 17),
        plot.caption = ggplot2::element_text(color = "grey40", size = 10),
        plot.margin = ggplot2::margin(0, 10, 0, 10)
      )

    if (isave == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      # Save map as figure.png
      file <- base::paste0(getwd(), "/", folder, "lcz_interp_map.png")
      ggplot2::ggsave(file, final_graph, height = 9, width = 16, units = "in", dpi = 600)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(final_graph)
  } else {
    final_graph <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = {{ x }}) +
      # MetBrewer::scale_fill_met_c(name = palette, direction=direction)+
      tidyterra::scale_fill_whitebox_c(palette = palette, direction = direction) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top")) +
      ggplot2::coord_sf(expand = FALSE, clip = "off") +
      ggplot2::labs(...) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.height = ggplot2::unit(40L, "pt"),
        legend.title = ggplot2::element_text(size = 15, hjust = 0.5),
        strip.text = ggplot2::element_text(hjust = 0, size = 14),
        legend.text = ggplot2::element_text(size = 12),
        # plot.background = ggplot2::element_rect(fill = "grey98", color = NA),
        plot.title = ggplot2::element_text(face = "bold", size = 20),
        plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(5, 0, 15, 0), size = 17),
        plot.caption = ggplot2::element_text(color = "grey30", size = 12),
        plot.margin = ggplot2::margin(0, 10, 0, 10)
      )
    if (isave == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      # Save map as figure.png
      file <- base::paste0(getwd(), "/", folder, "lcz_interp_map.png")
      ggplot2::ggsave(file, final_graph, height = 9, width = 16, units = "in", dpi = 600)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
    }

    return(final_graph)
  }
}







