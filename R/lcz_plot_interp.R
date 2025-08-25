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
#' @param save_extension File format for saving the plot. Options: "png", "jpg", "jpeg", "tif", "pdf", "svg" (default is "png").
#' @param ... Additional arguments to modify axis, legend, and plot labels, including title, subtitle, and caption.
#'
#' @return A plot of the LCZ interpolated map in ggplot2 format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # Plot the interpolated map
#'  lcz_plot_interp(my_interp_map)
#'  }
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
                            ncol = NULL,
                            nrow = NULL,
                            isave = FALSE,
                            save_extension = "png",
                            ...) {
  # Validate inputs more efficiently
  if (is.null(x)) {
    stop("The input must be a SpatRaster object. Please use lcz_anomaly_map() or lcz_interp_map() to generate the input.")
  }

  # Convert to SpatRaster if needed (only once)
  x <- if (!inherits(x, "SpatRaster")) terra::rast(x) else x

  # Create base plot elements (common to both cases)
  base_plot <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = x, na.rm = TRUE) +  # Handle NA values
    tidyterra::scale_fill_whitebox_c(
      palette = palette,
      direction = direction,
      na.value = "transparent"
    ) +
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
      plot.title = ggplot2::element_text(face = "bold", size = 20),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(5, 0, 15, 0), size = 17),
      plot.caption = ggplot2::element_text(color = "grey30", size = 12),
      plot.margin = ggplot2::margin(0, 10, 0, 10)
    )

  # Add faceting only if multiple layers exist
  if (terra::nlyr(x) > 1) {
    final_graph <- base_plot +
      ggplot2::facet_wrap(~lyr, ncol = ncol, nrow = nrow)
  } else {
    final_graph <- base_plot
  }

  # Handle saving (common to both cases)
  if (isave) {
    folder <- "LCZ4r_output/"
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    file <- file.path(getwd(), folder, paste0("lcz_interp_map.", save_extension))
    ggplot2::ggsave(
      filename = file,
      plot = final_graph,
      height = 9,
      width = 16,
      units = "in",
      dpi = 600
    )
    message("Looking at your files in the path: ", file.path(getwd(), folder))
  }

  return(final_graph)
}


