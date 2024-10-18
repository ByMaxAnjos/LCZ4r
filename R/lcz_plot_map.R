#' Visualize the LCZ Map
#'
#' This function generates a graphical representation of a Local Climate Zone (LCZ) map provided as a `SpatRaster` object.
#'
#' @param x A SpatRaster object containing the LCZ map to be plotted.
#' @param isave Logical. Set to TRUE to save the plot in your working directory.
#' @param save_extension File format for saving the plot. Options: "png", "jpg", "jpeg", "tif", "pdf", "svg" (default is "png").
#' @param show_legend Logical. If TRUE, displays the legend on the plot. If FALSE, the legend will be hidden. Default is TRUE.
#' @param inclusive Logical. Set to TRUE to use a colorblind-friendly palette.
#' @param ... Optional arguments to modify axis labels, legend, plot title, subtitle, and caption.
#'
#' @return A visual representation of the LCZ map in ggplot format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lcz_plot_map(LCZmap)
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_plot_map <- function(x,
                         isave = FALSE,
                         show_legend = TRUE,
                         save_extension = "png",
                         inclusive = FALSE,
                         ...) {

  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map( )")
  }

  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast({{x}})
    }
  if (terra::nlyr(x) > 1) {
    x <- x[[2]]
  }

  if(!inherits(x, "RasterLayer")) {
    x <- raster::raster(x)
    }
  if (raster::nlayers(x) > 0) {
    x[raster::values(x) == 0] <- 17
  }

  lczClass <- raster::ratify(x)
  rat <- raster::levels(lczClass)[[1]]

    # Check if 'ID' column contains 0 and replace it with 17 if it does
    ID <- c(base::seq(1, 10, 1), base::seq(11, 17)) %>%
      tibble::as_tibble() %>%
      purrr::set_names("ID")

    lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                  "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                  "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                  "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water") %>%
      tibble::as_tibble() %>%
      purrr::set_names("lcz.name")

    lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
                 "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
                 "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA") %>%
      tibble::as_tibble() %>%
      purrr::set_names("lcz.col")
    lcz_colorblind <- c("#E16A86", "#D8755E", "#C98027", "#B48C00",
                        "#989600", "#739F00", "#36A631", "#00AA63",
                        "#00AD89", "#00ACAA", "#00A7C5", "#009EDA",
                        "#6290E5", "#9E7FE5", "#C36FDA", "#D965C6",
                        "#E264A9") %>%
      tibble::as_tibble() %>%
      purrr::set_names("lcz_colorblind")

    lcz_df <- dplyr::bind_cols(ID, lcz.name, lcz.col, lcz_colorblind) %>%
      dplyr::inner_join(rat, by = "ID") %>%
      dplyr::distinct(ID, .keep_all = TRUE)

    base::names(x) <- "class"

    # Define qualitative palette

    if(inclusive == TRUE) {
      color_values <- lcz_df %>%
        dplyr::select(.data$ID, lcz_colorblind) %>%
        dplyr::pull(.data$lcz_colorblind, .data$ID)

    } else {
      color_values <- lcz_df %>%
        dplyr::select(.data$ID, lcz.col) %>%
        dplyr::pull(.data$lcz.col, .data$ID)
    }
    # Define LCZ labels
    lcz.lables <- lcz_df$lcz.name

  # ggplot using the same data
  dataPlot <- terra::as.data.frame(x, xy=TRUE) %>%
    stats::na.omit()

  if (any(rat$ID == 0)) {
    rat$ID[rat$ID == 0] <- 17
  }

  my_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = .data$y, fill = base::as.factor(class)),
                       data = dataPlot,  inherit.aes = FALSE) +
    ggplot2::scale_fill_manual(values = color_values, name = "LCZ class",
                               labels = lcz.lables,
                               guide = ggplot2::guide_legend(reverse = FALSE,
                                                             title.position = "top")) +
    ggplot2::labs(...) +
    ggplot2::coord_fixed()+
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(color = "black", size = 17, hjust = 0.5),
                   plot.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size = 17, color = "black", face = "bold"),
                   legend.text = ggplot2::element_text(size = 16, color = "black"),
                   plot.caption = ggplot2::element_text(colour = "grey40", size = 10, hjust = 0), # move caption to the left
                   axis.line = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.spacing.y = ggplot2::unit(0.02, "cm"),
                   plot.margin = ggplot2::margin(25, 25, 10, 25)
    )

  if (!show_legend) {
    my_plot <- my_plot + ggplot2::theme(legend.position = "none")
  }

  if (isave == TRUE){

    # Create a folder name using paste0
    folder <- base::paste0("LCZ4r_output/")

    # Check if the folder exists
    if (!base::dir.exists(folder)) {
      # Create the folder if it does not exist
      base::dir.create(folder)
    }

    file <- base::paste0(getwd(), "/", folder,"lcz_plot_map.", save_extension)
    ggplot2::ggsave(file, my_plot, height = 7, width = 10, dpi=600)
    base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

  }

  return(my_plot)

}

