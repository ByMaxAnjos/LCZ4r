
#' Plot Local Climate Zone (LCZ) Map
#'
#' This function generates a graphical representation of an LCZ (Local Climate Zone) map provided as a SpatRaster object.
#'
#' @param x A SpatRaster object containing the LCZ map to be plotted.
#' @param isubtitle An optional subtitle for the plot, allowing you to specify the area or region represented in the map.
#' @param isave Logical. Set to TRUE if you want to save the plot in your working directory.
#' @param legend Specify the type of legend to include in the plot. Use "code" to display LCZ codes in the legend.
#'
#' @return A visual representation of the LCZ map in ggplot format
#'
#' @export
#'
#' @examples
#'
#' # Example: Plot an LCZ map with the legend showing LCZ codes and specify a subtitle.
#' # myplot <- lcz_plot_map(x = myLCZmap, isubtitle = "My Area", legend = "code")
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_map() to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_plot_map <- function(x, isubtitle = "", isave = FALSE, legend = "name") {

  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map( )")
  }

  if(!inherits(x, "RasterLayer")) {

    x <- raster::raster(x)

  }

    lczClass <- raster::ratify(x)
    rat <- raster::levels(lczClass)[[1]]
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

    lcz_df <- dplyr::bind_cols(ID, lcz.name, lcz.col) %>%
      dplyr::inner_join(rat, by = "ID")

    base::names(x) <- "class"

    # Define qualitative palette
    color_values <- lcz_df %>%
      dplyr::select(.data$ID, lcz.col) %>%
      dplyr::pull(.data$lcz.col, .data$ID)

    # Define LCZ labels
    if(legend == "code") {
      lcz.lables <- lcz_df$lcz
    } else {
      lcz.lables <- lcz_df$lcz.name
    }

  # ggplot using the same data
  dataPlot <- terra::as.data.frame(x, xy=TRUE) %>%
    tidyr::drop_na()

  my_plot <-
    ggplot2::ggplot() +
    # Add the raster layer
    #ggplot2::geom_sf(data=boundary, fill='transparent', lwd = 2, inherit.aes = FALSE) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = .data$y, fill = base::as.factor(class)),
                       data = dataPlot,  inherit.aes = FALSE) +

    # Set the color palette to a qualitative one and add labels, title and legend.hist
    ggplot2::scale_fill_manual(values = color_values, name = "LCZ class",
                               labels = lcz.lables,
                               guide = ggplot2::guide_legend(reverse = FALSE,
                                                             title.position = "top")) +
    ggplot2::coord_sf() +
    ggplot2::labs(title = "Local Climate Zones",
                  subtitle = isubtitle,
                  caption = "Source: LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData: Stewart and Oke, 2012; Demuzere et al.2022") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(color = "black", size = 18, hjust = 0.5),
                   plot.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size = 16, color = "black", face = "bold"),
                   legend.text = ggplot2::element_text(size = 16, color = "black"),
                   plot.caption = ggplot2::element_text(colour = "grey30", size = 9, hjust = 0), # move caption to the left
                   axis.line = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(25, 25, 10, 25)
                   #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                   #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
    )
  # ggspatial::annotation_scale() +
  # ggspatial::annotation_north_arrow(location = "br", which_north = "true")

  if(isave == TRUE){

    # Create a folder name using paste0
    folder <- base::paste0("LCZ4r_output/")

    # Check if the folder exists
    if (!base::dir.exists(folder)) {
      # Create the folder if it does not exist
      base::dir.create(folder)
    }

    file <- base::paste0(folder,"lcz_PlotMap.png")
    ggplot2::ggsave(file, my_plot, height = 7, width = 10, units="in", dpi=300)

  }

  base::cat("Congratulations! You've successfully generated the LCZ map.\n")
  return(my_plot)

}

