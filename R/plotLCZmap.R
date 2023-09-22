
#' Plot LCZ Map
#'
#' This function plots the classes of an LCZ (Local Climate Zone) map represented as a SpatRaster object.
#'
#' @param x The LCZ map in SpatRaster format.
#' @param isubtitle Specify your name area
#' @param isave Save the plot into your directory.
#' @param legend Specify whether to include a legend. Set to "code" to show LCZ codes in the legend.
#'
#' @return A plot of the LCZ map.
#'
#' @export
#'
#' @examples
#' myLCZmap <- getLCZmap(city = "Berlin")
#'
#' plotLCZmap(x = myLCZmap, legend = "name")
#'
#' @seealso
#' See the documentation for getLCZmap() to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


plotLCZmap <- function(x, isubtitle = "", isave = TRUE, legend = "name") {
  lcz_map <- raster::raster({{x}})
  lczClass <- raster::ratify(lcz_map)
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

  base::names(lcz_map) <- "class"

  # Define qualitative palette
  color_values <- lcz_df %>%
    dplyr::select(ID, lcz.col) %>%
    dplyr::pull(lcz.col, ID)

  # Define LCZ labels
  if(legend == "code") {
    lcz.lables <- lcz_df$lcz
  } else {
    lcz.lables <- lcz_df$lcz.name
  }

  # ggplot using the same data
my_plot <-
  ggplot2::ggplot() +
    # Add the raster layer
    ggplot2::geom_raster(aes(x = x, y = y, fill = base::as.factor(class)),
                         data = terra::as.data.frame(lcz_map, xy=TRUE) %>% tidyr::drop_na(),
                         interpolate = TRUE) +
    # Set the color palette to a qualitative one and add labels, title and legend.hist
    ggplot2::scale_fill_manual(values = color_values, name = "LCZ class",
                               labels = lcz.lables,
                               guide = ggplot2::guide_legend(reverse = FALSE,
                                                             title.position = "top")) +
    # Add layout elements
    #ggplot2::coord_equal() +
    ggplot2::labs(title = "Local Climate Zones",
                  subtitle = isubtitle,
      caption = "• Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022), https://doi.org/10.5194/essd-14-3835-2022") +
    ggplot2::theme_void() +
    theme(plot.title = ggplot2::element_text(color = "#3f1651", size = 18, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = "#3f1651", size = 18),
          plot.background = ggplot2::element_rect(fill = "white"),
          legend.title = element_text(size = 16, color = "black", face = "bold"),
          legend.text = ggplot2::element_text(size = 16, color = "black"),
          plot.caption = ggplot2::element_text(colour = "grey60", size = 9), # move caption to the left
          axis.line = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
          #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
          plot.margin = ggplot2::margin(1, 1, 1, 1))

    # ggplot2::coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs") +
    # ggspatial::annotation_scale(plot_unit = "m", bar_cols = c("grey40", "grey80"), colour = "white") +
    # ggspatial::annotation_north_arrow(location = "br", which_north = "true",
    #                                   height = unit(1.0, "cm"),
    #                                   width = unit(1.0, "cm"))

  if(isave == TRUE){

  # Create a folder name using paste0
  folder <- base::paste0("LCZ4r_output/")

  # Check if the folder exists
  if (!base::dir.exists(folder)) {
    # Create the folder if it does not exist
    base::dir.create(folder)
  }

  file <- base::paste0(folder,"lcz_plot.png")
  ggplot2::ggsave(file, my_plot, height = 7, width = 10, units="in", dpi=300)

}

return(my_plot)

}
