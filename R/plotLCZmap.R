
#' Plot LCZ Map
#'
#' This function plots the classes of an LCZ (Local Climate Zone) map represented as a SpatRaster object.
#' You can obtain the LCZ map using the getLCZmap() function.
#'
#' @param x The LCZ map in SpatRaster format.
#' @param legend Specify whether to include a legend. Set to "code" to show LCZ codes in the legend.
#'
#' @return A plot of the LCZ map.
#'
#' @export
#'
#' @examples
#' myLCZmap <- getLCZmap(city = "Berlin")
#' plotLCZmap(x = myLCZmap, legend = "code")
#'
#' @seealso
#' See the documentation for getLCZmap() to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


plotLCZmap <- function(x, legend = "name") {
  lcz_map <- raster::raster(x)
  lczClass <- raster::ratify(lcz_map)
  rat <- raster::levels(lczClass)[[1]]
  ID <- c(seq(1, 10, 1), seq(11, 17)) %>% as_tibble() %>% set_names("ID")

  lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water") %>% as_tibble() %>% set_names("lcz.name")
  lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
               "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
               "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA") %>% as_tibble() %>% set_names("lcz.col")

  lcz_df <- bind_cols(ID, lcz.name, lcz.col) %>%
    inner_join(rat, by = "ID")
  names(lcz_map) <- "class"

  # Define qualitative palette
  color_values <- lcz_df %>% dplyr::select(ID, lcz.col) %>% pull(lcz.col, ID)

  # Define LCZ labels
  if(legend == "code") {
    lcz.lables <- lcz_df$lcz
  } else {
    lcz.lables <- lcz_df$lcz.name
  }

  # ggplot using the same data
  ggplot() +
    # Add the raster layer
    geom_raster(aes(x = x, y = y, fill = as.factor(class)), data = terra::as.data.frame(lcz_map, xy=TRUE) %>% drop_na(), interpolate = TRUE) +
    # Set the color palette to a qualitative one and add labels, title and legend.hist
    scale_fill_manual(values = color_values, name = "Class", labels = lcz.lables, guide = guide_legend(reverse = FALSE, title.position = "top")) +
    # Add layout elements
    coord_equal() +
    labs(title = "Local Climate Zones",
      caption = "Chart: Max Anjos • Source:©LCZ4 and ggplot2\nData:Demuzere et al.(2022), https://doi.org/10.5194/essd-14-3835-2022",
         colour = "grey60", size = 8) +
    theme_void()+
    theme(plot.title = element_text(color = "#3f1651", size = 14, face = "bold"),
          plot.background = element_rect(fill = "white"),
          legend.position = "bottom",
          legend.box.background = element_rect(colour = "black", size = 0.5, fill = NA),
          legend.background = element_rect(size = 0.5, colour = "black", fill = "#f7f7f7", linetype = "solid"),
          legend.text = element_text(size = 10, color = "black"),
          plot.caption = element_text(colour = "grey60", size = 8), # move caption to the left
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(color = "white", size = 0.3),
          panel.grid.minor = element_line(color = "white", size = 0.3),
          plot.margin = margin(1, 1, 1, 1))+
    ggspatial::annotation_scale(bar_cols = c("grey40", "grey80"), colour = "white")+
    ggspatial::annotation_north_arrow(location = "br", which_north = "true",
                                      height = unit(1.0, "cm"),
                                      width = unit(1.0, "cm"))
}
