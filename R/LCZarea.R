
#' Calculate LCZ class areas
#'
#' @param lcz_map
#' @param iplot
#'
#' @return areas em percentage and km2
#' @export
#'
#' @examples
#' myLCZarea <- LCZarea(map, iplot = TRUE)
#'
LCZarea <- function(lcz_map, iplot=TRUE){
  suppressMessages(sf_use_s2(TRUE))
  imap <- lcz_map %>%
    terra::rast()
  freq_df <- as_tibble(terra::freq(imap, bylayer=FALSE, usenames=TRUE)) %>%
    set_names("lcz", "count")
  lcz_shp <- terra::as.polygons(rast(lcz_map)) %>% sf::st_as_sf()
  lcz_shp <- sf::st_transform(lcz_shp, crs = 4326)
  lcz_area <- sf::st_area(lcz_shp)/1000000
  lcz_area <- as_tibble(as.numeric(lcz_area)) %>%
    bind_cols(lcz_shp %>% sf::st_drop_geometry()) %>% set_names("area_km2", "lcz")

  summary_resul <- inner_join(freq_df, lcz_area, by="lcz") %>%
    mutate(area_perc = round(area_km2/sum(area_km2)*100, digits = 2),
           area_km2 = round(area_km2, digits = 3))

  if(iplot == TRUE) {

    lcz <- c(seq(1, 10, 1), seq(11, 17)) %>% as_tibble() %>% set_names("ID")

    lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                  "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                  "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                  "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water") %>% as_tibble() %>% set_names("lcz.name")
    lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
                 "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
                 "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA") %>% as_tibble() %>% set_names("lcz.col")
    lcz_df <- bind_cols(lcz, lcz.name, lcz.col) %>%
      mutate(lcz = ID) %>%
      inner_join(summary_resul, by = "lcz")

    # Define qualitative palette
    color_values <- lcz_df %>% dplyr::select(lcz, lcz.col) %>% pull(lcz.col, lcz)

    # Define LCZ labels
    lcz.lables <- lcz_df$lcz.name

    # Create the ggplot

    graph <- ggplot(lcz_df, aes(x = factor(lcz), y = area_km2,  fill = factor(lcz))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables, guide = guide_legend(reverse = FALSE, title.position = "top")) +
      geom_text(data = lcz_df, label = paste0(round(lcz_df$area_perc, 1), "%"), vjust = -1)+
      labs(title = "",
           x = "LCZ",
           y = "Area [km-2]",
           fill = "LCZ") +
      theme_bw() +
      labs(caption = "Chart: Max Anjos • Source: ©ZoomCityCarbonModel\nData:Demuzere et al.(2022), https://doi.org/10.5194/essd-14-3835-2022") +
      theme(
        axis.text.x = element_text(size = 12),
        axis.title.x =element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.caption = element_text(color = "grey60",
                                    size = 8))
    graph

  } else {
    return(summary_resul)
  }

}

