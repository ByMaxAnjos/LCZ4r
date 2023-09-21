
#' Calculate LCZ Class Areas
#'
#' This function calculates the areas of LCZ (Local Climate Zones) classes in both percentage and square kilometers.
#'
#' @param lcz_map A raster Spatraster layer containing LCZ classes.
#' @param iplot Logical, indicating whether to create a plot (default is TRUE).
#'
#' @return A summary table of LCZ class areas if iplot is FALSE, otherwise, a bar plot.
#'
#' @export
#'
#' @examples
#' myLCZarea <- LCZarea(map, iplot = TRUE)
#'

LCZarea <- function(lcz_map= NULL, iplot=TRUE){

  freq_df <- tibble::as_tibble(terra::freq({{lcz_map}}, bylayer=FALSE, usenames=TRUE)) %>%
    purrr::set_names("lcz", "count") %>%
    dplyr::mutate(lcz = as.factor(lcz))
  lcz_area <- terra::cellSize(lcz_map, unit = "km")

  lcz_areas_df <- data.frame(LCZ = terra::values(lcz_map), Area_Km2 = terra::values(lcz_area)) %>%
    drop_na() %>%
    group_by(lcz) %>%
    summarise(area_km2 = round(sum(area),digits = 2)) %>%
    dplyr::mutate(lcz = as.factor(lcz))

  summary_resul <- dplyr::inner_join(freq_df, lcz_areas_df, by="lcz") %>%
    dplyr::mutate(area_perc = base::round(area_km2/sum(area_km2)*100, digits = 2))

  if(iplot == TRUE) {

    lcz <- c(base::seq(1, 10, 1), base::seq(11, 17)) %>%
      tibble::as_tibble() %>% purrr::set_names("ID")

    lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                  "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                  "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                  "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water") %>% as_tibble() %>% set_names("lcz.name")
    lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
                 "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
                 "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA") %>% as_tibble() %>% set_names("lcz.col")

    lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col) %>%
      dplyr::mutate(lcz = ID) %>%
      dplyr::inner_join(summary_resul, by = "lcz")

    # Define qualitative palette
    color_values <- lcz_df %>%
      dplyr::select(lcz, lcz.col) %>%
      dplyr::pull(lcz.col, lcz)

    # Define LCZ labels
    lcz.lables <- lcz_df$lcz.name

    # Create the ggplot

    graph <-
      ggplot(lcz_df, aes(x = factor(lcz), y = area_km2,  fill = factor(lcz))) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = color_values, name = "LCZ class",
                                 labels = lcz.lables,
                                 guide = guide_legend(reverse = FALSE,
                                                      title.position = "top")) +
      ggplot2::geom_text(data = lcz_df,
                         label = paste0(round(lcz_df$area_perc, 1), "%"), vjust = -1)+
      ggplot2::labs(title = "",
           x = "LCZ",
           y = "Area [km-2]",
           fill = "LCZ") +
      ggplot2::theme_bw() +
      ggplot2::labs(caption = "Chart: Max Anjos • Source:©LCZ4r\nData:Demuzere et al.(2022), https://doi.org/10.5194/essd-14-3835-2022") +
      ggplot2::theme(
        axis.text.x = element_text(size = 12),
        axis.title.x =element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y =element_text(size = 12),
        plot.caption = element_text(color = "grey60",
                                    size = 8))
    return(graph)

  } else {
    return(summary_resul)
  }

}

