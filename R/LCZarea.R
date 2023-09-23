
#' Calculate LCZ Class Areas
#'
#' This function calculates the areas of LCZ (Local Climate Zones) classes in both percentage and square kilometers.
#'
#' @param x A raster SpatRaster layer containing LCZ classes.
#' @param iplot Logical, indicating whether to create a plot (default is TRUE).
#' @param isave Save the plot into your directory.
#'
#' @return A summary table of LCZ class areas if iplot is FALSE, otherwise, a bar plot.
#'
#' @export
#'
#' @examples
#'
#' #Calcute the LCZarea
#' #my_lcz_area <- LCZarea(x= my_lcz_map, iplot = TRUE, isave = TRUE)
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for getLCZmap() to obtain an LCZ map.

LCZarea <- function(x, iplot=TRUE, isave=FALSE){

    freq_df <- tibble::as_tibble(terra::freq({{x}}, bylayer=FALSE, usenames=TRUE)) %>%
    purrr::set_names(c("lcz", "count")) %>%
    dplyr::mutate(lcz = base::as.factor(lcz))

   lcz_area <- terra::cellSize({{x}}, unit = "km")

  lcz_areas_df <- base::data.frame(LCZ = terra::values({{x}}), Area_Km2 = terra::values(lcz_area)) %>%
    stats::na.omit() %>%
    dplyr::group_by(lcz) %>%
    dplyr::summarise(area_km2 = base::round(base::sum(.data$area),digits = 2)) %>%
    dplyr::mutate(lcz = base::as.factor(lcz))

  summary_resul <- dplyr::inner_join(freq_df, lcz_areas_df, by="lcz") %>%
    dplyr::mutate(area_perc = base::round(.data$area_km2/sum(.data$area_km2)*100, digits = 2))

  if(iplot == TRUE) {

    lcz <- c(base::seq(1, 10, 1), base::seq(11, 17)) %>%
      tibble::as_tibble() %>% purrr::set_names("ID")

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

    lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col) %>%
      dplyr::mutate(lcz = .data$ID) %>%
      dplyr::mutate(lcz = base::as.factor(lcz)) %>%
      dplyr::inner_join(summary_resul, by = "lcz")

    # Define qualitative palette
    color_values <- lcz_df %>%
      dplyr::select(lcz, lcz.col) %>%
      dplyr::pull(lcz.col, lcz)

    # Define LCZ labels
    lcz.lables <- lcz_df$lcz.name

    # Create the ggplot

    graph <-
      ggplot2::ggplot(lcz_df, ggplot2::aes(x = factor(lcz), y = .data$area_km2,  fill = factor(lcz))) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = color_values, name = "LCZ class",
                                 labels = lcz.lables,
                                 guide = ggplot2::guide_legend(reverse = FALSE,
                                                      title.position = "top")) +
      ggplot2::geom_text(data = lcz_df,
                         label = paste0(round(lcz_df$area_perc, 1), "%"), vjust = -1, size = 6) +
      ggplot2::labs(title = "",
           x = "LCZ",
           y = "Area [square kilometer]",
           fill = "LCZ") +
      ggplot2::theme_bw() +
      ggplot2::labs(caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022), https://doi.org/10.5194/essd-14-3835-2022") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 17),
        axis.title.x =ggplot2::element_text(size = 17),
        axis.text.y = ggplot2::element_text(size = 17),
        axis.title.y =ggplot2::element_text(size = 17),
        legend.text = ggplot2::element_text(size = 18),
        legend.title = ggplot2::element_text(size = 18, face = "bold"),
        plot.caption = ggplot2::element_text(color = "grey60", size = 9))

    if(isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_area.png")
      ggplot2::ggsave(file, graph, height = 9, width = 14, units="in", dpi=300)

    }

    return(graph)

  } else {
    return(summary_resul)
  }


}

