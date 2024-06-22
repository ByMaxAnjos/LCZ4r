#' Calculate LCZ areas
#'
#' This function calculates the areas of LCZ classes in both percentage and square kilometers.
#'
#' @param x A raster SpatRaster layer containing LCZ classes.
#' @param iplot Logical, indicating whether to create a plot (default is TRUE).
#' @param isave Save the plot into your directory.
#' @param inclusive Set to TRUE to a colorblind-friendly palette.
#' @param ylab y-axis name.
#' @param xlab x-axis name.
#' @param ... Additional arguments to modify axis, legend, and plot labels, including title, subtitle and caption.
#'
#' @return A summary table of LCZ class areas if iplot is FALSE, otherwise, a bar plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Calculate the LCZ area
#' my_lcz_area <- lcz_cal_area(my_lcz_map, iplot = TRUE, isave = TRUE)
#' }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_map() to obtain an LCZ map.

lcz_cal_area <- function(x, iplot=TRUE, isave=FALSE, inclusive = FALSE,
                         xlab = "LCZ code",
                         ylab = "Area [square kilometer]", ...){


# Validate inputs ---------------------------------------------------------

  if(!inherits(x, "SpatRaster")) {x <- terra::rast({{x}}) }

  if (terra::nlyr(x) > 1) {
    x <- x[[2]]
  }

# Calculate raster area ---------------------------------------------------

  freq_df <- tibble::as_tibble(terra::freq({{x}}, bylayer=FALSE, usenames=TRUE)) %>%
    purrr::set_names(c("lcz", "count")) %>%
    dplyr::mutate(lcz = base::as.factor(lcz))

  if (base::any((freq_df$lcz == 0))) {

    freq_df$lcz[freq_df$lcz==0] <- 17

    freq_df <- dplyr::group_by(freq_df, lcz) %>%
      dplyr::summarise(count = sum(.data$count)) %>%
      dplyr::ungroup()
  }

   lcz_area <- terra::cellSize({{x}}, unit = "km")


  lcz_areas_df <- base::data.frame(LCZ = terra::values({{x}}),
                                   Area_Km2 = terra::values(lcz_area)) %>%
    purrr::set_names(c("lcz", "area")) %>%
    stats::na.omit() %>%
    dplyr::group_by(lcz) %>%
    dplyr::summarise(area_km2 = base::round(base::sum(.data$area),digits = 2)) %>%
    dplyr::mutate(lcz = base::as.factor(lcz))

  summary_resul <- dplyr::inner_join(freq_df, lcz_areas_df, by="lcz") %>%
    dplyr::mutate(area_perc = base::round(.data$area_km2/sum(.data$area_km2)*100, digits = 2))

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

    lcz_colorblind <- c("#E16A86", "#D8755E", "#C98027", "#B48C00",
                        "#989600", "#739F00", "#36A631", "#00AA63",
                        "#00AD89", "#00ACAA", "#00A7C5", "#009EDA",
                        "#6290E5", "#9E7FE5", "#C36FDA", "#D965C6",
                        "#E264A9") %>%
      tibble::as_tibble() %>%
      purrr::set_names("lcz_colorblind")

    lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col, lcz_colorblind) %>%
      dplyr::mutate(lcz = .data$ID) %>%
      dplyr::mutate(lcz = base::as.factor(lcz)) %>%
      dplyr::inner_join(summary_resul, by = "lcz")

    # Define qualitative palette
    if(inclusive == TRUE) {

      color_values <- lcz_df %>%
        dplyr::select(lcz, lcz_colorblind) %>%
        dplyr::pull(lcz_colorblind, lcz)

    } else {

      color_values <- lcz_df %>%
        dplyr::select(lcz, lcz.col) %>%
        dplyr::pull(lcz.col, lcz)
    }

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
                         label = paste0(round(lcz_df$area_perc, 1), "%"), vjust = -0.2, size = 5) +
      ggplot2::scale_y_continuous(limits = c(0, base::max(lcz_df$area_km2) + 50)) +
       ggplot2::labs(...,
           x = xlab,
           y = ylab) +
      ggplot2::theme_bw()+
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(color = "black", size = 17, hjust = 0.5),
        panel.background = ggplot2::element_rect(color = NA, fill = "grey97"),
        panel.grid.major = ggplot2::element_line(color = "grey90"),
        panel.grid.minor = ggplot2::element_line(color = "grey90"),
        panel.grid.major.y = ggplot2::element_line(color = "grey90"),
        axis.text.x = ggplot2::element_text(size = 16),
        axis.title.x =ggplot2::element_text(size = 16, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 16),
        axis.title.y =ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 16),
        legend.title = ggplot2::element_text(size = 17),
        legend.spacing.y = ggplot2::unit(0.02, "cm"),
        plot.margin = ggplot2::margin(25, 25, 10, 25),
        plot.caption = ggplot2::element_text(color = "grey40", size = 10, hjust = 0))

    if (isave == TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file.1 <- base::paste0(getwd(), "/", folder,"lcz4r_area_plot.png")
      ggplot2::ggsave(file.1, graph, height = 7, width = 12, dpi=600)
      file.2 <- base::paste0(getwd(),"/", folder,"lcz4r_area_df.csv")
      utils::write.csv(lcz_df, file.2)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    if (iplot == FALSE) {

      return(lcz_df)

    } else {
        return(graph)
      }


}

