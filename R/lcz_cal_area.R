#' Calculate LCZ areas
#'
#' This function calculates the areas of LCZ classes in both percentage and square kilometers.
#'
#' @param x A raster SpatRaster layer containing LCZ classes.
#' @param plot_type Type of plot to generate: "bar", "pie", or "donut". Default is "bar".
#' @param iplot Logical, indicating whether to create a plot (default is TRUE).
#' @param isave Logical, whether to save the plot to your directory (default is FALSE).
#' @param save_extension File format for saving the plot. Options: "png", "jpg", "jpeg", "tif", "pdf", "svg" (default is "png").
#' @param show_legend Logical. If TRUE, displays the legend on the plot. If FALSE, the legend will be hidden. Default is TRUE.
#' @param inclusive Logical. If TRUE, use a colorblind-friendly palette. Default is FALSE.
#' @param xlab Character, label for the x-axis (default is "LCZ code").
#' @param ylab Character, label for the y-axis (default is "Area (km2)").
#' @param ... Additional arguments to modify axis, legend, and plot labels, including title, subtitle, and caption.
#'
#' @return A summary table of LCZ class areas if plot = FALSE, otherwise, the specified plot in ggplot format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate LCZ area and visualize as bar plot
#' lcz_cal_area(my_lcz_map, plot_type = "bar")
#'
#' # Visualize as pie chart
#' lcz_cal_area(my_lcz_map, plot_type = "pie")
#'
#' # Save donut chart plot
#' lcz_cal_area(my_lcz_map, plot_type = "donut", isave = TRUE)
#'
#' # Return a dataframe
#' lcz_cal_area(my_lcz_map, plot_type = "donut", iplot = FALSE)
#' }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_map() to obtain an LCZ map.

lcz_cal_area <- function(x,
                         plot_type = "bar",
                         iplot = TRUE,
                         isave = TRUE,
                         save_extension = "png",
                         show_legend = TRUE,
                         inclusive = FALSE,
                         xlab = "LCZ code",
                         ylab = "Area [km2]",
                         ...) {
  # Validate inputs ---------------------------------------------------------

  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast({{ x }})
  }

  if (terra::nlyr(x) > 1) {
    x <- x[[2]]
  }

  # Calculate raster area ---------------------------------------------------

  freq_df <- tibble::as_tibble(terra::freq({{ x }}, bylayer = FALSE, usenames = TRUE)) %>%
    purrr::set_names(c("lcz", "count")) %>%
    dplyr::mutate(lcz = base::as.factor(lcz))

  if (base::any((freq_df$lcz == 0))) {
    freq_df$lcz[freq_df$lcz == 0] <- 17

    freq_df <- dplyr::group_by(freq_df, lcz) %>%
      dplyr::summarise(count = sum(.data$count)) %>%
      dplyr::ungroup()
  }

  lcz_area <- terra::cellSize({{ x }}, unit = "km")


  lcz_areas_df <- base::data.frame(
    LCZ = terra::values({{ x }}),
    Area_Km2 = terra::values(lcz_area)
  ) %>%
    purrr::set_names(c("lcz", "area")) %>%
    stats::na.omit() %>%
    dplyr::group_by(lcz) %>%
    dplyr::summarise(area_km2 = base::round(base::sum(.data$area), digits = 2)) %>%
    dplyr::mutate(lcz = base::as.factor(lcz))

  summary_resul <- dplyr::inner_join(freq_df, lcz_areas_df, by = "lcz") %>%
    dplyr::mutate(area_perc = base::round(.data$area_km2 / base::sum(.data$area_km2) * 100, digits = 2))

  lcz <- c(base::seq(1, 10, 1), base::seq(11, 17)) %>%
    tibble::as_tibble() %>%
    purrr::set_names("ID")

  lcz.name <- c(
    "Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
    "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
    "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
    "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water"
  ) %>%
    tibble::as_tibble() %>%
    purrr::set_names("lcz.name")

  lcz.col <- c(
    "#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
    "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
    "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA"
  ) %>%
    tibble::as_tibble() %>%
    purrr::set_names("lcz.col")

  lcz_colorblind <- c(
    "#E16A86", "#D8755E", "#C98027", "#B48C00",
    "#989600", "#739F00", "#36A631", "#00AA63",
    "#00AD89", "#00ACAA", "#00A7C5", "#009EDA",
    "#6290E5", "#9E7FE5", "#C36FDA", "#D965C6",
    "#E264A9"
  ) %>%
    tibble::as_tibble() %>%
    purrr::set_names("lcz_colorblind")

  lcz_df <- dplyr::bind_cols(lcz, lcz.name, lcz.col, lcz_colorblind) %>%
    dplyr::mutate(lcz = .data$ID) %>%
    dplyr::mutate(lcz = base::as.factor(lcz)) %>%
    dplyr::inner_join(summary_resul, by = "lcz")

  # Define qualitative palette
  if (inclusive == TRUE) {
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

  if (plot_type == "bar") {
    # Create the ggplot
    graph <-
      ggplot2::ggplot(lcz_df, ggplot2::aes(x = factor(lcz), y = .data$area_km2, fill = factor(lcz))) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(
        values = color_values, name = "LCZ class",
        labels = lcz.lables,
        guide = ggplot2::guide_legend(
          reverse = FALSE,
          title.position = "top"
        )
      ) +
      ggplot2::geom_text(
        data = lcz_df,
        label = paste0(round(lcz_df$area_perc, 1), "%"),
        vjust = -0.2, size = 5, check_overlap = TRUE
      ) +
      ggplot2::scale_y_continuous(limits = c(0, base::max(lcz_df$area_km2) + 50)) +
      ggplot2::labs(...,
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(color = "black", size = 17, hjust = 0.5),
        plot.background = ggplot2::element_rect(fill="transparent", color=NA),
        legend.background = ggplot2::element_rect(fill="transparent", color=NA),
        panel.background = ggplot2::element_rect(color = NA, fill = "grey97"),
        panel.grid.major = ggplot2::element_line(color = "grey90"),
        panel.grid.minor = ggplot2::element_line(color = "grey90"),
        panel.grid.major.y = ggplot2::element_line(color = "grey90"),
        axis.text.x = ggplot2::element_text(size = 16),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 16),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 16),
        legend.title = ggplot2::element_text(size = 17),
        legend.spacing.y = ggplot2::unit(0.02, "cm"),
        plot.margin = ggplot2::margin(25, 25, 10, 25),
        plot.caption = ggplot2::element_text(color = "grey40", size = 10, hjust = 0)
      )


    if (!show_legend) {
      graph <- graph + ggplot2::theme(legend.position = "none")
    }
  }

  if (plot_type == "pie") {
    # Compute a good label
    lcz_df$label <- base::paste0(
      base::round(lcz_df$area_perc, 1), "\u0025\n"
    )

    graph <-
      ggplot2::ggplot(lcz_df, ggplot2::aes(x = 1, y = .data$area_perc, fill = lcz, label = .data$label)) +
      ggplot2::geom_bar(position = "fill", stat = "identity", width = 0.3, key_glyph = ggplot2::draw_key_dotplot, color = NA, lwd = 0.2) +
      ggplot2::geom_text(position = ggplot2::position_fill(vjust = 0.5), stat = "identity", size = 5, check_overlap = TRUE, color = "white") +
      ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 12, linetype = "solid", color = NA))) +
      ggplot2::coord_radial(theta = "y", expand = FALSE, start = -pi / 1.5, direction = -1) +
      ggplot2::theme_void() +
      ggplot2::labs(...) +
      # Add the labels
      ggplot2::annotate("text",
        x = 1.2, y = 0.8, size = 5, fontface = "bold", hjust = 0.1,
        label = base::paste0("Total: ", base::sum(lcz_df$area_km2), " km\u00B2")
      ) +
      ggplot2::theme(
        legend.key.spacing.y = ggplot2::unit(-1.4, "line"),
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(size = 22),
        plot.subtitle = ggplot2::element_text(size = 20),
        #plot.caption.position = "plot",
        legend.text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_text(size = 14, hjust = 0.2, vjust = -1, face = "bold"),
        legend.spacing.y = ggplot2::unit(0.01, "cm"),
        plot.caption = ggplot2::element_text(color = "grey40", size = 10, hjust = 0)
      )

    if (!show_legend) {
      graph <- graph + ggplot2::theme(legend.position = "none")
    }
  }

  if (plot_type == "donut") {
    # Compute a good label
    lcz_df$label <- base::paste0(
      base::round(lcz_df$area_perc, 1), "\u0025\n"
    )
    graph <-
      ggplot2::ggplot(lcz_df, ggplot2::aes(x = 1, y = .data$area_perc, fill = lcz, label = .data$label)) +
      ggplot2::geom_bar(position = "fill", stat = "identity", width = 0.3, key_glyph = ggplot2::draw_key_dotplot, color = "white", lwd = 0.2) +
      ggplot2::geom_text(position = ggplot2::position_fill(vjust = 0.5), stat = "identity", size = 4, check_overlap = TRUE, color = "white") +
      ggplot2::scale_fill_manual(values = color_values, name = "LCZ class", labels = lcz.lables) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 12, linetype = "solid", color = NA))) +
      ggplot2::annotate(
        geom = "text", x = 0.5, y = 1, size = 5, fontface = "bold",
        label = base::paste0("Total:\n", base::sum(lcz_df$area_km2), " km\u00B2")
      ) +
      ggplot2::coord_radial(theta = "y", expand = FALSE, start = -pi / 1.5, direction = -1) +
      ggplot2::xlim(0.5, 1.5) +
      ggplot2::theme_void() +
      ggplot2::labs(...) +
      ggplot2::theme(
        # legend.position = "left",
        legend.margin = ggplot2::margin(0, -5, 0, 0),
        legend.key.spacing.y = ggplot2::unit(-1.3, "line"),
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(size = 22),
        plot.subtitle = ggplot2::element_text(size = 20),
        plot.caption.position = "plot",
        legend.text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_text(size = 14, hjust = 0.2, vjust = -1, face = "bold"),
        legend.spacing.y = ggplot2::unit(0.01, "cm"),
        plot.caption = ggplot2::element_text(color = "grey40", size = 10, hjust = 0)
      )

    if (!show_legend) {
      graph <- graph + ggplot2::theme(legend.position = "none")
    }
  }

  if (isave == TRUE && plot_type == "bar") {
    folder <- base::paste0("LCZ4r_output/")
    if (!base::dir.exists(folder)) {
      base::dir.create(folder)
    }
    file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_area_bar.", save_extension)
    ggplot2::ggsave(file.1, graph,
      height = 7, width = 12,
      dpi = 600
    )
    file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_area_df.csv")
    utils::write.csv(lcz_df, file.2)
    base::message("Looking at your files in the path:", base::paste0(
      getwd(),
      "/", folder
    ))
  }

  if (isave == TRUE && plot_type == "pie") {
    folder <- base::paste0("LCZ4r_output/")
    if (!base::dir.exists(folder)) {
      base::dir.create(folder)
    }

    file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_area_pie.", save_extension)
    ggplot2::ggsave(file.1, graph, height = 5, width = 7, dpi = 600)
    file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_area_df.csv")
    utils::write.csv(lcz_df, file.2)
    base::message("Looking at your files in the path:", base::paste0(
      getwd(),
      "/", folder
    ))
  }

  if (isave == TRUE && plot_type == "donut") {
    folder <- base::paste0("LCZ4r_output/")
    if (!base::dir.exists(folder)) {
      base::dir.create(folder)
    }
    file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_area_donut.", save_extension)
    ggplot2::ggsave(file.1, graph, scale = 1.5,
      height = 6, width = 8,
      dpi = 600
    )
    file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_area_df.csv")
    utils::write.csv(lcz_df, file.2)
    base::message("Looking at your files in the path:", base::paste0(
      getwd(),
      "/", folder
    ))
  }

  if (iplot == FALSE) {
    return(lcz_df)
  } else {
    return(graph)
  }
}
