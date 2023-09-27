
#' Plot LCZ parameter map
#'
#' This function plots the parameters of an LCZ (Local Climate Zone) map.
#'
#' @param x The LCZ map in SpatRaster format.
#' @param iSelect Character vector. Specify one or more parameter names to retrieve specific
#'                parameters. For example, "SVF.min" to get the minimum Sky View Factor,
#'                or c("SVF.min", "veg.frac.max") to select multiple parameters.
#' @param isave Save the plot into your directory.
#' @param isubtitle Specify your name area
#' @param all Salve all parameters into LCZ4r_output
#'
#' @return A plot of the LCZ parameters
#'
#' @export
#'
#' @examples
#'
#' #myplot_par <- plotLCZparameters(x = lcz_map, iSelect = "SVF.min", isubtitle = "Your City")
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for getLCZparameters() to obtain an LCZ parameters
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


plotLCZparameters <- function(x, iSelect = "", isubtitle = "Your City", all = FALSE, isave = FALSE) {

  # # Validate inputs
  # if (is.null(x) & is.null(x)) {
  #   stop("Error: provide either a raster or raster stack from getLCZparameters()")
  # } else if (!is.null(iSelect) & !is.character(iSelect)) {
  #   stop("Error: city input must be a character string")
  # } else if (!is.null(x) & !inherits(x, "raster")) {
  #   stop("Error: x input must be raster stack object of raster package. Please, use the getLCZparameters(x, iStack = TRUE, iShp = FALSE")
  # }

  LCZpar <- {{x}}[[-1]]

  take_names <- function(x) {
    if(x == "SVF.min") return("Minimum Sky View Factor")
    if(x == "SVF.max") return("Maximum Sky View Factor")
    if(x == "SVF.mean") return("Mean Sky View Factor")
    if(x == "aspect.ratio.min") return("Minimum Aspect Ratio")
    if(x == "aspect.ratio.max") return("Maximum Aspect Ratio")
    if(x == "aspect.ratio.mean") return("Mean Aspect Ratio")
    if(x == "build.frac.min") return("Minimum Building Surface Fraction")
    if(x == "build.frac.max") return("Maximum Building Surface Fraction")
    if(x == "build.frac.mean") return("Mean Building Surface Fraction")
    if(x == "imp.frac.min") return("Minimum Impervious Surface Fraction")
    if(x == "imp.frac.max") return("Maximum Impervious Surface Fraction")
    if(x == "imp.frac.mean") return("Mean Impervious Surface Fraction")
    if(x == "veg.frac.min") return("Minimum Pervious Surface Fraction")
    if(x == "veg.frac.max") return("Maximum Pervious Surface Fraction")
    if(x == "veg.frac.mean") return("Mean Pervious Surface Fraction")
    if(x == "tree.frac.min") return("Minimum Tree Surface Fraction")
    if(x == "tree.frac.max") return("Maximum Tree Surface Fraction")
    if(x == "tree.frac.mean") return("Mean Tree Surface Fraction")
    if(x == "height.roug.min") return("Minimum Height Roughness Elements")
    if(x == "height.roug.max") return("Maximum Height Roughness Elements")
    if(x == "height.roug.mean") return("Mean Height Roughness Elements")
    if(x == "terra.roug.min") return("Minimum Terrain Roughness class")
    if(x == "terra.roug.max") return("Maximum Terrain Roughness Class")
    if(x == "terra.roug.mean") return("Mean Terrain Roughness Class")
    if(x == "surf.admit.min") return("Minimum Surface Admittance")
    if(x == "surf.admit.max") return("Maximum Surface Admittance")
    if(x == "surf.admit.mean") return("Mean Surface Admittance")
    if(x == "surf.albedo.min") return("Minimum Surface Albedo")
    if(x == "surf.albedo.max") return("Maximum Surface Aldedo")
    if(x == "surf.albedo.mean") return("Mean Surface Albedo")
    if(x == "antrop.heat.min") return("Minimum Anthropogenic Heat Outupt")
    if(x == "antrop.heat.max") return("Maximum Anthropogenic Heat Outupt")
    if(x == "antrop.heat.mean") return("Mean Anthropogenic Heat Outupt")
    if(x == "z0") return("Roughness Lenght")
  }

  take_unit <- function(x) {

    if(x == "SVF.min") return("[0-1]")
    if(x == "SVF.max") return("[0-1]")
    if(x == "SVF.mean") return("[0-1]")
    if(x == "aspect.ratio.min") return("[%]")
    if(x == "aspect.ratio.max") return("[%]")
    if(x == "aspect.ratio.mean") return("[%]")
    if(x == "build.frac.min") return("[%]")
    if(x == "build.frac.max") return("[%]")
    if(x == "build.frac.mean") return("[%]")
    if(x == "imp.frac.min") return("[%]")
    if(x == "imp.frac.max") return("[%]")
    if(x == "imp.frac.mean") return("[%]")
    if(x == "veg.frac.min") return("[%]")
    if(x == "veg.frac.max") return("[%]")
    if(x == "veg.frac.mean") return("[%]")
    if(x == "tree.frac.min") return("[%]")
    if(x == "tree.frac.max") return("[%]")
    if(x == "tree.frac.mean") return("[%]")
    if(x == "height.roug.min") return("[m]")
    if(x == "height.roug.max") return("[m]")
    if(x == "height.roug.mean") return("[m]")
    if(x == "terra.roug.min") return("[m]")
    if(x == "terra.roug.max") return("[m]")
    if(x == "terra.roug.mean") return("[m]")
    if(x == "surf.admit.min") return("[J/m/s/K]")
    if(x == "surf.admit.max") return("[J/m/s/K]")
    if(x == "surf.admit.mean") return("[J/m/s/K]")
    if(x == "surf.albedo.min") return("[ratio]")
    if(x == "surf.albedo.max") return("[ratio]")
    if(x == "surf.albedo.mean") return("[ratio]")
    if(x == "antrop.heat.min") return("[W/m2]")
    if(x == "antrop.heat.max") return("[W/m2]")
    if(x == "antrop.heat.mean") return("[W/m2]")
    if(x == "z0") return("[m]")
  }

  take_color <- function(x) {

    if(x == "SVF.min") return("magma")
    if(x == "SVF.max") return("magma")
    if(x == "SVF.mean") return("magma")
    if(x == "aspect.ratio.min") return("inferno")
    if(x == "aspect.ratio.max") return("inferno")
    if(x == "aspect.ratio.mean") return("inferno")
    if(x == "build.frac.min") return("cividis")
    if(x == "build.frac.max") return("cividis")
    if(x == "build.frac.mean") return("cividis")
    if(x == "imp.frac.min") return("plasma")
    if(x == "imp.frac.max") return("plasma")
    if(x == "imp.frac.mean") return("plasma")
    if(x == "veg.frac.min") return("viridis")
    if(x == "veg.frac.max") return("viridis")
    if(x == "veg.frac.mean") return("viridis")
    if(x == "tree.frac.min") return("viridis")
    if(x == "tree.frac.max") return("viridis")
    if(x == "tree.frac.mean") return("viridis")
    if(x == "height.roug.min") return("cividis")
    if(x == "height.roug.max") return("cividis")
    if(x == "height.roug.mean") return("cividis")
    if(x == "terra.roug.min") return("magma")
    if(x == "terra.roug.max") return("magma")
    if(x == "terra.roug.mean") return("magma")
    if(x == "surf.admit.min") return("cividis")
    if(x == "surf.admit.max") return("cividis")
    if(x == "surf.admit.mean") return("cividis")
    if(x == "surf.albedo.min") return("plasma")
    if(x == "surf.albedo.max") return("plasma")
    if(x == "surf.albedo.mean") return("plasma")
    if(x == "antrop.heat.min") return("inferno")
    if(x == "antrop.heat.max") return("inferno")
    if(x == "antrop.heat.mean") return("inferno")
    if(x == "z0") return("cividis")

  }
  names_par <- tibble::as_tibble(names(LCZpar))
  names_par$name <- base::sapply(names_par$value, take_names)
  names_par$unit <- base::sapply(names_par$value, take_unit)
  names_par$color <- base::sapply(names_par$value, take_color)

  if(all == TRUE) {

    for (i in 1:raster::nlayers(LCZpar)) {

      # Convert the raster layer to a data frame
      parameter_df <- terra::as.data.frame(LCZpar[[i]], xy=TRUE) %>%
        tidyr::drop_na()%>%
        purrr::set_names(c("x", "y", "values"))

      #Plot the lcz parameters
      fig_par <- ggplot2::ggplot() +
        ggplot2::geom_raster(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
        ggplot2::scale_fill_viridis_c(option = paste0(names_par$color[i]), name=paste0(names_par$unit[i]))+
        ggplot2::labs(title = paste0(names_par$name[i]),
                      subtitle = isubtitle,
                      caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022) and Stewart and Oke (2012)") +
        ggplot2::coord_equal(expand = TRUE)+
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "#3f1651", size = 18, face = "bold", hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(color = "#3f1651", size = 18, hjust = 0.5),
                       plot.background = ggplot2::element_rect(fill = "white"),
                       legend.title = ggplot2::element_text(size = 16, color = "black", face = "bold"),
                       legend.text = ggplot2::element_text(size = 16, color = "black"),
                       plot.caption = ggplot2::element_text(colour = "grey60", size = 9), # move caption to the left
                       axis.line = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                       #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
                       plot.margin = ggplot2::margin(1, 1, 1, 1))

      if(isave == TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder, names_par$value[i], ".png")
        ggplot2::ggsave(file, fig_par, height = 7, width = 10, units="in", dpi=300)

      }

    }

    return(fig_par)

  }

  if(!base::is.null(iSelect)){


    if(length(iSelect)>1) {

      #Select the raster
      select_raster <- LCZpar[[{{iSelect}}]]
      names_par_select <- tibble::as_tibble(names(select_raster))
      names_par_select$name <- base::sapply(names_par_select$value, take_names)
      names_par_select$unit <- base::sapply(names_par_select$value, take_unit)
      names_par_select$color <- base::sapply(names_par_select$value, take_color)

      for (i in 1:raster::nlayers(select_raster)) {

        # Convert the raster layer to a data frame
        parameter_df <- terra::as.data.frame(select_raster[[i]], xy=TRUE) %>%
          tidyr::drop_na() %>%
          purrr::set_names(c("x", "y", "values"))

        #Plot the lcz parameters
        fig_par <- ggplot2::ggplot() +
          ggplot2::geom_raster(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
          ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color[i]), name=paste0(names_par_select$unit[i]))+
          ggplot2::labs(title = paste0(names_par_select$name[i]),
                        subtitle = isubtitle,
                        caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022) and Stewart and Oke (2012)") +
          ggplot2::coord_equal(expand = TRUE)+
          ggplot2::theme_void() +
          ggplot2::theme(plot.title = ggplot2::element_text(color = "#3f1651", size = 18, face = "bold", hjust = 0.5),
                         plot.subtitle = ggplot2::element_text(color = "#3f1651", size = 18, hjust = 0.5),
                         plot.background = ggplot2::element_rect(fill = "white"),
                         legend.title = ggplot2::element_text(size = 16, color = "black", face = "bold"),
                         legend.text = ggplot2::element_text(size = 16, color = "black"),
                         plot.caption = ggplot2::element_text(colour = "grey60", size = 9), # move caption to the left
                         axis.line = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                         #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
                         plot.margin = ggplot2::margin(1, 1, 1, 1))

        if(isave == TRUE) {

          # Create a folder name using paste0
          folder <- base::paste0("LCZ4r_output/")

          # Check if the folder exists
          if (!base::dir.exists(folder)) {
            # Create the folder if it does not exist
            base::dir.create(folder)
          }

          file <- base::paste0(folder, names_par_select$value[i], ".png")
          ggplot2::ggsave(file, fig_par, height = 7, width = 10, units="in", dpi=300)

        }

      }

      return(fig_par)

    } else {

      select_raster <- LCZpar[[{{iSelect}}]]
      names_par_select <- tibble::as_tibble(names(select_raster))
      names_par_select$name <- base::sapply(names_par_select$value, take_names)
      names_par_select$unit <- base::sapply(names_par_select$value, take_unit)
      names_par_select$color <- base::sapply(names_par_select$value, take_color)

      # Convert the raster layer to a data frame
      parameter_df <- terra::as.data.frame(select_raster, xy=TRUE) %>%
        tidyr::drop_na() %>%
        purrr::set_names(c("x", "y", "values"))

      #Plot the lcz parameters
      fig_par <- ggplot2::ggplot() +
        ggplot2::geom_raster(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
        ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color), name=paste0(names_par_select$unit))+
        ggplot2::labs(title = paste0(names_par_select$name),
                      subtitle = isubtitle,
                      caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022) and Stewart and Oke (2012)") +
        ggplot2::coord_equal(expand = TRUE)+
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "#3f1651", size = 18, face = "bold", hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(color = "#3f1651", size = 18, hjust = 0.5),
                       plot.background = ggplot2::element_rect(fill = "white"),
                       legend.title = ggplot2::element_text(size = 16, color = "black", face = "bold"),
                       legend.text = ggplot2::element_text(size = 16, color = "black"),
                       plot.caption = ggplot2::element_text(colour = "grey60", size = 9), # move caption to the left
                       axis.line = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                       #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
                       plot.margin = ggplot2::margin(1, 1, 1, 1))

      if(isave == TRUE) {

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder, names_par_select$value, ".png")
        ggplot2::ggsave(file, fig_par, height = 7, width = 10, units="in", dpi=300)

      }

    }

    return(fig_par)

  }


}



