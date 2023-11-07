
#' Plot LCZ parameter map
#'
#' This function plots the parameters of an LCZ (Local Climate Zone) map.
#'
#' @param x The LCZ map in SpatRaster format.
#' @param iselect Character vector. Specify one or more parameter names to retrieve specific
#'                parameters. For example, "SVF1" to get the minimum Sky View Factor,
#'                or c("z0", "VEG3") to select multiple parameters.
#'                See https://bymaxanjos.github.io/LCZ4r/articles/Introd_to_LCZ4r.html
#' @param isave Logical, indicating whether to save the plot to your directory. Default is FALSE.
#' @param isubtitle Character, specifying a subtitle for the plot.
#' @param all Logical, specifying whether to save all selected parameters into LCZ4r_output. Default is FALSE.
#'
#' @return A plot of the selected LCZ parameters.
#'
#' @export
#'
#' @examples
#'
#' # Plot the minimum Sky View Factor (SVF1) for your city
#' #lcz_plot_parameters(x = lcz_map, iselect = "SVF1, isubtitle = "Rio de Janeiro")
#'
#' # Plot multiple parameters and save them to the LCZ4r_output directory
#' #lcz_plot_parameters(x = lcz_map, iselect = c("SVF1", "HEI1"))
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_parameters() to obtain LCZ parameters.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_plot_parameters <- function(x, iselect = "", isubtitle = "", all = FALSE, isave = FALSE) {

  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster stack object of raster package. Please, use the lcz_get_map(x, iStack = TRUE, iShp = FALSE")
  } else if (!is.null(x) & !inherits(x, "RasterStack")) {
    stop("The input must be raster stack object of raster package. Please, use the lcz_get_map(x, iStack = TRUE, iShp = FALSE")
  }

  LCZpar <- {{x}}[[-1]]

  take_names <- function(x) {
    if(x == "SVF1") return("Minimum Sky View Factor")
    if(x == "SVF2") return("Maximum Sky View Factor")
    if(x == "SVF3") return("Mean Sky View Factor")
    if(x == "ASP1") return("Minimum Aspect Ratio")
    if(x == "ASP2") return("Maximum Aspect Ratio")
    if(x == "ASP3") return("Mean Aspect Ratio")
    if(x == "BUI1") return("Minimum Building Surface Fraction")
    if(x == "BUI2") return("Maximum Building Surface Fraction")
    if(x == "BUI3") return("Mean Building Surface Fraction")
    if(x == "IMP1") return("Minimum Impervious Surface Fraction")
    if(x == "IMP2") return("Maximum Impervious Surface Fraction")
    if(x == "IMP3") return("Mean Impervious Surface Fraction")
    if(x == "VEG1") return("Minimum Pervious Surface Fraction")
    if(x == "VEG2") return("Maximum Pervious Surface Fraction")
    if(x == "VEG3") return("Mean Pervious Surface Fraction")
    if(x == "TRE1") return("Minimum Tree Surface Fraction")
    if(x == "TRE2") return("Maximum Tree Surface Fraction")
    if(x == "TRE3") return("Mean Tree Surface Fraction")
    if(x == "HEI1") return("Minimum Height Roughness Elements")
    if(x == "HEI2") return("Maximum Height Roughness Elements")
    if(x == "HEI3") return("Mean Height Roughness Elements")
    if(x == "TER1") return("Minimum Terrain Roughness class")
    if(x == "TER2") return("Maximum Terrain Roughness Class")
    if(x == "TER3") return("Mean Terrain Roughness Class")
    if(x == "ADM1") return("Minimum Surface Admittance")
    if(x == "ADM2") return("Maximum Surface Admittance")
    if(x == "ADM3") return("Mean Surface Admittance")
    if(x == "ALB1") return("Minimum Surface Albedo")
    if(x == "ALB2") return("Maximum Surface Aldedo")
    if(x == "ALB3") return("Mean Surface Albedo")
    if(x == "ANT1") return("Minimum Anthropogenic Heat Outupt")
    if(x == "ANT2") return("Maximum Anthropogenic Heat Outupt")
    if(x == "ANT3") return("Mean Anthropogenic Heat Outupt")
    if(x == "z0") return("Roughness Lenght")
  }

  take_unit <- function(x) {

    if(x == "SVF1") return("[0 - 1]")
    if(x == "SVF2") return("[0 - 1]")
    if(x == "SVF3") return("[0 - 1]")
    if(x == "ASP1") return("[0 – 3+]")
    if(x == "ASP2") return("[0 – 3+]")
    if(x == "ASP3") return("[0 – 3+]")
    if(x == "BUI1") return("[%]")
    if(x == "BUI2") return("[%]")
    if(x == "BUI3") return("[%]")
    if(x == "IMP1") return("[%]")
    if(x == "IMP2") return("[%]")
    if(x == "IMP3") return("[%]")
    if(x == "VEG1") return("[%]")
    if(x == "VEG2") return("[%]")
    if(x == "VEG3") return("[%]")
    if(x == "TRE1") return("[%]")
    if(x == "TRE2") return("[%]")
    if(x == "TRE3") return("[%]")
    if(x == "HEI1") return("[m]")
    if(x == "HEI2") return("[m]")
    if(x == "HEI3") return("[m]")
    if(x == "TER1") return("[m]")
    if(x == "TER2") return("[m]")
    if(x == "TER3") return("[m]")
    if(x == "ADM1") return("[J m-2 s1/2 K-1]")
    if(x == "ADM2") return("[J m-2 s1/2 K-1]")
    if(x == "ADM3") return("[J m-2 s1/2 K-1]")
    if(x == "ALB1") return("[0 – 0.5]")
    if(x == "ALB2") return("[0 – 0.5]")
    if(x == "ALB3") return("[0 – 0.5]")
    if(x == "ANT1") return("[W m-2]")
    if(x == "ANT2") return("[W m-2]")
    if(x == "ANT3") return("[W m-2]")
    if(x == "z0") return("[m]")
  }

  take_color <- function(x) {

    if(x == "SVF1") return("magma")
    if(x == "SVF2") return("magma")
    if(x == "SVF3") return("magma")
    if(x == "ASP1") return("inferno")
    if(x == "ASP2") return("inferno")
    if(x == "ASP3") return("inferno")
    if(x == "BUI1") return("cividis")
    if(x == "BUI2") return("cividis")
    if(x == "BUI3") return("cividis")
    if(x == "IMP1") return("plasma")
    if(x == "IMP2") return("plasma")
    if(x == "IMP3") return("plasma")
    if(x == "VEG1") return("viridis")
    if(x == "VEG2") return("viridis")
    if(x == "VEG3") return("viridis")
    if(x == "TRE1") return("viridis")
    if(x == "TRE2") return("viridis")
    if(x == "TRE3") return("viridis")
    if(x == "HEI1") return("cividis")
    if(x == "HEI2") return("cividis")
    if(x == "HEI3") return("cividis")
    if(x == "TER1") return("magma")
    if(x == "TER2") return("magma")
    if(x == "TER3") return("magma")
    if(x == "ADM1") return("cividis")
    if(x == "ADM2") return("cividis")
    if(x == "ADM3") return("cividis")
    if(x == "ALB1") return("plasma")
    if(x == "ALB2") return("plasma")
    if(x == "ALB3") return("plasma")
    if(x == "ANT1") return("inferno")
    if(x == "ANT2") return("inferno")
    if(x == "ANT3") return("inferno")
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
        ggplot2::geom_tile(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
        ggplot2::scale_fill_viridis_c(option = paste0(names_par$color[i]), name=paste0(names_par$unit[i]))+
        ggplot2::labs(title = paste0(names_par$name[i]),
                      subtitle = isubtitle,
                      caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022) and Stewart and Oke (2012)") +
        ggplot2::coord_equal(expand = TRUE, clip = "off")+
        ggplot2::theme_void() +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(color = "black", size = 18, hjust = 0.5),
                       plot.background = ggplot2::element_blank(),
                       legend.title = ggplot2::element_text(size = 16, color = "black", face = "bold"),
                       legend.text = ggplot2::element_text(size = 16, color = "black"),
                       plot.caption = ggplot2::element_text(colour = "grey30", size = 9, hjust = 0),# move caption to the left
                       axis.line = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                       #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
                       plot.margin = ggplot2::margin(25, 25, 10, 25))

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

    base::cat("Wow! You've successfully salved all the LCZ parameter maps into you pc.\n")
    return(fig_par)

  }

  if(!base::is.null(iselect)){


    if(length(iselect)>1) {

      #Select the raster
      select_raster <- LCZpar[[{{iselect}}]]
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
          ggplot2::geom_tile(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
          ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color[i]), name=paste0(names_par_select$unit[i]))+
          ggplot2::labs(title = paste0(names_par_select$name[i]),
                        subtitle = isubtitle,
                        caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022) and Stewart and Oke (2012)") +
          ggplot2::coord_equal(expand = TRUE, clip = "off")+
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
                         #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                         #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
                         plot.margin = ggplot2::margin(25, 25, 10, 25))

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

      base::cat("Wow! You've successfully salved the selected LCZ parameter maps into you pc.\n")
      return(fig_par)


    } else {

      select_raster <- LCZpar[[{{iselect}}]]
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
        ggplot2::geom_tile(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
        ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color), name=paste0(names_par_select$unit))+
        ggplot2::labs(title = paste0(names_par_select$name),
                      subtitle = isubtitle,
                      caption = "Source:LCZ4r, https://github.com/ByMaxAnjos/LCZ4r\nData:Demuzere et al.(2022) and Stewart and Oke (2012)") +
        ggplot2::coord_equal(expand = TRUE, clip = "off")+
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
                       #panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
                       #panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
                       plot.margin = ggplot2::margin(25, 25, 10, 25))

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

    base::cat("Wow! You've successfully salved the selected LCZ parameter map into you pc.\n")
    return(fig_par)

  }


}



