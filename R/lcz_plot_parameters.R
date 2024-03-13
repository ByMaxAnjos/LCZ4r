
#' Visualize LCZ parameters
#'
#' This function plots the parameters of an Local Climate Zone map.
#'
#' @param x The LCZ map in SpatRaster in a stack format.
#' @param iselect Character vector. Specify one or more parameter names to retrieve specific
#'                parameters. For example, "SVF1" to get the minimum Sky View Factor,
#'                or c("z0", "VEG3") to select multiple parameters.
#' @param isave Logical, indicating whether to save the plot to your directory. Default is FALSE.
#' @param all Logical, specifying whether to save all selected parameters into LCZ4r_output. Default is FALSE.
#' @param ... An optional modify axis, legend, and plot labels: title, subtitle, and caption.
#'
#' @return A plot of the selected LCZ parameters in ggplot2 format
#'
#' @export
#'
#' @examples
#'
#' # Plot the minimum Sky View Factor (SVF1) for your city
#' # lcz_plot_parameters(lcz_map, iselect = "SVF1")
#'
#' # Plot multiple parameters and save them to the LCZ4r_output directory
#' # lcz_plot_parameters(lcz_map, iselect = c("BSF3", "AH3"))
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_parameters() to obtain LCZ parameters.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_plot_parameters <- function(x, iselect = "", all = FALSE, isave = FALSE, ...) {

  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster stack object of raster package. Please, use the lcz_get_map(x, istack = TRUE, ishp = FALSE")
  } else if (!is.null(x) & !inherits(x, "SpatRaster")) {
    x <- terra::rast({{x}})
  }

  LCZpar <- {{x}}[[-1]]

  take_names <- function(x) {
    if(x == "SVF1") return("Minimum Sky View Factor")
    if(x == "SVF2") return("Maximum Sky View Factor")
    if(x == "SVF3") return("Mean Sky View Factor")
    if(x == "AR1") return("Minimum Aspect Ratio")
    if(x == "AR2") return("Maximum Aspect Ratio")
    if(x == "AR3") return("Mean Aspect Ratio")
    if(x == "BSF1") return("Minimum Building Surface Fraction")
    if(x == "BSF2") return("Maximum Building Surface Fraction")
    if(x == "BSF3") return("Mean Building Surface Fraction")
    if(x == "ISF1") return("Minimum Impervious Surface Fraction")
    if(x == "ISF2") return("Maximum Impervious Surface Fraction")
    if(x == "ISF3") return("Mean Impervious Surface Fraction")
    if(x == "PSF1") return("Minimum Pervious Surface Fraction")
    if(x == "PSF2") return("Maximum Pervious Surface Fraction")
    if(x == "PSF3") return("Mean Pervious Surface Fraction")
    if(x == "TSF1") return("Minimum Tree Surface Fraction")
    if(x == "TSF2") return("Maximum Tree Surface Fraction")
    if(x == "TSF3") return("Mean Tree Surface Fraction")
    if(x == "HRE1") return("Minimum Height Roughness Elements")
    if(x == "HRE2") return("Maximum Height Roughness Elements")
    if(x == "HRE3") return("Mean Height Roughness Elements")
    if(x == "TRC1") return("Minimum Terrain Roughness class")
    if(x == "TRC2") return("Maximum Terrain Roughness Class")
    if(x == "TRC3") return("Mean Terrain Roughness Class")
    if(x == "SAD1") return("Minimum Surface Admittance")
    if(x == "SAD2") return("Maximum Surface Admittance")
    if(x == "SAD3") return("Mean Surface Admittance")
    if(x == "SAL1") return("Minimum Surface Albedo")
    if(x == "SAL2") return("Maximum Surface Aldedo")
    if(x == "SAL3") return("Mean Surface Albedo")
    if(x == "AH1") return("Minimum Anthropogenic Heat Outupt")
    if(x == "AH2") return("Maximum Anthropogenic Heat Outupt")
    if(x == "AH3") return("Mean Anthropogenic Heat Outupt")
    if(x == "z0") return("Roughness Lenght")
  }

  take_unit <- function(x) {

    if(x == "SVF1") return("[0 - 1]")
    if(x == "SVF2") return("[0 - 1]")
    if(x == "SVF3") return("[0 - 1]")
    if(x == "AR1") return("[0 - 3]")
    if(x == "AR2") return("[0 - 3]")
    if(x == "AR2") return("[0 - 3]")
    if(x == "BSF1") return("[%]")
    if(x == "BSF2") return("[%]")
    if(x == "BSF3") return("[%]")
    if(x == "ISF1") return("[%]")
    if(x == "ISF2") return("[%]")
    if(x == "ISF3") return("[%]")
    if(x == "PSF1") return("[%]")
    if(x == "PSF2") return("[%]")
    if(x == "PSF3") return("[%]")
    if(x == "TSF1") return("[%]")
    if(x == "TSF2") return("[%]")
    if(x == "TSF3") return("[%]")
    if(x == "HRE1") return("[m]")
    if(x == "HRE2") return("[m]")
    if(x == "HRE3") return("[m]")
    if(x == "TRC1") return("[m]")
    if(x == "TRC2") return("[m]")
    if(x == "TRC3") return("[m]")
    if(x == "SAD1") return("[J m-2 s1/2 K-1]")
    if(x == "SAD2") return("[J m-2 s1/2 K-1]")
    if(x == "SAD3") return("[J m-2 s1/2 K-1]")
    if(x == "SAL1") return("[0 - 0.5]")
    if(x == "SAL2") return("[0 - 0.5]")
    if(x == "SAL3") return("[0 - 0.5]")
    if(x == "AH1") return("[W m-2]")
    if(x == "AH2") return("[W m-2]")
    if(x == "AH3") return("[W m-2]")
    if(x == "z0") return("[m]")
  }

  take_color <- function(x) {

    if(x == "SVF1") return("magma")
    if(x == "SVF2") return("magma")
    if(x == "SVF3") return("magma")
    if(x == "AR1") return("inferno")
    if(x == "AR2") return("inferno")
    if(x == "AR3") return("inferno")
    if(x == "BSF1") return("cividis")
    if(x == "BSF2") return("cividis")
    if(x == "BSF3") return("cividis")
    if(x == "ISF1") return("plasma")
    if(x == "ISF2") return("plasma")
    if(x == "ISF3") return("plasma")
    if(x == "PSF1") return("viridis")
    if(x == "PSF2") return("viridis")
    if(x == "PSF3") return("viridis")
    if(x == "TSF1") return("viridis")
    if(x == "TSF2") return("viridis")
    if(x == "TSF3") return("viridis")
    if(x == "HRE1") return("cividis")
    if(x == "HRE2") return("cividis")
    if(x == "HRE3") return("cividis")
    if(x == "TRC1") return("magma")
    if(x == "TRC2") return("magma")
    if(x == "TRC3") return("magma")
    if(x == "SAD1") return("cividis")
    if(x == "SAD2") return("cividis")
    if(x == "SAD3") return("cividis")
    if(x == "SAL1") return("plasma")
    if(x == "SAL2") return("plasma")
    if(x == "SAL3") return("plasma")
    if(x == "AH1") return("inferno")
    if(x == "AH2") return("inferno")
    if(x == "AH3") return("inferno")
    if(x == "z0") return("cividis")

  }
  names_par <- tibble::as_tibble(names(LCZpar))
  names_par$name <- base::sapply(names_par$value, take_names)
  names_par$unit <- base::sapply(names_par$value, take_unit)
  names_par$color <- base::sapply(names_par$value, take_color)

  if(all == TRUE) {

    for (i in 1:terra::nlyr(LCZpar)) {

      # Convert the raster layer to a data frame
      parameter_df <- terra::as.data.frame(LCZpar[[i]], xy=TRUE) %>%
        tidyr::drop_na()%>%
        purrr::set_names(c("x", "y", "values"))

      #Plot the lcz parameters
      fig_par <- ggplot2::ggplot() +
        ggplot2::geom_tile(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
        ggplot2::scale_fill_viridis_c(option = paste0(names_par$color[i]), name=paste0(names_par$unit[i]))+
        ggplot2::labs(title = paste0(names_par$name[i]),...) +
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
                       legend.spacing.y = ggplot2::unit(0.02, "cm"),
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

    base::cat("Wow! You've salved all the LCZ parameter maps into folder LCZ4r_output.\n")
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

      for (i in 1:terra::nlyr(select_raster)) {

        # Convert the raster layer to a data frame
        parameter_df <- terra::as.data.frame(select_raster[[i]], xy=TRUE) %>%
          tidyr::drop_na() %>%
          purrr::set_names(c("x", "y", "values"))

        #Plot the lcz parameters
        fig_par <- ggplot2::ggplot() +
          ggplot2::geom_tile(ggplot2::aes(x=x, y=.data$y, fill=.data$values), data = parameter_df, show.legend = TRUE) +
          ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color[i]), name=paste0(names_par_select$unit[i]))+
          ggplot2::labs(title = paste0(names_par_select$name[i]),...) +
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
                         legend.spacing.y = ggplot2::unit(0.02, "cm"),
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

      base::cat("Wow! You've salved the",iselect,"LCZ parameter maps into folder LCZ4r_output.\n")
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
        ggplot2::labs(title = paste0(names_par_select$name),...) +
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
                       legend.spacing.y = ggplot2::unit(0.02, "cm"),
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

        base::cat("Wow! You've salved",iselect,"LCZ parameter map into folder LCZ4r_output.\n")

      }

    }

    base::cat("You've generated",iselect,"LCZ parameter map.\n")
    return(fig_par)

  }


}



