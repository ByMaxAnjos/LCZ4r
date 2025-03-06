#' Visualize LCZ parameters
#'
#' This function plots the parameters of an Local Climate Zone map.
#'
#' @param x The LCZ map in SpatRaster in a stack format.
#' @param iselect Code vector. Specify one or more parameter:
#' \itemize{
#'   \item \strong{SVFmean}: Mean Sky View Factor
#'   \item \strong{SVFmax}: Maximum Sky View Factor
#'   \item \strong{SVFmin}: Minimum Sky View Factor
#'   \item \strong{ARmean}: Mean Aspect Ratio
#'   \item \strong{ARmax}: Maximum Aspect Ratio
#'   \item \strong{ARmin}: Minimum Aspect Ratio
#'   \item \strong{BSFmean}: Mean Building Surface Fraction
#'   \item \strong{BSFmax}: Maximum Building Surface Fraction
#'   \item \strong{BSFmin}: Minimum Building Surface Fraction
#'   \item \strong{ISFmean}: Mean Impervious Surface Fraction
#'   \item \strong{ISFmax}: Maximum Impervious Surface Fraction
#'   \item \strong{ISFmin}: Minimum Impervious Surface Fraction
#'   \item \strong{PSFmean}: Mean Vegetation Surface Fraction
#'   \item \strong{PSFmax}: Maximum Vegetation Surface Fraction
#'   \item \strong{PSFmin}: Minimum Vegetation Surface Fraction
#'   \item \strong{TSFmean}: Mean Tree Surface Fraction
#'   \item \strong{TSFmax}: Maximum Tree Surface Fraction
#'   \item \strong{TSFmin}: Minimum Tree Surface Fraction
#'   \item \strong{HREmean}: Mean Height Roughness Elements
#'   \item \strong{HREmax}: Maximum Height Roughness Elements
#'   \item \strong{HREmin}: Minimum Height Roughness Elements
#'   \item \strong{TRCmean}: Mean Terrain Roughness class
#'   \item \strong{TRCmax}: Maximum Terrain Roughness class
#'   \item \strong{TRCmin}: Minimum Terrain Roughness class
#'   \item \strong{SADmean}: Mean Surface Admittance
#'   \item \strong{SADmax}: Maximum Surface Admittance
#'   \item \strong{SADmin}: Minimum Surface Admittance
#'   \item \strong{SALmean}: Mean Surface Albedo
#'   \item \strong{SALmax}: Maximum Surface Albedo
#'   \item \strong{SALmin}: Minimum Surface Albedo
#'   \item \strong{AHmean}: Mean Anthropogenic Heat Outupt
#'   \item \strong{AHmax}: Maximum Anthropogenic Heat Outupt
#'   \item \strong{AHmin}: Minimum Anthropogenic Heat Outupt
#'   \item \strong{z0}: Roughness Lenght class
#' }
#' @param isave Logical, indicating whether to save the plot to your directory. Default is FALSE.
#' @param save_extension File format for saving the plot. Options: "png", "jpg", "jpeg", "tif", "pdf", "svg" (default is "png").
#' @param inclusive Set to TRUE to a colorblind-friendly palette.
#' @param all Logical, specifying whether to save all selected parameters into LCZ4r_output. Default is FALSE.
#' @param ... An optional modify axis, legend, and plot labels: subtitle, and caption. It does not work with title.
#'
#' @return A plot of the selected LCZ parameters in ggplot2 format
#'
#'#'@references
#' Stewart, I., and T. Oke, 2012: Local climate zones for urban temperature studies. Bull. Amer. Meteor. Soc., 93, 1879–1900. https://doi.org/10.1175/BAMS-D-11-00019.1
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot the mean Sky View Factor (SVFmean) for your city
#' lcz_plot_parameters(lcz_map, iselect = "SVFmean")
#'
#' # Plot multiple parameters and save them to the LCZ4r_output directory
#' lcz_plot_parameters(lcz_map, iselect = c("BSFmean", "AHmean"))
#' }
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for lcz_get_parameters() to obtain LCZ parameters.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_plot_parameters <- function(x,
                                iselect = "",
                                all = FALSE,
                                inclusive = FALSE,
                                isave = FALSE,
                                save_extension = "png",
                                ...) {
  # Validate inputs
  if (is.null(x)) {
    stop("The input must be raster stack object of raster package. Please, use the lcz_get_map(x, istack = TRUE, ishp = FALSE")
  } else if (!is.null(x) & !inherits(x, "SpatRaster")) {
    x <- terra::rast({{ x }})
  }

  LCZpar <- {{ x }}[[-1]]

  take_names <- function(x) {
    if (x == "SVFmin") {
      return("Minimum Sky View Factor")
    }
    if (x == "SVFmax") {
      return("Maximum Sky View Factor")
    }
    if (x == "SVFmean") {
      return("Mean Sky View Factor")
    }
    if (x == "ARmin") {
      return("Minimum Aspect Ratio")
    }
    if (x == "ARmax") {
      return("Maximum Aspect Ratio")
    }
    if (x == "ARmean") {
      return("Mean Aspect Ratio")
    }
    if (x == "BSFmin") {
      return("Minimum Building Surface Fraction")
    }
    if (x == "BSFmax") {
      return("Maximum Building Surface Fraction")
    }
    if (x == "BSFmean") {
      return("Mean Building Surface Fraction")
    }
    if (x == "ISFmin") {
      return("Minimum Impervious Surface Fraction")
    }
    if (x == "ISFmax") {
      return("Maximum Impervious Surface Fraction")
    }
    if (x == "ISFmean") {
      return("Mean Impervious Surface Fraction")
    }
    if (x == "PSFmin") {
      return("Minimum Pervious Surface Fraction")
    }
    if (x == "PSFmax") {
      return("Maximum Pervious Surface Fraction")
    }
    if (x == "PSFmean") {
      return("Mean Pervious Surface Fraction")
    }
    if (x == "TSFmin") {
      return("Minimum Tree Surface Fraction")
    }
    if (x == "TSFmax") {
      return("Maximum Tree Surface Fraction")
    }
    if (x == "TSFmean") {
      return("Mean Tree Surface Fraction")
    }
    if (x == "HREmin") {
      return("Minimum Height Roughness Elements")
    }
    if (x == "HREmax") {
      return("Maximum Height Roughness Elements")
    }
    if (x == "HREmean") {
      return("Mean Height Roughness Elements")
    }
    if (x == "TRCmin") {
      return("Minimum Terrain Roughness class")
    }
    if (x == "TRCmax") {
      return("Maximum Terrain Roughness Class")
    }
    if (x == "TRCmean") {
      return("Mean Terrain Roughness Class")
    }
    if (x == "SADmin") {
      return("Minimum Surface Admittance")
    }
    if (x == "SADmax") {
      return("Maximum Surface Admittance")
    }
    if (x == "SADmean") {
      return("Mean Surface Admittance")
    }
    if (x == "SALmin") {
      return("Minimum Surface Albedo")
    }
    if (x == "SALmax") {
      return("Maximum Surface Albedo")
    }
    if (x == "SALmean") {
      return("Mean Surface Albedo")
    }
    if (x == "AHmin") {
      return("Minimum Anthropogenic Heat Outupt")
    }
    if (x == "AHmax") {
      return("Maximum Anthropogenic Heat Outupt")
    }
    if (x == "AHmean") {
      return("Mean Anthropogenic Heat Outupt")
    }
    if (x == "z0") {
      return("Roughness Lenght")
    }
  }

  take_unit <- function(x) {
    if (x == "SVFmin") {
      return("[0 - 1]")
    }
    if (x == "SVFmax") {
      return("[0 - 1]")
    }
    if (x == "SVFmean") {
      return("[0 - 1]")
    }
    if (x == "ARmin") {
      return("[0 - 3]")
    }
    if (x == "ARmax") {
      return("[0 - 3]")
    }
    if (x == "ARmean") {
      return("[0 - 3]")
    }
    if (x == "BSFmin") {
      return("[%]")
    }
    if (x == "BSFmax") {
      return("[%]")
    }
    if (x == "BSFmean") {
      return("[%]")
    }
    if (x == "ISFmin") {
      return("[%]")
    }
    if (x == "ISFmax") {
      return("[%]")
    }
    if (x == "ISFmean") {
      return("[%]")
    }
    if (x == "PSFmin") {
      return("[%]")
    }
    if (x == "PSFmax") {
      return("[%]")
    }
    if (x == "PSFmean") {
      return("[%]")
    }
    if (x == "TSFmin") {
      return("[%]")
    }
    if (x == "TSFmax") {
      return("[%]")
    }
    if (x == "TSFmean") {
      return("[%]")
    }
    if (x == "HREmin") {
      return("[m]")
    }
    if (x == "HREmax") {
      return("[m]")
    }
    if (x == "HREmean") {
      return("[m]")
    }
    if (x == "TRCmin") {
      return("[m]")
    }
    if (x == "TRCmax") {
      return("[m]")
    }
    if (x == "TRCmean") {
      return("[m]")
    }
    if (x == "SADmin") {
      return("[J m-2 s1/2 K-1]")
    }
    if (x == "SADmax") {
      return("[J m-2 s1/2 K-1]")
    }
    if (x == "SADmean") {
      return("[J m-2 s1/2 K-1]")
    }
    if (x == "SALmin") {
      return("[0 - 0.5]")
    }
    if (x == "SALmax") {
      return("[0 - 0.5]")
    }
    if (x == "SALmean") {
      return("[0 - 0.5]")
    }
    if (x == "AHmin") {
      return("[W m-2]")
    }
    if (x == "AHmax") {
      return("[W m-2]")
    }
    if (x == "AHmean") {
      return("[W m-2]")
    }
    if (x == "z0") {
      return("[m]")
    }
  }

  if (inclusive == TRUE) {
    take_color <- function(x) {
      if (x == "SVFmin") {
        return("Archambault")
      }
      if (x == "SVFmax") {
        return("Archambault")
      }
      if (x == "SVFmean") {
        return("Archambault")
      }
      if (x == "ARmin") {
        return("Ingres")
      }
      if (x == "ARmax") {
        return("Ingres")
      }
      if (x == "ARmean") {
        return("Ingres")
      }
      if (x == "BSFmin") {
        return("Cassatt1")
      }
      if (x == "BSFmax") {
        return("Cassatt1")
      }
      if (x == "BSFmean") {
        return("Cassatt1")
      }
      if (x == "ISFmin") {
        return("Cassatt2")
      }
      if (x == "ISFmax") {
        return("Cassatt2")
      }
      if (x == "ISFmean") {
        return("Cassatt2")
      }
      if (x == "PSFmin") {
        return("VanGogh3")
      }
      if (x == "PSFmax") {
        return("VanGogh3")
      }
      if (x == "PSFmean") {
        return("VanGogh3")
      }
      if (x == "TSFmin") {
        return("Hokusai3")
      }
      if (x == "TSFmax") {
        return("Hokusai3")
      }
      if (x == "TSFmean") {
        return("Hokusai3")
      }
      if (x == "HREmin") {
        return("Hokusai2")
      }
      if (x == "HREmax") {
        return("Hokusai2")
      }
      if (x == "HREmean") {
        return("Hokusai2")
      }
      if (x == "TRCmin") {
        return("Pissaro")
      }
      if (x == "TRCmax") {
        return("Pissaro")
      }
      if (x == "TRCmean") {
        return("Pissaro")
      }
      if (x == "SADmin") {
        return("Tam")
      }
      if (x == "SADmax") {
        return("Tam")
      }
      if (x == "SADmean") {
        return("Tam")
      }
      if (x == "SALmin") {
        return("Renoir")
      }
      if (x == "SALmax") {
        return("Renoir")
      }
      if (x == "SALmean") {
        return("Renoir")
      }
      if (x == "AHmin") {
        return("Demuth")
      }
      if (x == "AHmax") {
        return("Demuth")
      }
      if (x == "AHmean") {
        return("Demuth")
      }
      if (x == "z0") {
        return("Troy")
      }
    }
    names_par <- tibble::as_tibble(names(LCZpar))
    names_par$name <- base::sapply(names_par$value, take_names)
    names_par$unit <- base::sapply(names_par$value, take_unit)
    names_par$color <- base::sapply(names_par$value, take_color)
  } else {
    take_color <- function(x) {
      if (x == "SVFmin") {
        return("Archambault")
      }
      if (x == "SVFmax") {
        return("Archambault")
      }
      if (x == "SVFmean") {
        return("Archambault")
      }
      if (x == "ARmin") {
        return("Greek")
      }
      if (x == "ARmax") {
        return("Greek")
      }
      if (x == "ARmean") {
        return("Greek")
      }
      if (x == "BSFmin") {
        return("VanGogh1")
      }
      if (x == "BSFmax") {
        return("VanGogh1")
      }
      if (x == "BSFmean") {
        return("VanGogh1")
      }
      if (x == "ISFmin") {
        return("VanGogh2")
      }
      if (x == "ISFmax") {
        return("VanGogh2")
      }
      if (x == "ISFmean") {
        return("VanGogh2")
      }
      if (x == "PSFmin") {
        return("VanGogh3")
      }
      if (x == "PSFmax") {
        return("VanGogh3")
      }
      if (x == "PSFmean") {
        return("VanGogh3")
      }
      if (x == "TSFmin") {
        return("Hokusai3")
      }
      if (x == "TSFmax") {
        return("Hokusai3")
      }
      if (x == "TSFmean") {
        return("Hokusai3")
      }
      if (x == "HREmin") {
        return("Hokusai2")
      }
      if (x == "HREmax") {
        return("Hokusai2")
      }
      if (x == "HREmean") {
        return("Hokusai2")
      }
      if (x == "TRCmin") {
        return("Pissaro")
      }
      if (x == "TRCmax") {
        return("Pissaro")
      }
      if (x == "TRCmean") {
        return("Pissaro")
      }
      if (x == "SADmin") {
        return("Tam")
      }
      if (x == "SADmax") {
        return("Tam")
      }
      if (x == "SADmean") {
        return("Tam")
      }
      if (x == "SALmin") {
        return("Renoir")
      }
      if (x == "SALmax") {
        return("Renoir")
      }
      if (x == "SALmean") {
        return("Renoir")
      }
      if (x == "AHmin") {
        return("Manet")
      }
      if (x == "AHmax") {
        return("Manet")
      }
      if (x == "AHmean") {
        return("Manet")
      }
      if (x == "z0") {
        return("Troy")
      }
    }
    names_par <- tibble::as_tibble(names(LCZpar))
    names_par$name <- base::sapply(names_par$value, take_names)
    names_par$unit <- base::sapply(names_par$value, take_unit)
    names_par$color <- base::sapply(names_par$value, take_color)
  }


  if (all == TRUE) {
    for (i in 1:terra::nlyr(LCZpar)) {
      # Convert the raster layer to a data frame
      parameter_df <- terra::as.data.frame(LCZpar[[i]], xy = TRUE) %>%
        stats::na.omit() %>%
        dplyr::rename(values=.data$lcz)
        #purrr::set_names(c("x", "y", "values"))

      # Plot the lcz parameters
      fig_par <- ggplot2::ggplot() +
        ggplot2::geom_tile(ggplot2::aes(x = x, y = .data$y, fill = .data$values), data = parameter_df, show.legend = TRUE) +
        # ggplot2::scale_fill_viridis_c(option = paste0(names_par$color[i]), name=paste0(names_par$unit[i]))+
        MetBrewer::scale_fill_met_c(name = paste0(names_par$color[i])) +
        ggplot2::labs(title = paste0(names_par$name[i]), fill = paste0(names_par$unit[i]), ...) +
        ggplot2::coord_fixed() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
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
          # panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
          # panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
          plot.margin = ggplot2::margin(25, 25, 10, 25)
        )

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(getwd(), "/", folder, names_par$value[i], ".", save_extension)
        ggplot2::ggsave(file, fig_par, height = 7, width = 9, units = "in", dpi = 600)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }
    }

    return(fig_par)
  }

  if (!base::is.null(iselect)) {
    if (length(iselect) > 1) {
      # Select the raster
      select_raster <- LCZpar[[{{ iselect }}]]
      names_par_select <- tibble::as_tibble(names(select_raster))
      names_par_select$name <- base::sapply(names_par_select$value, take_names)
      names_par_select$unit <- base::sapply(names_par_select$value, take_unit)
      names_par_select$color <- base::sapply(names_par_select$value, take_color)

      for (i in 1:terra::nlyr(select_raster)) {
        # Convert the raster layer to a data frame
        parameter_df <- terra::as.data.frame(select_raster[[i]], xy = TRUE) %>%
          stats::na.omit() %>%
          dplyr::rename(values=.data$lcz)
          #purrr::set_names(c("x", "y", "values"))

        # Plot the lcz parameters
        fig_par <- ggplot2::ggplot() +
          ggplot2::geom_tile(ggplot2::aes(x = x, y = .data$y, fill = .data$values), data = parameter_df, show.legend = TRUE) +
          # ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color[i]), name=paste0(names_par_select$unit[i]))+
          MetBrewer::scale_fill_met_c(name = paste0(names_par_select$color[i])) +
          ggplot2::labs(title = paste0(names_par_select$name[i]), fill = paste0(names_par_select$unit[i]), ...) +
          ggplot2::coord_fixed() +
          ggplot2::theme_void() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
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
            # panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
            # panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
            plot.margin = ggplot2::margin(25, 25, 10, 25)
          )

        if (isave == TRUE) {
          # Create a folder name using paste0
          folder <- base::paste0("LCZ4r_output/")

          # Check if the folder exists
          if (!base::dir.exists(folder)) {
            # Create the folder if it does not exist
            base::dir.create(folder)
          }

          file <- base::paste0(getwd(), "/", folder, names_par_select$value[i], ".", save_extension)
          ggplot2::ggsave(file, fig_par, height = 7, width = 9, units = "in", dpi = 600)
          base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
        }
      }

      return(fig_par)
    } else {
      select_raster <- LCZpar[[{{ iselect }}]]
      names_par_select <- tibble::as_tibble(names(select_raster))
      names_par_select$name <- base::sapply(names_par_select$value, take_names)
      names_par_select$unit <- base::sapply(names_par_select$value, take_unit)
      names_par_select$color <- base::sapply(names_par_select$value, take_color)

      # Convert the raster layer to a data frame
      parameter_df <- terra::as.data.frame(select_raster, xy = TRUE) %>%
        stats::na.omit() %>%
        dplyr::rename(values=.data$lcz)
        #purrr::set_names(c("x", "y", "values"))

      # Plot the lcz parameters
      fig_par <- ggplot2::ggplot() +
        ggplot2::geom_tile(ggplot2::aes(x = x, y = .data$y, fill = .data$values), data = parameter_df, show.legend = TRUE) +
        # ggplot2::scale_fill_viridis_c(option = paste0(names_par_select$color), name=paste0(names_par_select$unit))+
        MetBrewer::scale_fill_met_c(name = paste0(names_par_select$color)) +
        ggplot2::labs(title = paste0(names_par_select$name), fill = paste0(names_par_select$unit), ...) +
        ggplot2::coord_fixed() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
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
          # panel.grid.major = ggplot2::element_line(color = "white", size = 0.3),
          # panel.grid.minor = ggplot2::element_line(color = "white", size = 0.3),
          plot.margin = ggplot2::margin(25, 25, 10, 25)
        )

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(getwd(), "/", folder, names_par_select$value, ".", save_extension)
        ggplot2::ggsave(file, fig_par, height = 7, width = 9, units = "in", dpi = 600)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }
    }

    return(fig_par)
  }
}



