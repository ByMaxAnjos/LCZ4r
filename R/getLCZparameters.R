
#' Get LCZ Parameters from LCZ Stewart and Oke (2012)
#'
#' This function extracts 34 Local Climate Zone (LCZ) parameters based on the classification
#' scheme developed by Stewart and Oke (2012). LCZs provide valuable information about urban
#' morphology and are useful for urban climate and environmental studies.
#'
#' @param x A LCZ map in .tiff format SpatRaster object containing LCZ classes.
#' @param iSelect Character vector. Specify one or more parameter names to retrieve specific
#'                parameters. For example, "SVF.min" to get the minimum Sky View Factor,
#'                or c("SVF.min", "veg.frac.max") to select multiple parameters.
#' @param iShp Logical. If TRUE, returns all parameters as an ESRI shapefile.
#' @param iStack Logical. If TRUE, returns all parameters as a raster stack.
#'               If FALSE, returns a list of individual parameter rasters.
#' @param isave Set to TRUE if you want to save the your clipped resulting map as a raster.tiff or .shp file on your local machine.
#'
#' @return If 'iStack' is TRUE, it returns a raster stack containing 34 LCZ-related parameters.
#'         If 'iStack' is FALSE, it returns a list of individual parameter rasters.
#'         If 'iShp' is TRUE, it returns an ESRI shapefile containing all parameters.
#'         If 'iSelect' is specified, it returns the selected parameter(s) as a raster(s).
#'
#' @export
#'
#' @examples
#' # Load the LCZ map
#' #lcz_map <- raster::raster("path/to/lcz_map.tif")
#'
#' # Get LCZ parameters as a raster stack
#' #lcz_params_stack <- getLCZparameters(lcz_map, iStack = TRUE)
#'
#' # Get LCZ parameters as a list of individual parameter rasters
#' #lcz_params_list <- getLCZparameters(lcz_map, iStack = FALSE)
#'
#' # Access individual parameters from the list
#' #specific_param <- lcz_params_list$parameter_name
#'
#' # Plot a specific parameter
#' #plot(specific_param, main = "LCZ Parameter: Parameter Name")
#'
#' # Get specific parameters as a raster or shapefile
#' #selected_params <- getLCZparameters(lcz_map, iSelect = c("SVF.min", "veg.frac.max"), iShp = TRUE)
#'
#' @importFrom rlang .data

getLCZparameters <- function(x,  iSelect = NULL, iShp = FALSE, iStack = FALSE, isave = FALSE) {

  lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water")

  lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
               "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
               "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA")

  # Data frame for LCZ parameters
  lcz.df <- base::data.frame(
    lcz = c(base::seq(1, 10, 1), base::seq(11, 17)),
    lcz.name = lcz.name,
    lcz.code = c(base::seq(1, 10, 1), "A", "B", "C", "D", "E", "F", "G"),
    lcz.col = lcz.col,
    SVF.min = c(0.2, 0.3, 0.2, 0.5, 0.5, 0.6, 0.2, 0.75, 0.85, 0.6, 0.35, 0.5, 0.7, base::rep(0.9, 4)),
    SVF.max = c(0.4, 0.6, 0.6, 0.7, 0.8, 0.9, 0.5, 0.75, 0.85, 0.9, 0.35, 0.8, 0.9, base::rep(0.9, 4)),
    aspect.ratio.min = c(3, 0.75, 0.75, 0.75, 0.3, 0.3, 1, 0.1, 0.1, 0.2, 1.5, 0.25, 0.25, base::rep(0.1, 4)),
    aspect.ratio.max = c(3, 2, 1.5, 1.25, 0.75, 0.75, 2, 0.3, 0.25, 0.5, 1.5, 0.75, 1.0, base::rep(0.1, 4)),
    build.frac.min = c(40, 40, 40, base::rep(20,3), 60, 30, 10, 20, base::rep(9, 7)),
    build.frac.max = c(60, 70, 70, base::rep(40,3), 90, 50, 20, 30, base::rep(9, 7)),
    imp.frac.min = c(40, 40, 40, base::rep(20, 3), 60, 30, 10, 20, base::rep(0, 7)),
    imp.frac.max = c(60, 70, 70, base::rep(40, 3), 90, 50, 20, 30, base::rep(10, 7)),
    veg.frac.max = c(10, 20, 30, 40, 40, 60, 30, 20, 80, 50, base::rep(100, 4), 10, 100, 100),
    veg.frac.min = c(0, 0, 0, 30, 20, 30, 0, 0, 60, 40, 90, 90, 90, 90, 0, 90, 90),
    tree.frac.min = c(base::rep(0, 10), 90, 90, base::rep(0, 5)),
    tree.frac.max = c(base::rep(0, 10), 100, 100, base::rep(0, 5)),
    height.roug.min = c(26, 10, 3, 26, 10, 3, 2, 3, 3, 5, 3, 3, 2.9, 0.9, 0.24, 0.23,  0),
    height.roug.max = c(26, 25, 10, 26, 25, 10, 4, 10, 10, 15, 30, 15, 2.9, 0.9, 0.24, 0.23, 0),
    terra.roug.min = c(8, 6, 6, 7, 5, 5, 4, 5, 5, 5, 8, 5, 4, 3, 1, 1, 1),
    terra.roug.max = c(8, 7, 6, 8, 6, 6, 5, 5, 6, 6, 8, 6, 5, 4, 2, 2, 1),
    surf.admit.min = c(1.500, 1.500, 1.200, 1.400, 1.400, 1.200, 800, 1.200, 1.000, 1.000, 0, 1.000, 700, 1.200, 1.200, 600, 1.500),
    surf.admit.max = c(1.800, 2.000, 1.800, 1.800, 2.000, 1.800, 1.500, 1.800, 1.800, 2.5000, 0, 1.800, 1.500, 1.600, 2.500, 1.400, 1.500),
    surf.albedo.min = c(base::rep(0.10, 3), base::rep(0.12, 3), base::rep(0.15, 2), base::rep(0.12, 2), 0.10, base::rep(0.15, 4), 0.20, 0.02),
    surf.albedo.max = c(base::rep(0.20, 3), base::rep(0.25, 3), 0.35, 0.25, 0.25, 0.20, 0.20, 0.25, 0.30, 0.25, 0.30, 0.35, 0.10),
    antrop.heat.min = c(50, 74, 74, 49, 24, 24, 34, 49, 9, 310, base::rep(0, 7)),
    antrop.heat.max = c(300, 74, 74, 49, 24, 24, 34, 49, 9, 310, base::rep(0, 7))
  )

  # Function to calculate z0
  calculate_z0 <- function(code) {
    if (code == "G") return(0.0002)
    if (code %in% c("E", "F")) return(0.0005)
    if (code == "D") return(0.03)
    if (code %in% c(7, "C")) return(0.10)
    if (code %in% c(8, "B")) return(0.25)
    if (code %in% c(2, 3, 5, 6, 9, 10)) return(0.5)
    if (code %in% c(2, 4)) return(1.0)
    if (code %in% c(1, "A")) return(2)
    return(NA)
  }

  # Calculate z0 values
  lcz.df$z0 <- sapply(lcz.df$lcz.code, calculate_z0)

  # Calculate mean values for parameters
  lcz.df$SVF.mean <- (lcz.df$SVF.min + lcz.df$SVF.max) / 2
  lcz.df$aspect.ratio.mean <- (lcz.df$aspect.ratio.min + lcz.df$aspect.ratio.max) / 2
  lcz.df$build.frac.mean <- (lcz.df$build.frac.min + lcz.df$build.frac.max) / 2
  lcz.df$imp.frac.mean <- (lcz.df$imp.frac.min + lcz.df$imp.frac.max) / 2
  lcz.df$veg.frac.mean <- (lcz.df$veg.frac.min + lcz.df$veg.frac.max) / 2
  lcz.df$tree.frac.mean <- (lcz.df$tree.frac.min + lcz.df$tree.frac.max) / 2
  lcz.df$height.roug.mean <- (lcz.df$height.roug.min + lcz.df$height.roug.max) / 2
  lcz.df$terra.roug.mean <- (lcz.df$terra.roug.min + lcz.df$terra.roug.max) / 2
  lcz.df$surf.admit.mean <- (lcz.df$surf.admit.min + lcz.df$surf.admit.max) / 2
  lcz.df$surf.albedo.mean <- (lcz.df$surf.albedo.min + lcz.df$surf.albedo.max) / 2

  #Pbase::reprocessing raster
  base::names(x) <- "lcz"
  lcz_shp <- terra::as.polygons({{x}}) %>%
    sf::st_as_sf()
  lcz_result <- dplyr::inner_join(lcz_shp, lcz.df, by="lcz") %>%
    dplyr::select(-.data$lcz.code, -lcz.name, -lcz.col)

  if(iShp==TRUE) {

    # if(isave==TRUE){
    #
    #   lcz_result_named <- lcz_result %>% janitor::clean_names()
    #   # Create a folder name using paste0
    #   folder <- base::paste0("LCZ4r_output/")
    #
    #   # Check if the folder exists
    #   if (!dir.exists(folder)) {
    #     # Create the folder if it does not exist
    #     base::dir.create(folder)
    #   }
    #
    #   file <- base::paste0(folder,"lcz_par.shp")
    #
    #   sf::st_write(lcz_result_named, file,  append = FALSE)
    # }

      return(lcz_result)
  }

  if(iStack==TRUE){

      # Remove the 'lcz' column from lcz_result
      lcz_df_pre <- dplyr::select(lcz_result)

      # Initialize a list to store rasterized and resampled maps
      ras_map <- list()
      # Loop through columns of lcz_result
      for (i in 1:ncol(lcz_df_pre)) {
        # Rasterize the column and convert it to a raster
        ras <- stars::st_rasterize(lcz_df_pre[, i])
        # Add the raster to the list
        ras_map[[i]] <- ras
      }

      # Convert stars Rasters to terra Rasters
      terra_rasters <- base::lapply(ras_map, function(stars_raster) {
        terra::rast(stars_raster) %>% raster::raster()
      })

      # Create a raster stack from the list of rasters
      ras_stack <- raster::stack(terra_rasters)

      # Set names for the layers in the raster stack
      base::names(ras_stack) <- base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

    if(isave==TRUE){

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_par_stack.tif")

      raster::writeRaster(ras_stack, file, format="GTiff", overwrite = TRUE)
    }

    return(ras_stack)
  }

  if(!base::is.null(iSelect)){


    if(length(iSelect)>1) {

      # Remove the 'lcz' column from lcz_result
      lcz_df_pre <- lcz_result %>%
        dplyr::select({{iSelect}})

      ras_select <- lapply(2:ncol(lcz_df_pre)-1, FUN = function(i) {
        ras_select <- stars::st_rasterize(lcz_df_pre[, i]) %>%
          terra::rast() %>%
          raster::raster()})

      # Set names for the layers in the raster stack
      ras_stack_selec <- raster::stack(ras_select)

      # Set names for the layers in the raster stack
      base::names(ras_stack_selec) <- base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

      if(isave==TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder,"lcz_par_stack_select.tif")

        raster::writeRaster(ras_stack_selec, file, format="GTiff", overwrite = TRUE)
      }

      return(ras_stack_selec)

    } else {

      # Remove the 'lcz' column from lcz_result
      lcz_df_pre <- lcz_result %>%
        dplyr::select({{iSelect}})

      ras_select <- stars::st_rasterize(lcz_df_pre)
      ras_select_raster <- raster::raster(terra::rast(ras_select))

      # Set names for the layers in the raster stack
      base::names(ras_select_raster) <- base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

      if(isave==TRUE){

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(folder,"lcz_par_select.tif")

        raster::writeRaster(ras_select_raster, file, format="GTiff", overwrite = TRUE)
      }

      return(ras_select_raster)
    }

  }

}



