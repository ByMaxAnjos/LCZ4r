#' Retrieve urban canopy LCZ parameters
#'
#' This function extracts 34 LCZ parameters based on the classification
#' scheme developed by Stewart and Oke (2012). LCZs provide valuable information about urban
#' morphology and are useful for urban climate and environmental studies.
#'
#' @param x A LCZ map in .tiff format SpatRaster object containing LCZ classes.
#' @param iselect Character vector. Specify one or more parameter names to retrieve specific
#'                parameters. For example, "SVF1" to get the minimum Sky View Factor,
#'                or c("SVF1", "z0") to select multiple parameters.
#' @param ishp Logical. If TRUE, returns all parameters as an ESRI shapefile.
#' @param istack Logical. If TRUE, returns all parameters as a raster stack.
#'               If FALSE, returns a list of individual parameter rasters.
#' @param isave Set to TRUE if you want to save the your clipped resulting map as a raster.tiff or .shp file on your local machine.
#'
#' @return If 'istack' is TRUE, it returns a raster stack containing 34 LCZ-related parameters.
#'         If 'istack' is FALSE, it returns a list of individual parameter rasters.
#'         If 'ishp' is TRUE, it returns an ESRI shapefile containing all parameters.
#'         If 'iselect' is specified, it returns the selected parameter(s) as a raster(s).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the LCZ map
#' lcz_map <- raster::raster("path/to/lcz_map.tif")
#'
#' # Get LCZ parameters as a raster stack
#' lcz_params_stack <- lcz_get_parameters(lcz_map, istack = TRUE)
#'
#' # Get LCZ parameters as a list of individual parameter rasters
#' lcz_params_list <- lcz_get_parameters(lcz_map, istack = FALSE)
#'
#' # Access individual parameters from the list
#' specific_param <- lcz_params_list$parameter_name
#'
#' # Get specific parameters as a raster or shapefile
#' selected_params <- lcz_get_parameters(lcz_map, iselect = c("SVF1", "VEG1"))
#' }
#' @importFrom rlang .data
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_get_parameters <- function(x,  iselect = "", istack = TRUE, ishp = FALSE, isave = FALSE) {

  # Validate inputs
  if (base::is.null(x)) {
    stop("The input must be SpatRaster object of terra package. Please, use the get_lcz_map()")
  }

  if(!inherits(x, "SpatRaster")) { x <- terra::rast({{x}}) }

  x<- x[[1]]
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
    SVF1 = c(0.2, 0.3, 0.2, 0.5, 0.5, 0.6, 0.2, 0.75, 0.85, 0.6, 0.35, 0.5, 0.7, base::rep(0.9, 4)),
    SVF2 = c(0.4, 0.6, 0.6, 0.7, 0.8, 0.9, 0.5, 0.75, 0.85, 0.9, 0.35, 0.8, 0.9, base::rep(0.9, 4)),
    AR1 = c(3, 0.75, 0.75, 0.75, 0.3, 0.3, 1, 0.1, 0.1, 0.2, 1.5, 0.25, 0.25, base::rep(0.1, 4)),
    AR2 = c(3, 2, 1.5, 1.25, 0.75, 0.75, 2, 0.3, 0.25, 0.5, 1.5, 0.75, 1.0, base::rep(0.1, 4)),
    BSF1 = c(40, 40, 40, base::rep(20,3), 60, 30, 10, 20, base::rep(9, 7)),
    BSF2 = c(60, 70, 70, base::rep(40,3), 90, 50, 20, 30, base::rep(9, 7)),
    ISF1 = c(40, 40, 40, base::rep(20, 3), 60, 30, 10, 20, base::rep(0, 7)),
    ISF2 = c(60, 70, 70, base::rep(40, 3), 90, 50, 20, 30, base::rep(10, 7)),
    PSF2 = c(10, 20, 30, 40, 40, 60, 30, 20, 80, 50, base::rep(100, 4), 10, 100, 100),
    PSF1 = c(0, 0, 0, 30, 20, 30, 0, 0, 60, 40, 90, 90, 90, 90, 0, 90, 90),
    TSF1 = c(base::rep(0, 10), 90, 90, base::rep(0, 5)),
    TSF2 = c(base::rep(0, 10), 100, 100, base::rep(0, 5)),
    HRE1 = c(26, 10, 3, 26, 10, 3, 2, 3, 3, 5, 3, 3, 2.9, 0.9, 0.24, 0.23,  0),
    HRE2 = c(26, 25, 10, 26, 25, 10, 4, 10, 10, 15, 30, 15, 2.9, 0.9, 0.24, 0.23, 0),
    TRC1 = c(8, 6, 6, 7, 5, 5, 4, 5, 5, 5, 8, 5, 4, 3, 1, 1, 1),
    TRC2 = c(8, 7, 6, 8, 6, 6, 5, 5, 6, 6, 8, 6, 5, 4, 2, 2, 1),
    SAD1 = c(1.500, 1.500, 1.200, 1.400, 1.400, 1.200, 800, 1.200, 1.000, 1.000, 0, 1.000, 700, 1.200, 1.200, 600, 1.500),
    SAD2 = c(1.800, 2.000, 1.800, 1.800, 2.000, 1.800, 1.500, 1.800, 1.800, 2.5000, 0, 1.800, 1.500, 1.600, 2.500, 1.400, 1.500),
    SAL1 = c(base::rep(0.10, 3), base::rep(0.12, 3), base::rep(0.15, 2), base::rep(0.12, 2), 0.10, base::rep(0.15, 4), 0.20, 0.02),
    SAL2 = c(base::rep(0.20, 3), base::rep(0.25, 3), 0.35, 0.25, 0.25, 0.20, 0.20, 0.25, 0.30, 0.25, 0.30, 0.35, 0.10),
    AH1 = c(50, 74, 74, 49, 24, 24, 34, 49, 9, 310, base::rep(0, 7)),
    AH2 = c(300, 74, 74, 49, 24, 24, 34, 49, 9, 310, base::rep(0, 7))
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
  lcz.df$z0 <- base::sapply(lcz.df$lcz.code, calculate_z0)

  # Calculate mean values for parametres
  lcz.df$SVF3 <- (lcz.df$SVF1 + lcz.df$SVF2) / 2
  lcz.df$AR3<- (lcz.df$AR1 + lcz.df$AR2) / 2
  lcz.df$BSF3 <- (lcz.df$BSF1 + lcz.df$BSF2) / 2
  lcz.df$ISF3 <- (lcz.df$ISF1 + lcz.df$ISF2) / 2
  lcz.df$PSF3 <- (lcz.df$PSF1 + lcz.df$PSF2) / 2
  lcz.df$TSF3 <- (lcz.df$TSF1 + lcz.df$TSF2) / 2
  lcz.df$HRE3 <- (lcz.df$HRE1 + lcz.df$HRE2) / 2
  lcz.df$TRC3 <- (lcz.df$TRC1 + lcz.df$TRC2) / 2
  lcz.df$SAD3 <- (lcz.df$SAD1 + lcz.df$SAD2) / 2
  lcz.df$SAL3 <- (lcz.df$SAL1 + lcz.df$SAL2) / 2
  lcz.df$AH3 <- (lcz.df$AH1 + lcz.df$AH2) / 2

  #reprocessing raster
  base::names(x) <- "lcz"
  lcz_shp <- terra::as.polygons({{x}}) %>%
    sf::st_as_sf()
  lcz_result <- dplyr::inner_join(lcz_shp, lcz.df, by="lcz") %>%
    dplyr::select(-.data$lcz.code, -lcz.name, -lcz.col)

  if (ishp==TRUE) {

    if (isave==TRUE){

      string_list <- c(
        "lcz_class", "svf_min", "svf_max", "AR_min", "AR_max", "BSF_min",
        "BSF_max", "ISF_min", "ISF_max", "PSF_max", "PSF_min", "TSF_min",
        "TSF_max", "HRE_min", "HRE_max", "TRC_min", "TRC_max", "SAD_min",
        "SAD_max", "SAL_min", "SAL_max", "AH_min", "AH_max", "z0",
        "svf_mean", "aspect_mean", "BSF_mean", "ISF_mean", "PSF_mean", "TSF_mean",
        "HRE_mean", "TRC_mean", "SAD_mean", "SAL_mean", "AH_mean", "geometry"
      )

      base::names(lcz_result) <- string_list

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder,"lcz_par.shp")
      sf::st_write(lcz_result, file,  append = FALSE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }
      return(lcz_result)
  }

  if (istack==TRUE) {

      # Initialize a list to store rasterized and resampled maps
    ras <- base::lapply(1:ncol(lcz_result), FUN = function(i) {
      ras_select <- stars::st_rasterize(lcz_result[, i]) %>%
        terra::rast()})
      # Create a raster stack from the list of rasters
      ras_stack <- terra::rast(ras)[[-36]]

      # Set names for the layers in the raster stack
      base::names(ras_stack) <- base::colnames(lcz_result)[1:ncol(lcz_result)-1]

    if (isave==TRUE) {

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(getwd(), "/", folder,"lcz_par_stack.tif")
      raster::writeRaster(ras_stack, file, format="GTiff", overwrite = TRUE)
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

    }

    return(ras_stack)
  }

  if (!base::is.null(iselect)){

    if (length(iselect) > 1) {

      # Remove the 'lcz' column from lcz_result
      lcz_df_pre <- lcz_result %>%
        dplyr::select({{iselect}})

      ras_select <- base::lapply(2:ncol(lcz_df_pre)-1, FUN = function(i) {
        ras_select <- stars::st_rasterize(lcz_df_pre[, i]) %>%
          terra::rast()})

      # Set names for the layers in the raster stack
      ras_stack_selec <- terra::rast(ras_select)
      base::names(ras_stack_selec) <-  base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

      if (isave==TRUE) {

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(getwd(), "/", folder,"lcz_par_stack_select.tif")
        raster::writeRaster(ras_stack_selec, file, format="GTiff", overwrite = TRUE)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

      }

      return(ras_stack_selec)

    } else {

      # Remove the 'lcz' column from lcz_result
      lcz_df_pre <- lcz_result %>%
        dplyr::select({{iselect}})

      ras_select <- stars::st_rasterize(lcz_df_pre)
      ras_select_raster <- terra::rast(ras_select)
      base::names(ras_select_raster) <-  base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

      if (isave==TRUE) {

        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file <- base::paste0(getwd(), "/", folder,"lcz_par_select.tif")
        raster::writeRaster(ras_select_raster, file, format="GTiff", overwrite = TRUE)
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
      }

      return(ras_select_raster)
    }

  }

}



