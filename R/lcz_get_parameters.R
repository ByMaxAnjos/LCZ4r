
#' Get LCZ Parameters from LCZ Stewart and Oke (2012)
#'
#' This function extracts 34 Local Climate Zone (LCZ) parameters based on the classification
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
#' # Load the LCZ map
#' #lcz_map <- raster::raster("path/to/lcz_map.tif")
#'
#' # Get LCZ parameters as a raster stack
#' #lcz_params_stack <- lcz_get_parameters(lcz_map, istack = TRUE)
#'
#' # Get LCZ parameters as a list of individual parameter rasters
#' #lcz_params_list <- lcz_get_parameters(lcz_map, istack = FALSE)
#'
#' # Access individual parameters from the list
#' #specific_param <- lcz_params_list$parameter_name
#'
#' # Get specific parameters as a raster or shapefile
#' #selected_params <- lcz_get_parameters(lcz_map, iselect = c("SVF1", "VEG1"))
#'
#' @importFrom rlang .data

lcz_get_parameters <- function(x,  iselect = "", istack = TRUE, ishp = FALSE, isave = FALSE) {

  # Validate inputs
  if (base::is.null(x)) {
    stop("The input must be SpatRaster object of terra package. Please, use the get_lcz_map()")
  }


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
  lcz.df$z0 <- base::sapply(lcz.df$lcz.code, calculate_z0)

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
  lcz.df$antrop.heat.mean <- (lcz.df$antrop.heat.min + lcz.df$antrop.heat.max) / 2

  # take_names2 <- function(x) {
  #   if(x == "SVF.min") return("Minimum Sky View Factor")
  #   if(x == "SVF.max") return("Maximum Sky View Factor")
  #   if(x == "SVF.mean") return("Mean Sky View Factor")
  #   if(x == "aspect.ratio.min") return("Minimum Aspect Ratio")
  #   if(x == "aspect.ratio.max") return("Maximum Aspect Ratio")
  #   if(x == "aspect.ratio.mean") return("Mean Aspect Ratio")
  #   if(x == "build.frac.min") return("Minimum Building Surface Fraction")
  #   if(x == "build.frac.max") return("Maximum Building Surface Fraction")
  #   if(x == "build.frac.mean") return("Mean Building Surface Fraction")
  #   if(x == "imp.frac.min") return("Minimum Impervious Surface Fraction")
  #   if(x == "imp.frac.max") return("Maximum Impervious Surface Fraction")
  #   if(x == "imp.frac.mean") return("Mean Impervious Surface Fraction")
  #   if(x == "veg.frac.min") return("Minimum Pervious Surface Fraction")
  #   if(x == "veg.frac.max") return("Maximum Pervious Surface Fraction")
  #   if(x == "veg.frac.mean") return("Mean Pervious Surface Fraction")
  #   if(x == "tree.frac.min") return("Minimum Tree Surface Fraction")
  #   if(x == "tree.frac.max") return("Maximum Tree Surface Fraction")
  #   if(x == "tree.frac.mean") return("Mean Tree Surface Fraction")
  #   if(x == "height.roug.min") return("Minimum Height Roughness Elements")
  #   if(x == "height.roug.max") return("Maximum Height Roughness Elements")
  #   if(x == "height.roug.mean") return("Mean Height Roughness Elements")
  #   if(x == "terra.roug.min") return("Minimum Terrain Roughness class")
  #   if(x == "terra.roug.max") return("Maximum Terrain Roughness Class")
  #   if(x == "terra.roug.mean") return("Mean Terrain Roughness Class")
  #   if(x == "surf.admit.min") return("Minimum Surface Admittance")
  #   if(x == "surf.admit.max") return("Maximum Surface Admittance")
  #   if(x == "surf.admit.mean") return("Mean Surface Admittance")
  #   if(x == "surf.albedo.min") return("Minimum Surface Albedo")
  #   if(x == "surf.albedo.max") return("Maximum Surface Aldedo")
  #   if(x == "surf.albedo.mean") return("Mean Surface Albedo")
  #   if(x == "antrop.heat.min") return("Minimum Anthropogenic Heat Outupt")
  #   if(x == "antrop.heat.max") return("Maximum Anthropogenic Heat Outupt")
  #   if(x == "antrop.heat.mean") return("Mean Anthropogenic Heat Outupt")
  #   if(x == "z0") return("Roughness Lenght")
  # }

  # take_code <- function(x) {
  #   if(x == "SVF.min") return("SVF1")
  #   if(x == "SVF.max") return("SVF2")
  #   if(x == "SVF.mean") return("SVF3")
  #   if(x == "aspect.ratio.min") return("ASP1")
  #   if(x == "aspect.ratio.max") return("ASP2")
  #   if(x == "aspect.ratio.mean") return("ASP3")
  #   if(x == "build.frac.min") return("BUI1")
  #   if(x == "build.frac.max") return("BUI2")
  #   if(x == "build.frac.mean") return("BUI3")
  #   if(x == "imp.frac.min") return("IMP1")
  #   if(x == "imp.frac.max") return("IMP2")
  #   if(x == "imp.frac.mean") return("IMP3")
  #   if(x == "veg.frac.min") return("VEG1")
  #   if(x == "veg.frac.max") return("VEG2")
  #   if(x == "veg.frac.mean") return("VEG3")
  #   if(x == "tree.frac.min") return("TRE1")
  #   if(x == "tree.frac.max") return("TRE2")
  #   if(x == "tree.frac.mean") return("TRE3")
  #   if(x == "height.roug.min") return("HEI1")
  #   if(x == "height.roug.max") return("HEI2")
  #   if(x == "height.roug.mean") return("HEI3")
  #   if(x == "terra.roug.min") return("TER1")
  #   if(x == "terra.roug.max") return("TER2")
  #   if(x == "terra.roug.mean") return("TER3")
  #   if(x == "surf.admit.min") return("ADM1")
  #   if(x == "surf.admit.max") return("ADM2")
  #   if(x == "surf.admit.mean") return("ADM3")
  #   if(x == "surf.albedo.min") return("ALB1")
  #   if(x == "surf.albedo.max") return("ALB2")
  #   if(x == "surf.albedo.mean") return("ALB3")
  #   if(x == "antrop.heat.min") return("ANT1")
  #   if(x == "antrop.heat.max") return("ANT2")
  #   if(x == "antrop.heat.mean") return("ANT3")
  #   if(x == "z0") return("Z0")
  # }

  # Create a named vector with mappings
  code_mapping <- c(
     "lcz",
     "lcz.name",
     "lcz.code",
     "lcz.col",
     "SVF1",
     "SVF2",
     "SVF3",
     "ASP1",
     "ASP2",
     "ASP3",
     "BUI1",
     "BUI2",
     "BUI3",
     "IMP1",
     "IMP2",
     "IMP3",
     "VEG1",
     "VEG2",
     "VEG3",
     "TRE1",
     "TRE2",
     "TRE3",
     "HEI1",
     "HEI2",
     "HEI3",
     "TER1",
     "TER2",
     "TER3",
     "ADM1",
     "ADM2",
     "ADM3",
     "ALB1",
     "ALB2",
     "ALB3",
     "ANT1",
     "ANT2",
     "ANT3",
     "z0"
  )

  base::colnames(lcz.df) <- code_mapping

  #reprocessing raster
  base::names(x) <- "lcz"
  lcz_shp <- terra::as.polygons({{x}}) %>%
    sf::st_as_sf()
  lcz_result <- dplyr::inner_join(lcz_shp, lcz.df, by="lcz") %>%
    dplyr::select(-.data$lcz.code, -lcz.name, -lcz.col)

  if(ishp==TRUE) {

    if(isave==TRUE){

      string_list <- c(
        "lcz_class", "svf_min", "svf_max", "aspect_min", "aspect_max", "build_min",
        "build_max", "imper_min", "imper_max", "pervi_max", "pervi_min", "tree_min",
        "tree_max", "height_min", "height_max", "terra_min", "terra_max", "admit_min",
        "admit_max", "albed_min", "albed_max", "anthr_min", "anthr_max", "z0",
        "svf_mean", "aspect_mean", "build_mean", "imper_mean", "pervi_mean", "tree_mean",
        "height_mean", "terra_mean", "admit_mean", "albed_mean", "anthr_mean", "geometry"
      )

      base::names(lcz_result) <- string_list

      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file <- base::paste0(folder,"lcz_par.shp")

      sf::st_write(lcz_result, file,  append = FALSE)

    }

      return(lcz_result)
  }

  if(istack==TRUE){

      # Initialize a list to store rasterized and resampled maps
    ras <- base::lapply(1:ncol(lcz_result), FUN = function(i) {
      ras_select <- stars::st_rasterize(lcz_result[, i]) %>%
        terra::rast() %>%
        raster::raster()})

      # Create a raster stack from the list of rasters
      ras_stack <- raster::stack(ras)[[-36]]

      # Set names for the layers in the raster stack
      base::names(ras_stack) <- base::colnames(lcz_result)[1:ncol(lcz_result)-1]

      # names_par <- tibble::as_tibble(names(ras_stack))
      # take_names <- function(x) {
      #   if(x == "lcz") return("LCZ class")
      #   if(x == "SVF1") return("Minimum Sky View Factor")
      #   if(x == "SVF2") return("Maximum Sky View Factor")
      #   if(x == "SVF3") return("Mean Sky View Factor")
      #   if(x == "ASP1") return("Minimum Aspect Ratio")
      #   if(x == "ASP2") return("Maximum Aspect Ratio")
      #   if(x == "ASP3") return("Mean Aspect Ratio")
      #   if(x == "BUI1") return("Minimum Building Surface Fraction")
      #   if(x == "BUI2") return("Maximum Building Surface Fraction")
      #   if(x == "BUI3") return("Mean Building Surface Fraction")
      #   if(x == "IMP1") return("Minimum Impervious Surface Fraction")
      #   if(x == "IMP2") return("Maximum Impervious Surface Fraction")
      #   if(x == "IMP3") return("Mean Impervious Surface Fraction")
      #   if(x == "VEG1") return("Minimum Pervious Surface Fraction")
      #   if(x == "VEG2") return("Maximum Pervious Surface Fraction")
      #   if(x == "VEG3") return("Mean Pervious Surface Fraction")
      #   if(x == "TRE1") return("Minimum Tree Surface Fraction")
      #   if(x == "TRE2") return("Maximum Tree Surface Fraction")
      #   if(x == "TRE3") return("Mean Tree Surface Fraction")
      #   if(x == "HEI1") return("Minimum Height Roughness Elements")
      #   if(x == "HEI2") return("Maximum Height Roughness Elements")
      #   if(x == "HEI3") return("Mean Height Roughness Elements")
      #   if(x == "TER1") return("Minimum Terrain Roughness class")
      #   if(x == "TER2") return("Maximum Terrain Roughness Class")
      #   if(x == "TER3") return("Mean Terrain Roughness Class")
      #   if(x == "ADM1") return("Minimum Surface Admittance")
      #   if(x == "ADM2") return("Maximum Surface Admittance")
      #   if(x == "ADM3") return("Mean Surface Admittance")
      #   if(x == "ALB1") return("Minimum Surface Albedo")
      #   if(x == "ALB2") return("Maximum Surface Aldedo")
      #   if(x == "ALB3") return("Mean Surface Albedo")
      #   if(x == "ANT1") return("Minimum Anthropogenic Heat Outupt")
      #   if(x == "ANT2") return("Maximum Anthropogenic Heat Outupt")
      #   if(x == "ANT3") return("Mean Anthropogenic Heat Outupt")
      #   if(x == "z0") return("Roughness Lenght")
      # }
      # names_par$name <- base::sapply(names_par$value, take_names)


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

  if(!base::is.null(iselect)){

    if(length(iselect)>1) {

      # Remove the 'lcz' column from lcz_result
      lcz_df_pre <- lcz_result %>%
        dplyr::select({{iselect}})

      ras_select <- base::lapply(2:ncol(lcz_df_pre)-1, FUN = function(i) {
        ras_select <- stars::st_rasterize(lcz_df_pre[, i]) %>%
          terra::rast() %>%
          raster::raster()})

      # Set names for the layers in the raster stack
      ras_stack_selec <- raster::stack(ras_select)

      # Set names for the layers in the raster stack
      # names_par <- tibble::as_tibble(base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1])
      # take_names <- function(x) {
      #   if(x == "lcz") return("LCZ class")
      #   if(x == "SVF1") return("Minimum Sky View Factor")
      #   if(x == "SVF2") return("Maximum Sky View Factor")
      #   if(x == "SVF3") return("Mean Sky View Factor")
      #   if(x == "ASP1") return("Minimum Aspect Ratio")
      #   if(x == "ASP2") return("Maximum Aspect Ratio")
      #   if(x == "ASP3") return("Mean Aspect Ratio")
      #   if(x == "BUI1") return("Minimum Building Surface Fraction")
      #   if(x == "BUI2") return("Maximum Building Surface Fraction")
      #   if(x == "BUI3") return("Mean Building Surface Fraction")
      #   if(x == "IMP1") return("Minimum Impervious Surface Fraction")
      #   if(x == "IMP2") return("Maximum Impervious Surface Fraction")
      #   if(x == "IMP3") return("Mean Impervious Surface Fraction")
      #   if(x == "VEG1") return("Minimum Pervious Surface Fraction")
      #   if(x == "VEG2") return("Maximum Pervious Surface Fraction")
      #   if(x == "VEG3") return("Mean Pervious Surface Fraction")
      #   if(x == "TRE1") return("Minimum Tree Surface Fraction")
      #   if(x == "TRE2") return("Maximum Tree Surface Fraction")
      #   if(x == "TRE3") return("Mean Tree Surface Fraction")
      #   if(x == "HEI1") return("Minimum Height Roughness Elements")
      #   if(x == "HEI2") return("Maximum Height Roughness Elements")
      #   if(x == "HEI3") return("Mean Height Roughness Elements")
      #   if(x == "TER1") return("Minimum Terrain Roughness class")
      #   if(x == "TER2") return("Maximum Terrain Roughness Class")
      #   if(x == "TER3") return("Mean Terrain Roughness Class")
      #   if(x == "ADM1") return("Minimum Surface Admittance")
      #   if(x == "ADM2") return("Maximum Surface Admittance")
      #   if(x == "ADM3") return("Mean Surface Admittance")
      #   if(x == "ALB1") return("Minimum Surface Albedo")
      #   if(x == "ALB2") return("Maximum Surface Aldedo")
      #   if(x == "ALB3") return("Mean Surface Albedo")
      #   if(x == "ANT1") return("Minimum Anthropogenic Heat Outupt")
      #   if(x == "ANT2") return("Maximum Anthropogenic Heat Outupt")
      #   if(x == "ANT3") return("Mean Anthropogenic Heat Outupt")
      #   if(x == "z0") return("Roughness Lenght")
      # }
      # names_par$name <- base::sapply(names_par$value, take_names)
      base::names(ras_stack_selec) <-  base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

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
        dplyr::select({{iselect}})

      ras_select <- stars::st_rasterize(lcz_df_pre)
      ras_select_raster <- raster::raster(terra::rast(ras_select))

      # Set names for the layers in the raster stack
      # names_par <- tibble::as_tibble(base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1])
      # take_names <- function(x) {
      #   if(x == "lcz") return("LCZ class")
      #   if(x == "SVF1") return("Minimum Sky View Factor")
      #   if(x == "SVF2") return("Maximum Sky View Factor")
      #   if(x == "SVF3") return("Mean Sky View Factor")
      #   if(x == "ASP1") return("Minimum Aspect Ratio")
      #   if(x == "ASP2") return("Maximum Aspect Ratio")
      #   if(x == "ASP3") return("Mean Aspect Ratio")
      #   if(x == "BUI1") return("Minimum Building Surface Fraction")
      #   if(x == "BUI2") return("Maximum Building Surface Fraction")
      #   if(x == "BUI3") return("Mean Building Surface Fraction")
      #   if(x == "IMP1") return("Minimum Impervious Surface Fraction")
      #   if(x == "IMP2") return("Maximum Impervious Surface Fraction")
      #   if(x == "IMP3") return("Mean Impervious Surface Fraction")
      #   if(x == "VEG1") return("Minimum Pervious Surface Fraction")
      #   if(x == "VEG2") return("Maximum Pervious Surface Fraction")
      #   if(x == "VEG3") return("Mean Pervious Surface Fraction")
      #   if(x == "TRE1") return("Minimum Tree Surface Fraction")
      #   if(x == "TRE2") return("Maximum Tree Surface Fraction")
      #   if(x == "TRE3") return("Mean Tree Surface Fraction")
      #   if(x == "HEI1") return("Minimum Height Roughness Elements")
      #   if(x == "HEI2") return("Maximum Height Roughness Elements")
      #   if(x == "HEI3") return("Mean Height Roughness Elements")
      #   if(x == "TER1") return("Minimum Terrain Roughness class")
      #   if(x == "TER2") return("Maximum Terrain Roughness Class")
      #   if(x == "TER3") return("Mean Terrain Roughness Class")
      #   if(x == "ADM1") return("Minimum Surface Admittance")
      #   if(x == "ADM2") return("Maximum Surface Admittance")
      #   if(x == "ADM3") return("Mean Surface Admittance")
      #   if(x == "ALB1") return("Minimum Surface Albedo")
      #   if(x == "ALB2") return("Maximum Surface Aldedo")
      #   if(x == "ALB3") return("Mean Surface Albedo")
      #   if(x == "ANT1") return("Minimum Anthropogenic Heat Outupt")
      #   if(x == "ANT2") return("Maximum Anthropogenic Heat Outupt")
      #   if(x == "ANT3") return("Mean Anthropogenic Heat Outupt")
      #   if(x == "z0") return("Roughness Lenght")
      # }
      # names_par$name <- base::sapply(names_par$value, take_names)
      base::names(ras_select_raster) <-  base::colnames(lcz_df_pre)[1:ncol(lcz_df_pre)-1]

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



