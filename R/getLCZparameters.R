
#' Get LCZ parameters
#'
#' @param lcz_map lcz map .tiff format
#' @param iStack  all parameters in raster stack
#'
#' @return 34 LCZ-related parameters at raster.tiff format
#' @export
#'
#' @examples my_par <- getLCZparametes(lcz_map)

getLCZparameters <- function(lcz_map, iStack = TRUE) {
  #lcz.id <- c(seq(1, 10, 1), seq(101, 107))
  lcz <- c(seq(1, 10, 1), seq(11, 17))
  lcz.code <- c(seq(1, 10, 1), "A", "B", "C", "D", "E", "F", "G")
  # lcz.name <- c('compact_high-rise', 'compact_midrise', 'compact_low-rise',
  #               'open_high-rise', 'open_midrise', 'open_low-rise',
  #               'lightweight_low-rise', 'large_low-rise', 'sparsely_built',
  #               'heavy_industry', 'dense_trees', 'scattered_trees', 'bush_scrub',
  #               'low_plants', 'bare_rock_paved', 'bare_soil_sand', 'water')

  lcz.name <- c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise",
                "Open midrise", "Open lowrise", "Lightweight low-rise", "Large lowrise",
                "Sparsely built", "Heavy Industry", "Dense trees", "Scattered trees",
                "Bush, scrub", "Low plants", "Bare rock or paved", "Bare soil or sand", "Water")
  lcz.col <- c("#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
               "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
               "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA")

  #LCZ parameters
  SVF.min <- c(0.2, 0.3, 0.2, 0.5, 0.5, 0.6, 0.2, 0.75, 0.85, 0.6, 0.35, 0.5, 0.7, rep(0.9, 4))
  SVF.max <- c(0.4, 0.6, 0.6, 0.7, 0.8, 0.9, 0.5, 0.75, 0.85, 0.9, 0.35, 0.8, 0.9, rep(0.9, 4))
  aspect.ratio.min <- c(3, 0.75, 0.75, 0.75, 0.3, 0.3, 1, 0.1, 0.1, 0.2, 1.5, 0.25, 0.25, rep(0.1, 4))
  aspect.ratio.max <- c(3, 2, 1.5, 1.25, 0.75, 0.75, 2, 0.3, 0.25, 0.5, 1.5, 0.75, 1.0, rep(0.1, 4))
  build.frac.min <- c(40, 40, 40, rep(20,3), 60, 30, 10, 20, rep(9, 7))
  build.frac.max <- c(60, 70, 70, rep(40,3), 90, 50, 20, 30, rep(9, 7))
  imp.frac.min <- c(40, 40, 40, rep(20, 3), 60, 30, 10, 20, rep(0, 7))
  imp.frac.max <- c(60, 70, 70, rep(40, 3), 90, 50, 20, 30, rep(10, 7))
  veg.frac.max <- c(10, 20, 30, 40, 40, 60, 30, 20, 80, 50, rep(100, 4), 10, 100, 100)
  veg.frac.min <- c(0, 0, 0, 30, 20, 30, 0, 0, 60, 40, 90, 90, 90, 90, 0, 90, 90)
  tree.frac.min <- c(rep(0, 10), 90, 90, rep(0, 5))
  tree.frac.max <- c(rep(0, 10), 100, 100, rep(0, 5))
  height.roug.min <- c(26, 10, 3, 26, 10, 3, 2, 3, 3, 5, 3, 3, 2.9, 0.9, 0.24, 0.23,  0)
  height.roug.max <- c(26, 25, 10, 26, 25, 10, 4, 10, 10, 15, 30, 15, 2.9, 0.9, 0.24, 0.23, 0)
  terra.roug.min <- c(8, 6, 6, 7, 5, 5, 4, 5, 5, 5, 8, 5, 4, 3, 1, 1, 1)
  terra.roug.max <- c(8, 7, 6, 8, 6, 6, 5, 5, 6, 6, 8, 6, 5, 4, 2, 2, 1)
  surf.admit.min <- c(1.500, 1.500, 1.200, 1.400, 1.400, 1.200, 800, 1.200, 1.000, 1.000, 0, 1.000, 700, 1.200, 1.200, 600, 1.500)
  surf.admit.max <- c(1.800, 2.000, 1.800, 1.800, 2.000, 1.800, 1.500, 1.800, 1.800, 2.5000, 0, 1.800, 1.500, 1.600, 2.500, 1.400, 1.500)
  surf.albedo.min <- c(rep(0.10, 3), rep(0.12, 3), rep(0.15, 2), rep(0.12, 2), 0.10, rep(0.15, 4), 0.20, 0.02)
  surf.albedo.max <- c(rep(0.20, 3), rep(0.25, 3), 0.35, 0.25, 0.25, 0.20, 0.20, 0.25, 0.30, 0.25, 0.30, 0.35, 0.10)
  antrop.heat.min <- c(50, 74, 74, 49, 24, 24, 34, 49, 9, 310, rep(0, 7))
  antrop.heat.max <- c(300, 74, 74, 49, 24, 24, 34, 49, 9, 310, rep(0, 7))

  # lcz.col <- c('#8c0000', '#d10000', '#ff0100', '#be4d01', '#ff6602', '#ff9955',
  #              '#faee05', '#bcbcbc', '#ffccaa', '#555555', '#006a01', '#01aa00',
  #              '#648526', '#b9db79', '#000000', '#fbf7ae', '#6a6aff')
  lcz.df <- data.frame(lcz, lcz.name, lcz.code, lcz.col, SVF.min, SVF.max, aspect.ratio.min, aspect.ratio.max, build.frac.min, build.frac.max,
                       imp.frac.min, imp.frac.max, veg.frac.min, veg.frac.max, tree.frac.min, tree.frac.max,
                       height.roug.min, height.roug.max, terra.roug.min, terra.roug.max, surf.admit.min, surf.admit.max, surf.albedo.min, surf.albedo.max,
                       antrop.heat.min, antrop.heat.max,
                       stringsAsFactors = F) %>%
    mutate(z0 = ifelse(lcz.code %in% c("G"), 0.0002, #Get z0
                       ifelse(lcz.code %in% c("E", "F"), 0.0005,
                              ifelse(lcz.code=="D", 0.03,
                                     ifelse(lcz.code %in% c(7, "C"), 0.10,
                                            ifelse(lcz.code %in% c(8, "B"), 0.25,
                                                   ifelse(lcz.code %in% c(2, 3, 5, 6, 9, 10), 0.5,
                                                          ifelse(lcz.code %in% c(2, 4), 1.0,
                                                                 ifelse(lcz.code %in% c(1, "A"), 2, ""))))))))) %>%
    mutate(SVF.mean = round((SVF.min + SVF.max)/2, digits = 2),
           aspect.ratio.mean = (aspect.ratio.min + aspect.ratio.max)/2,
           build.frac.mea = (build.frac.min + build.frac.max)/2,
           imp.frac.mean = (imp.frac.min + imp.frac.max)/2,
           veg.frac.mean = (veg.frac.min + veg.frac.max)/2,
           tree.frac.mean = (tree.frac.min +tree.frac.max)/2,
           height.roug.mean = (height.roug.min + height.roug.max)/2,
           terra.roug.mean = (terra.roug.min + terra.roug.max)/2,
           surf.admit.mean = (surf.admit.min + surf.admit.max)/2,
           surf.albedo.mean = (surf.albedo.min + surf.albedo.max)/2,
           antrop.heat.mean = (antrop.heat.min + antrop.heat.max)/2
    )
  #Preprocessing raster
  names(lcz_map) <- "lcz"
  lcz_shp <- terra::as.polygons(rast(lcz_map)) %>% sf::st_as_sf()
  lcz_result <- inner_join(lcz_shp, lcz.df, by="lcz") %>%
    dplyr::select(-lcz.code, -lcz.name, -lcz.col)

  if(iStack==TRUE){
    lcz_result <-
      dplyr::select(lcz_result, -lcz)
    my.cores = parallel::detectCores()-1
    ras_map <- parallel::mclapply(1:34, FUN=function(i) {
      stars::st_rasterize(lcz_result[,i]) %>%
        terra::rast() %>% raster::raster() %>%
        terra::resample(lcz_map)}, mc.cores = my.cores)

    ras_stack <- raster::stack(ras_map)
    names(ras_stack) <- colnames(lcz_result)[1:34]
    return(ras_stack)
  } else {
    return(lcz_result)
  }
}
