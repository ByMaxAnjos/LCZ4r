#' Get LCZ parameters
#'
#' Maps the 17 LCZ classes to their physical parameters (Stewart and Oke, 2012),
#' returning either an `sf` polygon layer, a raster stack or a list of rasters.
#'
#' @param x A `SpatRaster` (or path) with LCZ classes (1-17). It can be obtained
#'   with [lcz_get_map()], [lcz_get_map_euro()] or [lcz_get_map_usa()].
#' @param iselect Character vector with the parameter(s) to return, e.g.
#'   `"SVFmean"`. When supplied, only the selected layers are returned,
#'   independently of `istack`.
#' @param istack Logical. If `TRUE` (default), return all parameters as a raster
#'   stack.
#' @param ishp Logical. If `TRUE`, return all parameters as an `sf` polygon
#'   layer. Takes precedence over `istack` and `iselect`.
#' @param isave Logical. Save the result in `LCZ4r_output/`.
#' @param lang Language used for console messages, errors and plot labels. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return Depending on the arguments: an `sf` object (`ishp = TRUE`), a
#'   `SpatRaster` stack (`istack = TRUE` or `iselect`), or a list of
#'   `SpatRaster` layers (`istack = FALSE`).
#'
#' @export
#'
#' @references
#' Stewart, I. D., and Oke, T. R. (2012). Local Climate Zones for urban
#' temperature studies. Bull. Amer. Meteor. Soc. 93(12) 1879-1900.
#' \doi{10.1175/BAMS-D-11-00019.1}
#'
#' @examples
#' \dontrun{
#' lcz_map <- lcz_get_map(city = "Juiz de Fora")
#'
#' # Full stack (34 layers)
#' lcz_params <- lcz_get_parameters(lcz_map, istack = TRUE)
#'
#' # Only the parameters you need (faster and lighter)
#' lcz_params <- lcz_get_parameters(lcz_map, iselect = c("SVFmean", "AHmean"))
#'
#' # Polygons
#' lcz_poly <- lcz_get_parameters(lcz_map, ishp = TRUE)
#' }
#'
#' @seealso [lcz_get_map()], [lcz_plot_parameters()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_get_parameters <- function(x,
                               iselect = NULL,
                               istack = TRUE,
                               ishp = FALSE,
                               isave = FALSE,
                               lang = "en") {
  x <- lcz_check_raster(x, lang = lang)

  # LCZ physical parameters (Stewart & Oke 2012, Table 3) ---------------------
  lcz.df <- base::data.frame(
    lcz = c(1:10, 11:17),
    lcz.code = c(1:10, "A", "B", "C", "D", "E", "F", "G"),
    SVFmin = c(0.2, 0.3, 0.2, 0.5, 0.5, 0.6, 0.2, 0.75, 0.85, 0.6, 0.35, 0.5, 0.7, rep(0.9, 4)),
    SVFmax = c(0.4, 0.6, 0.6, 0.7, 0.8, 0.9, 0.5, 0.75, 0.85, 0.9, 0.35, 0.8, 0.9, rep(0.9, 4)),
    ARmin = c(3, 0.75, 0.75, 0.75, 0.3, 0.3, 1, 0.1, 0.1, 0.2, 1.5, 0.25, 0.25, rep(0.1, 4)),
    ARmax = c(3, 2, 1.5, 1.25, 0.75, 0.75, 2, 0.3, 0.25, 0.5, 1.5, 0.75, 1.0, rep(0.1, 4)),
    BSFmin = c(40, 40, 40, rep(20, 3), 60, 30, 10, 20, rep(9, 7)),
    BSFmax = c(60, 70, 70, rep(40, 3), 90, 50, 20, 30, rep(9, 7)),
    ISFmin = c(40, 40, 40, rep(20, 3), 60, 30, 10, 20, rep(0, 7)),
    ISFmax = c(60, 70, 70, rep(40, 3), 90, 50, 20, 30, rep(10, 7)),
    PSFmax = c(10, 20, 30, 40, 40, 60, 30, 20, 80, 50, rep(100, 4), 10, 100, 100),
    PSFmin = c(0, 0, 0, 30, 20, 30, 0, 0, 60, 40, 90, 90, 90, 90, 0, 90, 90),
    TSFmin = c(rep(0, 10), 90, 90, rep(0, 5)),
    TSFmax = c(rep(0, 10), 100, 100, rep(0, 5)),
    HREmin = c(26, 10, 3, 26, 10, 3, 2, 3, 3, 5, 3, 3, 2.9, 0.9, 0.24, 0.23, 0),
    HREmax = c(26, 25, 10, 26, 25, 10, 4, 10, 10, 15, 30, 15, 2.9, 0.9, 0.24, 0.23, 0),
    TRCmin = c(8, 6, 6, 7, 5, 5, 4, 5, 5, 5, 8, 5, 4, 3, 1, 1, 1),
    TRCmax = c(8, 7, 6, 8, 6, 6, 5, 5, 6, 6, 8, 6, 5, 4, 2, 2, 1),
    SADmin = c(1.5, 1.5, 1.2, 1.4, 1.4, 1.2, 800, 1.2, 1.0, 1.0, 0, 1.0, 700, 1.2, 1.2, 600, 1.5),
    SADmax = c(1.8, 2.0, 1.8, 1.8, 2.0, 1.8, 1.5, 1.8, 1.8, 2.5, 0, 1.8, 1.5, 1.6, 2.5, 1.4, 1.5),
    SALmin = c(rep(0.10, 3), rep(0.12, 3), rep(0.15, 2), rep(0.12, 2), 0.10, rep(0.15, 4), 0.20, 0.02),
    SALmax = c(rep(0.20, 3), rep(0.25, 3), 0.35, 0.25, 0.25, 0.20, 0.20, 0.25, 0.30, 0.25, 0.30, 0.35, 0.10),
    AHmin = c(50, 74, 74, 49, 24, 24, 34, 49, 9, 310, rep(0, 7)),
    AHmax = c(300, 74, 74, 49, 24, 24, 34, 49, 9, 310, rep(0, 7)),
    stringsAsFactors = FALSE
  )

  # Roughness length. Class 2 must be evaluated before the generic 0.5 group:
  # the original code listed 2 in both branches, which made z0 = 1 unreachable.
  lcz.df$z0 <- c(2, 1, 0.5, 1, 0.5, 0.5, 0.1, 0.25, 0.5, 0.5,
                 2, 0.25, 0.1, 0.03, 0.0005, 0.0005, 0.0002)

  # Derived means
  for (p in c("SVF", "AR", "BSF", "ISF", "PSF", "TSF", "HRE", "TRC", "SAD", "SAL", "AH")) {
    lcz.df[[paste0(p, "mean")]] <- (lcz.df[[paste0(p, "min")]] + lcz.df[[paste0(p, "max")]]) / 2
  }

  param_cols <- setdiff(names(lcz.df), c("lcz", "lcz.code"))
  names(x) <- "lcz"

  # Validate `iselect` early so typos fail loudly instead of returning NULL.
  if (!is.null(iselect)) {
    iselect <- as.character(iselect)
    bad <- setdiff(iselect, param_cols)
    if (length(bad)) {
      lcz_abort("err_iselect", lang = lang, params = paste(bad, collapse = ", "))
    }
  }

  # Fast path: recode values directly (no polygonise + rasterise round-trip) ---
  build_stack <- function(cols) {
    layers <- lapply(cols, function(col) {
      out <- terra::subst(
        x,
        from = lcz.df$lcz,
        to = lcz.df[[col]],
        others = NA
      )
      names(out) <- col
      out
    })
    terra::rast(layers)
  }

  if (isTRUE(ishp)) {
    lcz_shp <- terra::as.polygons(x, dissolve = TRUE) %>%
      sf::st_as_sf()
    out <- dplyr::inner_join(lcz_shp, lcz.df[, c("lcz", param_cols)], by = "lcz")

    if (isTRUE(isave)) {
      dir <- lcz_output_dir()
      sf::st_write(out, file.path(dir, "lcz_par.shp"),
                   append = FALSE, quiet = TRUE)
      lcz_msg("msg_files_saved", lang = lang, path = dir)
    }
    return(out)
  }

  if (!is.null(iselect)) {
    out <- build_stack(iselect)
    if (isTRUE(isave)) {
      dir <- lcz_output_dir()
      terra::writeRaster(out, file.path(dir, "lcz_par_select.tif"),
                         overwrite = TRUE)
      lcz_msg("msg_files_saved", lang = lang, path = dir)
    }
    return(out)
  }

  if (isTRUE(istack)) {
    out <- build_stack(param_cols)
    if (isTRUE(isave)) {
      dir <- lcz_output_dir()
      terra::writeRaster(out, file.path(dir, "lcz_par_stack.tif"),
                         overwrite = TRUE)
      lcz_msg("msg_files_saved", lang = lang, path = dir)
    }
    return(out)
  }

  # istack = FALSE: list of individual rasters (documented behaviour)
  stats::setNames(
    lapply(param_cols, function(col) {
      out <- terra::subst(x, from = lcz.df$lcz, to = lcz.df[[col]], others = NA)
      names(out) <- col
      out
    }),
    param_cols
  )
}
