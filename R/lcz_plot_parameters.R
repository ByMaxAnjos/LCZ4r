#' Visualise LCZ parameters
#'
#' Maps one or more physical parameters of the LCZ scheme (sky view factor,
#' building surface fraction, anthropogenic heat, ...) as rasters.
#'
#' @param x The raster stack returned by
#'   `lcz_get_parameters(istack = TRUE)` (the first layer, the LCZ class, is
#'   dropped automatically).
#' @param iselect Character vector with one or more parameter codes. See the
#'   parameter list below.
#' @param all Logical. Plot every parameter in the stack.
#' @param inclusive Logical. Use colour-vision-deficiency-safe palettes.
#' @param isave Logical. Save each plot in `LCZ4r_output/`.
#' @param save_extension Image format: `"png"`, `"jpg"`, `"tif"`, `"pdf"` or
#'   `"svg"`.
#' @param max_pixels Maximum number of cells per plot; larger rasters are
#'   aggregated (mean) first.
#' @param ... Passed to [ggplot2::labs()] (`subtitle`, `caption`, ...). The
#'   title is generated from the parameter name.
#' @param lang Language used for titles, messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @section Parameter codes:
#' `SVFmin`, `SVFmax`, `SVFmean` (sky view factor);
#' `ARmin`, `ARmax`, `ARmean` (aspect ratio);
#' `BSFmin`, `BSFmax`, `BSFmean` (building surface fraction);
#' `ISFmin`, `ISFmax`, `ISFmean` (impervious surface fraction);
#' `PSFmin`, `PSFmax`, `PSFmean` (pervious surface fraction);
#' `TSFmin`, `TSFmax`, `TSFmean` (tree surface fraction);
#' `HREmin`, `HREmax`, `HREmean` (height of roughness elements);
#' `TRCmin`, `TRCmax`, `TRCmean` (terrain roughness class);
#' `SADmin`, `SADmax`, `SADmean` (surface admittance);
#' `SALmin`, `SALmax`, `SALmean` (surface albedo);
#' `AHmin`, `AHmax`, `AHmean` (anthropogenic heat output);
#' `z0` (roughness length).
#'
#' @return A `ggplot` object (the last plot when several parameters are drawn).
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
#' params <- lcz_get_parameters(lcz_map, istack = TRUE)
#'
#' lcz_plot_parameters(params, iselect = "SVFmean")
#' lcz_plot_parameters(params, iselect = c("BSFmean", "AHmean"), lang = "pt")
#' lcz_plot_parameters(params, all = TRUE, isave = TRUE)
#' }
#'
#' @seealso [lcz_get_parameters()], [lcz_plot_map()]
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_plot_parameters <- function(x,
                                iselect = NULL,
                                all = FALSE,
                                inclusive = FALSE,
                                isave = FALSE,
                                save_extension = "png",
                                max_pixels = 1e6,
                                ...,
                                lang = "en") {
  if (is.null(x)) {
    lcz_abort("err_plot_params_input", lang = lang)
  }
  if (!inherits(x, "SpatRaster")) {
    x <- terra::rast(x)
  }
  if (terra::nlyr(x) < 2L) {
    lcz_abort("err_plot_params_input", lang = lang)
  }

  LCZpar <- x[[-1]]

  # Metadata lookup tables (vectorised; replaces 500 lines of if/return) ------
  units <- c(
    SVF = "[0 - 1]", AR = "[0 - 3]", BSF = "[%]", ISF = "[%]", PSF = "[%]",
    TSF = "[%]", HRE = "[m]", TRC = "[-]", SAD = "[J m-2 s1/2 K-1]",
    SAL = "[0 - 0.5]", AH = "[W m-2]", z0 = "[m]"
  )
  param_meta <- function(codes) {
    prefix <- sub("^(SVF|AR|BSF|ISF|PSF|TSF|HRE|TRC|SAD|SAL|AH|z0).*$", "\\1", codes)
    data.frame(
      code = codes,
      name = vapply(codes, .lcz_param_label, character(1), lang = lang),
      unit = unname(units[prefix]),
      stringsAsFactors = FALSE
    )
  }

  plot_one <- function(r, meta) {
    if (!is.null(max_pixels) && terra::ncell(r) > max_pixels) {
      fact <- ceiling(sqrt(terra::ncell(r) / max_pixels))
      r <- terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
    }
    df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
    names(df)[3] <- "values"

    ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$values)) +
      ggplot2::geom_raster() +
      lcz_scale_fill_sequential(
        name = paste(meta$code, meta$unit),
        guide = ggplot2::guide_colorbar(
          barwidth = ggplot2::unit(70, "mm"),
          barheight = ggplot2::unit(3, "mm"),
          title.position = "top", title.hjust = 0.5,
          frame.colour = "grey70", ticks.colour = "grey40"
        )
      ) +
      ggplot2::labs(title = meta$name, ...) +
      ggplot2::coord_equal(expand = FALSE) +
      lcz_theme_map(legend_position = "bottom")
  }

  codes <- names(LCZpar)
  if (isTRUE(all)) {
    meta <- param_meta(codes)
    last_plot <- NULL
    for (i in seq_along(codes)) {
      last_plot <- plot_one(LCZpar[[i]], meta[i, ])
      if (isTRUE(isave)) {
        lcz_save_figure(
          last_plot, paste0(codes[i], ".", save_extension),
          width = "single", height = 105, lang = lang
        )
      }
    }
    return(last_plot)
  }

  if (is.null(iselect) || !length(iselect) || !nzchar(iselect[1L])) {
    lcz_abort("err_iselect", lang = lang, params = "<empty>")
  }
  bad <- setdiff(iselect, codes)
  if (length(bad)) {
    lcz_abort("err_iselect", lang = lang, params = paste(bad, collapse = ", "))
  }

  select_raster <- LCZpar[[iselect]]
  meta <- param_meta(names(select_raster))
  last_plot <- NULL
  for (i in seq_len(terra::nlyr(select_raster))) {
    last_plot <- plot_one(select_raster[[i]], meta[i, ])
    if (isTRUE(isave)) {
      lcz_save_figure(
        last_plot, paste0(meta$code[i], ".", save_extension),
        width = "single", height = 105, lang = lang
      )
    }
  }

  last_plot
}
