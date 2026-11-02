#' Analyze LCZ Time Series
#'
#' This function generates a graphical representation of time series air temperature data across different Local Climate Zones (LCZs). It allows for flexible temporal aggregation, faceting, smoothing, and visualization options including heatmaps, facet_plot line plots, and warming stripes. For more details on LCZ analysis, visit \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}.
#'
#' @param x A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()}, \code{lcz_get_map_euro()}, and \code{lcz_get_map_usa()} functions.
#' @param data_frame A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.
#' @param var The name of the variable in the data frame representing air temperature.
#' @param station_id The name of the variable in the data frame representing station IDs.
#' @param ... Additional arguments for the \code{selectByDate} function from the \code{openair} package. These arguments allow for flexible selection of specific time periods (year, month, day, hour). Examples of how to use these arguments include:
#' \itemize{
#'   \item \strong{Year(s)}: Numeric value(s) specifying the year(s) to select. For example, \code{year = 1998:2004} selects all years between 1998 and 2004 (inclusive), while \code{year = c(1998, 2004)} selects only the years 1998 and 2004.
#'   \item \strong{Month(s)}: Numeric or character value(s) specifying the months to select. Numeric examples: \code{month = 1:6} (January to June), or character examples: \code{month = c("January", "December")}.
#'   \item \strong{Day(s)}: Numeric value(s) specifying the days to select. For instance, \code{day = 1:30} selects days from 1 to 30, or \code{day = 15} selects only the 15th day of the month.
#'   \item \strong{Hour(s)}: Numeric value(s) specifying the hours to select. For example, \code{hour = 0:23} selects all hours in a day, while \code{hour = 9} selects only the 9th hour.
#'   \item \strong{Start date}: A string specifying the start date in either start="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#'   \item \strong{End date}: A string specifying the end date in either end="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#' }
#' @param time.freq Defines the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.
#' @param extract.method A character string specifying the method used to assign the LCZ class to each station point. The default is `"simple"`. Available methods are:
#'   \itemize{
#'     \item **simple**: Assigns the LCZ class based on the value of the raster cell in which the point falls. It often is used in low-density observational network.
#'     \item **two.step**: Assigns LCZs to stations while filtering out those located in heterogeneous LCZ areas. This method requires that at least 80% of the pixels within a 5 × 5 kernel match the LCZ of the center pixel (Daniel et al., 2017). Note that this method reduces the number of stations. It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
#'     \item **bilinear**: Interpolates the LCZ class values from the four nearest raster cells surrounding the point.
#'   }
#' @param plot_type A character string indicating the type of plot to generate. Options include: \dQuote{basic_line}, \dQuote{facet_line}, \dQuote{heatmap}, \dQuote{warming_stripes}. The default is \dQuote{basic_line}. Note: This argument does not work if the \code{by} argument is active.
#' @param facet_plot A character string indicating whether to divide the plot by \dQuote{LCZ} or \dQuote{station}. The default is \dQuote{LCZ}.
#' @param smooth Logical. Set to \code{TRUE} to add a smoothed trend line using a generalized additive model (GAM). The default is \code{FALSE}.
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time). Note that the **Daylight option may result in both daytime and nighttime hours being represented in UTC**. See NOAA \url{https://gml.noaa.gov/grad/solcalc/} and argument \emph{type} in openair package: \url{https://openair-project.github.io/book/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series) into your directory.
#' @param save_extension A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.
#' @param palette Categorical palette. `"okabe"` (default) uses the colour-blind-safe Okabe-Ito palette; any \pkg{MetBrewer} palette name is also accepted.
#' @param ylab y-axis label. `NULL` (default) uses a translated "Air temperature (°C)".
#' @param xlab x-axis label. `NULL` (default) uses a translated "Time".
#' @param title Plot title. `NULL` (default) leaves it empty.
#' @param caption Source caption. `NULL` (default) uses a translated "Source: LCZ4r".
#' @param legend_name Legend name for heatmap and warming stripes. `NULL` (default) uses a translated "Temperature (°C)".
#'
#' @param lang Language used for labels, messages and errors. Available codes:
#'   `"en"` (default), `"pt"`, `"es"`, `"fr"`, `"zh"`, `"de"`, `"it"`, `"ja"`,
#'   `"ko"`, `"ru"`, `"ar"`. See [lcz_langs()].
#'
#' @return A ggplot object representing the time series of air temperature in different LCZs, or a data frame in CSV format if \code{iplot = FALSE}.
#'
#' @author
#' Max Anjos (\url{https://github.com/ByMaxAnjos})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Hourly air temperature values in 2019.
#' my_ts <- lcz_ts(lcz_map, df = lcz_data, var = "airT",
#'                  station_id = "station", year = 2020)
#'
#' }
#' @importFrom rlang .data
#' @seealso
#' See the documentation for \code{lcz_get_map()}, \code{lcz_get_map_euro()}, and \code{lcz_get_map_usa()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis

lcz_ts <- function(x,
                   data_frame = "",
                   var = "",
                   station_id = "",
                   ...,
                   time.freq = "hour",
                   extract.method = "simple",
                   plot_type = "basic_line",
                   facet_plot = "LCZ",
                   by = NULL,
                   smooth = FALSE,
                   impute = NULL,
                   iplot = TRUE,
                   isave = FALSE,
                   save_extension = "png",
                   palette = "okabe",
                   ylab = NULL,
                   xlab = NULL,
                   title = NULL,
                   caption = NULL,
                   legend_name = NULL,
                   lang = "en") {
  # Resolve language-dependent defaults once --------------------------------
  if (is.null(ylab)) ylab <- lcz_tr("air_temp", lang = lang)
  if (is.null(xlab)) xlab <- lcz_tr("time", lang = lang)
  if (is.null(caption)) caption <- lcz_caption(lang)
  if (is.null(legend_name)) legend_name <- lcz_tr("temperature", lang = lang)
  if (is.null(title)) title <- ""
  # Check and validate raster inputs -----------------------------------------
  x <- lcz_check_raster(x, lang = lang)

  # Check data inputs --------------------------------------------------------
  lcz_check_data_frame(data_frame, var, station_id, lang = lang)

  if (!(extract.method %in% c("simple", "bilinear", "two.step"))) {
    lcz_abort("err_extract_method", lang = lang)
  }

  # Pre-processing time series ----------------------------------------------

  #Latitude and Longitude
  df_processed <- data_frame %>%
    dplyr::rename(
      latitude = base::grep("(?i)^(lat|latitude)$", names(.), value = TRUE, perl = TRUE),
      longitude = base::grep("(?i)^(lon|long|longitude)$", names(.), value = TRUE, perl = TRUE),
      date = base::grep("(?i)^(date|time|timestamp|datetime)$", names(.), value = TRUE, perl = TRUE)
    )

  df_processed$latitude <- base::as.numeric(df_processed$latitude)
  df_processed$longitude <- base::as.numeric(df_processed$longitude)

  if (!("Latitude" %in% tolower(colnames(df_processed)) || "latitude" %in% tolower(colnames(df_processed)))) {
    lcz_abort("err_col", lang = lang, arg = "latitude")
  }

  if (!("Longitude" %in% tolower(colnames(df_processed)) || "longitude" %in% tolower(colnames(df_processed)))) {
    lcz_abort("err_col", lang = lang, arg = "longitude")
  }

  # Rename and define lcz_id for each lat and long
  df_processed <- df_processed %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      lcz_id = dplyr::cur_group_id(),
      lcz_id = base::as.factor(.data$lcz_id),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    openair::selectByDate(...)

  df_processed$var_interp <- base::as.numeric(df_processed$var_interp)


  # Impute missing values if necessary
  missing_values = c("NAN","NaN", "-9999", "-99", "NULL", "",
                     "NA", "N/A", "na", "missing", ".",
                     "inf", "-inf", 9999, 999, Inf, -Inf)
  df_processed <- df_processed %>%
    dplyr::mutate(var_interp = ifelse(.data$var_interp %in% missing_values, NA, .data$var_interp))


  if (!is.null(impute)) {
    impute_methods <- c("mean", "median", "knn", "bag")
    if (!(impute %in% impute_methods)) {
      lcz_abort("err_impute", lang = lang)
    }
    impute_function <- switch(impute,
      "mean" = recipes::step_impute_mean,
      "median" = recipes::step_impute_median,
      "knn" = recipes::step_impute_knn,
      "bag" = recipes::step_impute_bag
    )
    lcz_recipe <- recipes::recipe(var_interp ~ ., data = df_processed) %>%
      impute_function(.data$var_interp)
    df_processed <- lcz_recipe %>%
      recipes::prep(df_processed) %>%
      recipes::bake(new_data = NULL)
    lcz_msg("msg_imputed", lang = lang, method = impute)
  }

  # Geospatial operations ---------------------------------------------------

  # Convert lcz_map to polygon
  base::names(x) <- "lcz"
  # lcz_shp <- terra::as.polygons(x) %>%
  #   sf::st_as_sf() %>%
  #   sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  # Get shp LCZ stations from lat and long
  stations_mod <- df_processed %>%
    dplyr::distinct(.data$longitude, .data$latitude, .keep_all = T) %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (extract.method == "simple") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "simple")
    stations_lcz$ID <- NULL

    lcz_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      ) %>%
      dplyr::select(.data$station, .data$lcz, .data$lcz_id)

    lcz_model <- dplyr::inner_join(df_processed %>%
                                     dplyr::select(.data$date, .data$lcz_id, .data$var_interp, .data$latitude, .data$longitude),
                                   lcz_model, by="lcz_id")
  }

  if (extract.method == "bilinear") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "bilinear")
    stations_lcz$ID <- NULL
    stations_lcz$lcz <- as.integer(stations_lcz$lcz)
    lcz_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      )  %>%
      dplyr::select(.data$station, .data$lcz, .data$lcz_id)

    lcz_model <- dplyr::inner_join(df_processed %>% dplyr::select(.data$date, .data$lcz_id, .data$var_interp, .data$latitude, .data$longitude), lcz_model, by="lcz_id")
  }

  if (extract.method == "two.step") {
    # Step 2: Define a function to filter stations based on LCZ homogeneity
    filter_homogeneous_lcz <- function(df, raster, kernel_size = 5, threshold = 0.8) {
      # Exact k x k homogeneity at station locations (no per-cell R callback).
      df$homogeneity <- lcz_homogeneity(raster, df, kernel_size = kernel_size)
      df[!is.na(df$homogeneity) & df$homogeneity >= threshold, , drop = FALSE]
    }

    # Step 3: Apply the function to filter stations
    df_homogeneous <- filter_homogeneous_lcz(stations_mod, x, kernel_size = 5, threshold = 0.8)
    stations_lcz <- terra::extract(x, terra::vect(df_homogeneous))
    stations_lcz$ID <- NULL
    lcz_model <- base::cbind(df_homogeneous, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      ) %>%
      dplyr::select(.data$station, .data$lcz, .data$lcz_id)
    lcz_model <- dplyr::inner_join(df_processed %>% dplyr::select(.data$date, .data$lcz_id, .data$var_interp, .data$latitude, .data$longitude), lcz_model, by="lcz_id")
  }


  if (nrow(lcz_model) == 0L) {
    lcz_abort("err_no_stations", lang = lang)
  }

  # Settings for plots ------------------------------------------------------

  # Get id stations
  my_stations <- lcz_model %>%
    dplyr::distinct(.data$lcz_id, .data$lcz, .data$station)

  lcz_df <- dplyr::inner_join(
    tibble::tibble(
      lcz = as.character(c(1:10, 11:17)),
      lcz.name = lcz_class_names(lang),
      lcz.col = unname(lcz_palette())
    ),
    dplyr::mutate(my_stations, lcz = as.character(.data$lcz)),
    by = "lcz"
  )

  # Define LCZ labels
  lcz.lables <- lcz_df$station
  nb.cols <- base::length(unique(my_stations$station))
  mycolors <- if (identical(palette, "okabe")) lcz_pal_categorical(nb.cols) else MetBrewer::met.brewer(name = palette, nb.cols)
  # A legend with dozens of stations would leave no room for the data.
  legend_pos <- if (nb.cols > 20L) "none" else "top"
  if (nb.cols > 20L) {
    lcz_warn("warn_many_stations", lang = lang)
  }
  lcz_theme <- lcz_theme_series(legend_position = legend_pos)

  # Extract the hemisphere (terra-only; no raster:: round-trip)
  hemisphere <- lcz_hemisphere(x)

  my_latitude <- lcz_model$latitude[1]
  my_longitude <- lcz_model$longitude[1]

  lcz_model$latitude <- NULL
  lcz_model$longitude <- NULL

  # Define time series frequency with argument "by"--------------------------------------------

  if (is.null(by)) {
    mydata <- openair::timeAverage(
      lcz_model,
      pollutant = "var_interp",
      avg.time = time.freq,
      type = c("station", "lcz")
    ) %>%
      stats::na.omit() %>%
      dplyr::ungroup()

    # Argument plot type checks
    if (!plot_type %in% c("basic_line", "facet_line", "heatmap", "warming_stripes")) {
      stop("Invalid plot_type. Only 'basic_line', 'facet_line', 'heatmap, 'warming_stripes' are supported.")
    }

    if (!facet_plot %in% c("LCZ", "station")) {
      stop("Invalid facet_plot option. Choose 'LCZ' or 'station'.")
    }

    if(plot_type == "basic_line") {
      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(
          x = .data$date,
          y = .data$var_interp,
          color = .data$station
        )) +
        ggplot2::geom_line(linewidth = 0.55, alpha = if (smooth) 0.3 else 1) +
        ggplot2::scale_x_datetime(
          expand = c(0, 0),
          date_labels = lcz_date_labels(mydata$date),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        ggplot2::scale_color_manual(
          name = lcz_tr("station_lcz", lang = lang),
          values = mycolors,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption) +
        lcz_theme

      if(smooth==TRUE) {
        final_graph <- final_graph +
          ggplot2::geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE)
      }

      }

    if(plot_type == "facet_line") {

      final_graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(
          x = .data$date,
          y = .data$var_interp,
          color = if(facet_plot=="LCZ") .data$station else .data$lcz
        )) +
        ggplot2::geom_line(linewidth = 0.55, alpha = if (smooth) 0.3 else 1) +
        ggplot2::scale_x_datetime(
          expand = c(0, 0),
          date_labels = lcz_date_labels(mydata$date),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        ggplot2::scale_color_manual(
          name = if (facet_plot == "station") lcz_tr("lcz_class", lang = lang) else lcz_tr("station_lcz", lang = lang),
          values = if(facet_plot == "LCZ") mycolors else lcz.col,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption) +
        lcz_theme +
        ggplot2::theme(
          legend.box.spacing = ggplot2::unit(20, "pt"),
          panel.spacing = ggplot2::unit(3, "lines"),
          axis.ticks.x = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(
            face = "bold",
            hjust = 0.5,
            size = 13
          ),
          strip.background = ggplot2::element_rect(linetype = "dotted")
        )

      facet_var <- base::ifelse(facet_plot == "LCZ", "lcz", "station")

      # Apply faceting and hide legend if faceting by "station"
      final_graph <- final_graph +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_var)),
          labeller = if (facet_var == "lcz") {
            ggplot2::labeller(lcz = function(v) paste("LCZ", v))
          } else {
            "label_value"
          }
        )

      if(smooth==TRUE) {
        final_graph <- final_graph +
          ggplot2::geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE)
      }

    }

    if(plot_type == "heatmap") {

      final_graph <- ggplot2::ggplot(mydata, ggplot2::aes(
        x = .data$date,
        y = 1,  # Use station on the y-axis for clarity
        fill = .data$var_interp)) +
        ggplot2::geom_tile() +  # Add thin gridlines between tiles
        ggplot2::scale_x_datetime(
          expand = c(0, 0),
          date_labels = lcz_date_labels(mydata$date),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        lcz_scale_fill_sequential(
          name = legend_name, option = "magma", direction = -1,
          guide = ggplot2::guide_colorbar(
            barwidth = ggplot2::unit(55, "mm"),
            barheight = ggplot2::unit(2.5, "mm"),
            title.position = "top", title.hjust = 0.5,
            frame.colour = "grey70", ticks.colour = "grey40"
          )
        ) +
        ggplot2::labs(title = title, x = xlab, y = "", fill = legend_name, caption = caption) +
        lcz_theme +
        ggplot2::theme(
          legend.position = "bottom",  # Position legend at the bottom
          legend.box.spacing = ggplot2::unit(13, "pt"),  # Adjust legend spacing
          panel.spacing = ggplot2::unit(2, "lines"),  # Adjust spacing between panels
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),  # Customize y-axis text for stations
          axis.text.x = ggplot2::element_text(hjust = 0.1),
          strip.text = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),  # Centered strip text
          strip.background = ggplot2::element_rect(fill = "grey90", color = NA, linetype = "solid"),  # Improved facet strip background
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )

      facet_var <- base::ifelse(facet_plot == "LCZ", "lcz", "station")

      # Apply faceting and hide legend if faceting by "station"
      final_graph <- final_graph +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_var)),
          labeller = if (facet_var == "lcz") {
            ggplot2::labeller(lcz = function(v) paste("LCZ", v))
          } else {
            "label_value"
          }
        )

    }

    if(plot_type == "warming_stripes") {

      final_graph <- ggplot2::ggplot(mydata, ggplot2::aes(
        x = .data$date,
        y = 1,  # Use station on the y-axis for clarity
        fill = .data$var_interp)) +
        ggplot2::geom_tile() +  # Add thin gridlines between tiles
        ggplot2::scale_x_datetime(
          expand = c(0, 0),
          date_labels = lcz_date_labels(mydata$date),
          guide = ggplot2::guide_axis(check.overlap = TRUE)
        ) +
        lcz_scale_stripes(mydata$var_interp, legend_name) +
        ggplot2::coord_cartesian(expand = FALSE)+
        ggplot2::labs(title = title, x = xlab, y = "", fill = legend_name, caption = caption) +
        ggplot2::theme(
          legend.position = "bottom",
          panel.grid = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )

      facet_var <- base::ifelse(facet_plot == "LCZ", "lcz", "station")

      # Apply faceting and hide legend if faceting by "station"
      final_graph <- final_graph +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_var)),
          labeller = if (facet_var == "lcz") {
            ggplot2::labeller(lcz = function(v) paste("LCZ", v))
          } else {
            "label_value"
          }
        )

    }

    if (isave == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_plot.", save_extension)
      ggplot2::ggsave(file.1, final_graph, height = 9, width = 16, units = "in", dpi = 600)
      file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_df.csv")
      utils::write.csv(mydata, file.2)
      file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_stations.csv")
      utils::write.csv(lcz_df, file.3)
      lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))

      }

    if (iplot == FALSE) {
      mydata$date <- lubridate::as_datetime(mydata$date)

      return(mydata)
    } else {
      return(final_graph)
    }
  }

  if (!is.null(by)) {
    if ("day" %in% by) {
      lcz_abort("err_by_day", lang = lang)
    }

    if (length(by) < 2 && any(c("month", "year", "season", "seasonyear", "yearseason") %in% by)) {
      mydata <- openair::cutData(lcz_model,
        type = by, hemisphere = hemisphere,
        latitude = my_latitude, longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")
      ) %>%
        stats::na.omit() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(date = base::as.factor(date))

      label_format <- switch(by,
        "season" = "%b %d",
        "seasonyear" = "%b %d",
        "yearseason" = "%b %d",
        "year" = "%b %d",
        "month" = "%d",
        "%b %d" # Default case if none of the above match
      )

      if (plot_type == "facet_line") {
        lcz_warn("warn_switch_line", lang = lang)
        plot_type <- "basic_line"
      }

      if(plot_type == "basic_line") {

        final_graph <-
          ggplot2::ggplot(
            mydata,
            ggplot2::aes(
              x = .data$date,
              y = .data$var_interp,
              color = .data$station,
              group = .data$station
            )
          ) +
          ggplot2::scale_x_discrete(
            expand = c(0, 0),
            breaks = function(x) x[seq(1, length(x), by = 1 * 24)],
            labels = function(x) base::format(lubridate::as_datetime(x), paste0(label_format)),
            guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)
          ) +
          ggplot2::geom_line(linewidth = if (smooth) 0.45 else 0.55, alpha = if (smooth) 0.3 else 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = lcz_tr("station_lcz", lang = lang),
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption)+
          ggplot2::facet_wrap(ggplot2::vars(my_time), scales = "free_x") +
          lcz_theme +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

        if(smooth==TRUE) {
          final_graph <- final_graph +
            ggplot2::geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE)
        }
      }

      if(plot_type == "heatmap") {

        final_graph <-
          ggplot2::ggplot(
            mydata,
            ggplot2::aes(
              x = .data$date,
              y = 1,
              fill = .data$var_interp
            )
          ) +
          ggplot2::scale_x_discrete(
            expand = c(0, 0),
            breaks = function(x) x[seq(1, length(x), by = 1 * 24)],
            labels = function(x) base::format(lubridate::as_datetime(x), paste0(label_format)),
            guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)
          ) +
          ggplot2::geom_tile() +  # Add thin gridlines between tiles
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          lcz_scale_fill_sequential(
          name = legend_name, option = "magma", direction = -1,
          guide = ggplot2::guide_colorbar(
            barwidth = ggplot2::unit(55, "mm"),
            barheight = ggplot2::unit(2.5, "mm"),
            title.position = "top", title.hjust = 0.5,
            frame.colour = "grey70", ticks.colour = "grey40"
          )
        ) +
          ggplot2::labs(title = title, x = xlab, y = "", fill = legend_name, caption = caption) +
          lcz_theme +
          ggplot2::facet_wrap(ggplot2::vars(my_time), scales = "free_x") +
          ggplot2::theme(
            legend.position = "bottom",
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

      }

      if(plot_type == "warming_stripes") {

        final_graph <-
          ggplot2::ggplot(
            mydata,
            ggplot2::aes(
              x = .data$date,
              y = 1,
              fill = .data$var_interp
            )
          ) +
          ggplot2::scale_x_discrete(
            expand = c(0, 0),
            breaks = function(x) x[seq(1, length(x), by = 1 * 24)],
            labels = function(x) base::format(lubridate::as_datetime(x), paste0(label_format)),
            guide = ggplot2::guide_axis(check.overlap = TRUE, angle = 90)
          ) +
          lcz_scale_stripes(mydata$var_interp, legend_name) +
          ggplot2::coord_cartesian(expand = FALSE)+
          ggplot2::labs(title = title, x = xlab, y = "", fill = legend_name, caption = caption) +
          lcz_theme +
          ggplot2::facet_wrap(ggplot2::vars(my_time), scales = "free_x") +
          ggplot2::theme(
            legend.position = "bottom",
            panel.grid = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          )
      }

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 9, width = 16, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      if (iplot == FALSE) {
        mydata$date <- lubridate::as_datetime(mydata$date)
        return(mydata)
      } else {
        return(final_graph)
      }
    }

    if (length(by) < 2 && "daylight" %in% by) {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      rec_df <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$my_time)
        ) %>%
        dplyr::ungroup()

      if(plot_type=="basic_line"){

        final_graph <-
          ggplot2::ggplot(
            mydata,
            ggplot2::aes(
              x = .data$date,
              y = .data$var_interp,
              color = .data$station,
              group = .data$station
            )
          ) +
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf, fill = .data$labs),
                             alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::scale_x_datetime(
            expand = c(0, 0),
            breaks = scales::date_breaks("3 hour"),
            labels = scales::date_format("%H"),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::geom_line(linewidth = 0.55) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = lcz_tr("station_lcz", lang = lang),
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption) +
          lcz_theme


      }

      if(plot_type=="facet_line"){

        mydata <- dplyr::mutate(mydata, lcz = gsub(".*\\((\\d+)\\).*", "\\1", .data$station))

        final_graph <-
          ggplot2::ggplot(
            mydata,
            ggplot2::aes(
              x = .data$date,
              y = .data$var_interp,
              color = if(facet_plot=="LCZ") .data$station else .data$lcz,
              group = if(facet_plot=="LCZ") .data$station else .data$lcz
            )
          ) +
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf, fill = .data$labs),
                             alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::scale_x_datetime(
            expand = c(0, 0),
            breaks = scales::date_breaks("3 hour"),
            labels = scales::date_format("%H"),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +
          ggplot2::geom_line(linewidth = 0.55) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = if (facet_plot == "station") lcz_tr("lcz_class", lang = lang) else lcz_tr("station_lcz", lang = lang),
            values = if(facet_plot == "LCZ") mycolors else lcz.col,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption) +
          lcz_theme +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(3, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0.5,
              size = 13
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

        facet_var <- base::ifelse(facet_plot == "LCZ", "lcz", "station")

        # Apply faceting and hide legend if faceting by "station"
        final_graph <- final_graph +
          ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(facet_var)),
          labeller = if (facet_var == "lcz") {
            ggplot2::labeller(lcz = function(v) paste("LCZ", v))
          } else {
            "label_value"
          }
        )


      }

      if (plot_type == "warming_stripes") {
        lcz_warn("warn_switch_heatmap", lang = lang)
        plot_type <- "heatmap"
      }

      if(plot_type=="heatmap"){

        mydata <- dplyr::mutate(mydata, lcz = base::gsub(".*\\((\\d+)\\).*", "\\1", .data$station))

        final_graph <-
          # Create an elegant heatmap with clear daylight and nighttime transitions
          ggplot2::ggplot(
            mydata,
            ggplot2::aes(
              x = .data$date,
              y = if(facet_plot == "LCZ") .data$lcz else .data$station,
              fill = .data$var_interp
            )
          ) +
          # Heatmap for var_interp over time
          ggplot2::geom_tile() +

          # X-axis as datetime with proper breaks and labels
          ggplot2::scale_x_datetime(
            expand = c(0, 0),
            breaks = scales::date_breaks("3 hour"),
            labels = scales::date_format("%H"),
            guide = ggplot2::guide_axis(check.overlap = TRUE)
          ) +

          # Y-axis as discrete variable without expansion
          ggplot2::scale_y_discrete(expand = c(0, 0)) +

          # Fill scale for heatmap using MetBrewer palette
          lcz_scale_fill_sequential(
          name = legend_name, option = "magma", direction = -1,
          guide = ggplot2::guide_colorbar(
            barwidth = ggplot2::unit(55, "mm"),
            barheight = ggplot2::unit(2.5, "mm"),
            title.position = "top", title.hjust = 0.5,
            frame.colour = "grey70", ticks.colour = "grey40"
          )
        ) +

          # Add daylight start (solid line) using geom_segment
          ggplot2::geom_segment(
            data = rec_df[1,],
            ggplot2::aes(
              x = .data$xmin,
              xend = .data$xmin,  # Vertical line at daylight start
              y = -Inf,
              yend = Inf,
              color = "Daylight start"
            ),
            size = 1,
            inherit.aes = FALSE
          ) +
          # Add nighttime start (dashed line) using geom_segment
          ggplot2::geom_segment(
            data = rec_df[1,],
            ggplot2::aes(
              x = .data$xmax,
              xend = .data$xmax,  # Vertical line at nighttime start
              y = -Inf,
              yend = Inf,
              color = "Nighttime start"
            ),
            size = 1,
            inherit.aes = FALSE
          ) +
          ggplot2::scale_color_manual(name = "", values = c("Daylight start" = "black", "Nighttime start" = "grey40")) +
           # Add labels and theme elements
          ggplot2::labs(
            title = title,
            x = xlab,
            y = if (facet_plot == "LCZ") lcz_tr("lcz_class", lang = lang) else lcz_tr("station_lcz", lang = lang),
            fill = legend_name,
            caption = caption
          ) +
          # Customize the fill color bar guide for heatmap
          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              title.hjust = 0.5,    # Center the title of the color bar
              barheight = ggplot2::unit(15, "lines")  # Increase color bar height
            )
          ) +
          # Apply a clean theme
          lcz_theme

      }


      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 9, width = 16, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      if (iplot == FALSE) {
        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata)
      } else {
        return(final_graph)
      }
    }

    if (length(by) > 1 && "daylight" %in% by) {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", by)
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      new_var <- rlang::syms(by)

      mydata2 <- mydata %>%
        dplyr::mutate(hour = lubridate::hour(.data$date)) %>%
        dplyr::group_by(.data$station, !!!new_var, .data$hour) %>%
        dplyr::summarize(var_interp = base::mean(.data$var_interp), .groups = "drop") %>%
        dplyr::ungroup()

      rec_df <- mydata %>%
        dplyr::group_by(!!!new_var) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$daylight), .groups = "drop"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          min_hour = lubridate::hour(.data$xmin),
          max_hour = lubridate::hour(.data$xmax)
        )

      # convert the string to a formula
      by_formula <- stats::as.formula(base::paste("~", base::paste(by[2], collapse = " + ")))

      if (plot_type == "facet_line") {
        lcz_warn("warn_switch_line", lang = lang)
        plot_type <- "basic_line"
      }

      if (plot_type == "basic_line") {

        graph <-
          ggplot2::ggplot(mydata2, ggplot2::aes(
            x = .data$hour,
            y = .data$var_interp,
            color = .data$station,
            group = .data$station
          )) +
          ggplot2::geom_rect(ggplot2::aes(xmin = .data$min_hour, xmax = .data$max_hour, ymin = -Inf, ymax = Inf, fill = .data$labs),
                             alpha = 0.3, data = rec_df, inherit.aes = FALSE
          ) +
          ggplot2::scale_fill_manual(name = "", values = c("lightblue", "grey")) +
          ggplot2::scale_x_continuous(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::geom_line(linewidth = 0.55) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = lcz_tr("station_lcz", lang = lang),
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption)
        final_graph <-
          graph + ggplot2::facet_wrap(by_formula, scales = "free_y") +
          lcz_theme +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(1, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

      }


      if (plot_type == "warming_stripes") {
        lcz_warn("warn_switch_heatmap", lang = lang)
        plot_type <- "heatmap"
      }

      if(plot_type=="heatmap"){

        mydata2 <- dplyr::mutate(mydata2, lcz = base::gsub(".*\\((\\d+)\\).*", "\\1", .data$station))

        final_graph <-
          # Create an elegant heatmap with clear daylight and nighttime transitions
          ggplot2::ggplot(
            mydata2,
            ggplot2::aes(
              x = .data$hour,
              y = if(facet_plot == "LCZ") .data$lcz else .data$station,
              fill = .data$var_interp
            )
          ) +
          # Heatmap for var_interp over time
          ggplot2::geom_tile() +

          # X-axis as datetime with proper breaks and labels
          ggplot2::scale_x_continuous(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +

          # Y-axis as discrete variable without expansion
          ggplot2::scale_y_discrete(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +

          # Fill scale for heatmap using MetBrewer palette
          lcz_scale_fill_sequential(
          name = legend_name, option = "magma", direction = -1,
          guide = ggplot2::guide_colorbar(
            barwidth = ggplot2::unit(55, "mm"),
            barheight = ggplot2::unit(2.5, "mm"),
            title.position = "top", title.hjust = 0.5,
            frame.colour = "grey70", ticks.colour = "grey40"
          )
        ) +

          # Add daylight start (solid line) using geom_segment
          ggplot2::geom_segment(
            data = rec_df[1,],
            ggplot2::aes(
              x = .data$min_hour,
              xend = .data$min_hour,  # Vertical line at daylight start
              y = -Inf,
              yend = Inf,
              color = "Daylight start"
            ),
            size = 1,
            inherit.aes = FALSE
          ) +
          # Add nighttime start (dashed line) using geom_segment
          ggplot2::geom_segment(
            data = rec_df[1,],
            ggplot2::aes(
              x = .data$max_hour,
              xend = .data$max_hour,  # Vertical line at nighttime start
              y = -Inf,
              yend = Inf,
              color = "Nighttime start"
            ),
            size = 1,
            inherit.aes = FALSE
          ) +
          ggplot2::scale_color_manual(name = "", values = c("Daylight start" = "black", "Nighttime start" = "grey40")) +
          # Add labels and theme elements
          ggplot2::labs(
            title = title,
            x = xlab,
            y = if (facet_plot == "LCZ") lcz_tr("lcz_class", lang = lang) else lcz_tr("station_lcz", lang = lang),
            fill = legend_name,
            caption = caption
          ) +
          # Customize the fill color bar guide for heatmap
          ggplot2::guides(
            fill = ggplot2::guide_colorbar(
              title.hjust = 0.5,    # Center the title of the color bar
              barheight = ggplot2::unit(15, "lines")  # Increase color bar height
            )
          )

        final_graph <-
          final_graph + ggplot2::facet_wrap(by_formula, scales = "free_y") +
          lcz_theme +
          ggplot2::theme(
            legend.box.spacing = ggplot2::unit(20, "pt"),
            panel.spacing = ggplot2::unit(1, "lines"),
            axis.ticks.x = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
              face = "bold",
              hjust = 0,
              size = 10
            ),
            strip.background = ggplot2::element_rect(linetype = "dotted")
          )

      }

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 9, width = 16, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_df.csv")
        utils::write.csv(mydata2, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      if (iplot == FALSE) {
        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata2)
      } else {
        return(final_graph)
      }
    } else {
      mydata <-
        openair::cutData(
          lcz_model,
          type = by,
          hemisphere = hemisphere,
          latitude = my_latitude,
          longitude = my_longitude
        ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        mydata,
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("station", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit()

      graph <-
        ggplot2::ggplot(mydata, ggplot2::aes(
          x = .data$date,
          y = .data$var_interp,
          color = .data$station
        )) +
        ggplot2::geom_line(linewidth = 0.55) +
        ggplot2::scale_x_datetime(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(
          name = lcz_tr("station_lcz", lang = lang),
          values = mycolors,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, color = lcz_tr("station_lcz", lang = lang), caption = caption)

      final_graph <-
        graph + ggplot2::facet_wrap(ggplot2::vars(my_time), scales = "free_x") +
        lcz_theme +
        ggplot2::theme(
          legend.box.spacing = ggplot2::unit(20, "pt"),
          panel.spacing = ggplot2::unit(3, "lines"),
          axis.ticks.x = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(
            face = "bold",
            hjust = 0,
            size = 10
          ),
          strip.background = ggplot2::element_rect(linetype = "dotted")
        )

      if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_plot.", save_extension)
        ggplot2::ggsave(file.1, final_graph, height = 9, width = 16, units = "in", dpi = 600)
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_ts_stations.csv")
        utils::write.csv(lcz_df, file.3)
        lcz_msg("msg_files_saved", lang = lang, path = base::file.path(getwd(), folder))
      }

      if (iplot == FALSE) {
        mydata$date <- lubridate::as_datetime(mydata$date)

        return(mydata)
      } else {
        return(final_graph)
      }
  }
  }
}
