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
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time).See argument \emph{type} in openair package: \url{https://bookdown.org/david_carslaw/openair/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series) into your directory.
#' @param save_extension A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.
#' @param palette A character string specifying the color palette to use. The default is \dQuote{VanGogh2}. You can choose from palettes available in \code{MetBrewer}: \url{https://github.com/BlakeRMills/MetBrewer?tab=readme-ov-file#palettes}.
#' @param ylab A character string for the y-axis label. The default is the variable name.
#' @param xlab A character string for the x-axis label. The default is \dQuote{Time}.
#' @param title A character string for the plot title. The default is \dQuote{""}.
#' @param caption source data. Default can be \dQuote{Source:LCZ4r,2024"}.
#' @param legend_name Legend name for heatmap and warming stripes plots. Default is "Temperature (°C)".
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
                   palette = "VanGogh2",
                   ylab = "Air temperature [Degree Celsius]",
                   xlab = "Time",
                   title = "LCZ - Timeseries",
                   caption = "LCZ4r, 2024.",
                   legend_name = "Temperature (C)") {
  # Check and validate raster inputs -----------------------------------------------

  if (missing(x)) {
    stop("The input must be raster object. Please, use the lcz_get_map* functions")
  }
  if (!inherits(x, "SpatRaster")) {
    warning("The input 'x' is not a SpatRaster object. Attempting to convert it using terra::rast()")
    x <- try(terra::rast(x), silent = TRUE)
    if (inherits(x, "try-error")) {
      stop("Failed to convert input 'x' to SpatRaster. Please provide a valid SpatRaster object.")
    }
  }

  if (terra::nlyr(x) > 1) {
    x <- x[[2]]
  }

  if (terra::crs(x, proj = TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {
    warning("The input 'x' is not in WGS84 projection. Reprojecting to WGS84.")
    x <- terra::project(x, "+proj=longlat +datum=WGS84 +no_defs")
  }


  # Check data inputs -------------------------------------------------------
  if (!is.data.frame(data_frame)) {
    stop("The 'data_frame' input must be a data frame containing air temperature measurements,station IDs, latitude, and longitude.")
  }

  if (missing(var) || !var %in% colnames(data_frame)) {
    stop("The 'var' input must be a column name in 'data_frame' representing air temperature.")
  }

  if (missing(station_id) || !station_id %in% colnames(data_frame)) {
    stop("The 'station_id' input must be a column name in 'data_frame' representing stations.")
  }

  if (!("Latitude" %in% tolower(colnames(data_frame)) || "latitude" %in% tolower(colnames(data_frame)))) {
    stop("The 'latitude' input must be a column name in 'data_frame' representing each station's latitude.")
  }

  if (!("Longitude" %in% tolower(colnames(data_frame)) || "longitude" %in% tolower(colnames(data_frame)))) {
    stop("The 'longitude' input must be a column name in 'data_frame' representing each station's longitude")
  }
  if (!(extract.method %in% c("simple", "bilinear", "two.step"))) {
    stop("Invalid extract-based pixel model. Choose from 'simple', 'bilinear', or 'two.step'.")
  }

  # Pre-processing time series ----------------------------------------------

  # Rename and define lcz_id for each lat and long
  df_processed <- data_frame %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    janitor::clean_names() %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      lcz_id = dplyr::cur_group_id(),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    openair::selectByDate(...)

  df_processed$var_interp <- base::as.numeric(df_processed$var_interp)
  df_processed$latitude <- base::as.numeric(df_processed$latitude)
  df_processed$longitude <- base::as.numeric(df_processed$longitude)

  # Impute missing values if necessary
  missing_values = c("NAN","NaN", "-9999", "-99", "NULL", "",
                     "NA", "N/A", "na", "missing", ".",
                     "inf", "-inf", 9999, 999, Inf, -Inf)
  df_processed <- df_processed %>%
    dplyr::mutate(var_interp = ifelse(.data$var_interp %in% missing_values, NA, .data$var_interp))


  if (!is.null(impute)) {
    impute_methods <- c("mean", "median", "knn", "bag")
    if (!(impute %in% impute_methods)) {
      stop("Invalid impute method. Choose from 'mean', 'median', 'knn', or 'bag'.")
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
    base::message("The missing values have been imputed with ", impute)
  }

  # Geospatial operations ---------------------------------------------------

  # Convert lcz_map to polygon
  base::names(x) <- "lcz"
  lcz_shp <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

  # Get shp LCZ stations from lat and long
  stations_mod <- df_processed %>%
    stats::na.omit() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  if (extract.method == "simple") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod))
    stations_lcz$ID <- NULL
    lcz_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz),
        lcz_id = base::as.factor(.data$lcz_id),
        station = base::as.factor(paste0(.data$station, "(", lcz, ")"))
      )
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
      )
  }

  if (extract.method == "two.step") {
    # Step 2: Define a function to filter stations based on LCZ homogeneity
    filter_homogeneous_lcz <- function(df, raster, kernel_size = 5, threshold = 0.8) {
      # Convert stations to SpatVector
      stations_vect <- terra::vect(df)

      # Create a moving kernel to analyze LCZ homogeneity
      kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)

      # Extract LCZ values in the kernel around each station
      lcz_homogeneity <- terra::focal(raster, w = kernel, fun = function(values) {
        # Compute percentage of pixels matching the center pixel
        center_pixel <- values[ceiling(length(values) / 2)]
        if (is.na(center_pixel)) {
          return(NA)
        }
        mean(values == center_pixel, na.rm = TRUE)
      })

      # Extract homogeneity values at station locations
      homogeneity_values <- terra::extract(lcz_homogeneity, stations_vect)

      # Add homogeneity values to the station data frame
      df$homogeneity <- homogeneity_values[, 2]  # Second column contains the homogeneity value

      # Filter stations where homogeneity meets or exceeds the threshold
      df_filtered <- df %>%
        dplyr::filter(.data$homogeneity >= threshold)

      return(df_filtered)
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
      )
  }


  # Settings for plots ------------------------------------------------------

  # Get id stations
  my_stations <- lcz_model %>%
    dplyr::distinct(.data$lcz_id, .data$lcz, .data$station)

  lcz <- tibble::as_tibble(c(seq(1, 10, 1), seq(11, 17))) %>%
    purrr::set_names("lcz")

  lcz.name <- tibble::as_tibble(c(
    "Compact highrise", "Compact midrise", "Compact lowrise",
    "Open highrise", "Open midrise", "Open lowrise",
    "Lightweight low-rise", "Large lowrise", "Sparsely built",
    "Heavy Industry", "Dense trees", "Scattered trees",
    "Bush, scrub", "Low plants", "Bare rock or paved",
    "Bare soil or sand", "Water"
  )) %>%
    purrr::set_names("lcz.name")

  lcz.col <- c(
   "1"= "#910613",
   "2"=  "#D9081C",
   "3"=  "#FF0A22",
   "4"= "#C54F1E",
   "5"= "#FF6628",
   "6"=  "#FF985E",
   "7"= "#FDED3F",
   "8"= "#BBBBBB",
   "9"= "#FFCBAB",
   "10"= "#565656",
   "11"= "#006A18",
   "12"= "#00A926",
   "13"= "#628432",
   "14"= "#B5DA7F",
   "15"= "#000000",
   "16"= "#FCF7B1",
   "17"= "#656BFA"
  )

  lcz_df <- dplyr::bind_cols(lcz, lcz.name) %>%
    dplyr::mutate(lcz = base::as.factor(.data$lcz)) %>%
    dplyr::inner_join(my_stations, by = "lcz")


  # Define LCZ labels
  lcz.lables <- lcz_df$station
  nb.cols <- base::length(my_stations$station)
  mycolors <- MetBrewer::met.brewer(name = palette, nb.cols)

  lcz_theme <-
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "black", size = 16, hjust = 0.5),
      plot.background = ggplot2::element_rect(fill="transparent", color=NA),
      legend.background = ggplot2::element_rect(fill="transparent", color=NA),
      panel.grid.minor = ggplot2::element_line(color = "white"),
      panel.grid.major.y = ggplot2::element_line(color = "grey90"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.5)),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.5)),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 13, face = "bold"),
      legend.key = ggplot2::element_blank(),
      legend.spacing.y = ggplot2::unit(0.02, "cm"),
      plot.margin = ggplot2::margin(25, 25, 10, 25),
      plot.caption = ggplot2::element_text(color = "grey40", hjust = 1, size = 10)
    )

  # Define hemisphere
  extract_hemisphere <- function(raster) {
    # Get the extent of the raster
    extent <- raster::extent(raster::raster(raster))
    # Check the ymin value of the extent
    if (extent@ymin >= 0) {
      hemisphere <- "northern"
    } else {
      hemisphere <- "southern"
    }

    return(hemisphere)
  }

  # Extract the hemisphere
  hemisphere <- extract_hemisphere(raster = {{ x }})

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
        ggplot2::geom_line(lwd = 1, alpha =if(smooth==TRUE) 0.3 else 1) +
        ggplot2::scale_x_datetime(expand = c(0, 0)) +
        ggplot2::scale_color_manual(
          name = "Station (LCZ)",
          values = mycolors,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
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
        ggplot2::geom_line(lwd = 1, alpha = if(smooth==TRUE) 0.3 else 1) +
        ggplot2::scale_x_datetime(expand = c(0, 0)) +
        ggplot2::scale_color_manual(
          name = if(facet_plot == "station") "LCZ class" else "Station (LCZ)",
          values = if(facet_plot == "LCZ") mycolors else lcz.col,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
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
        ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))

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
        ggplot2::scale_x_datetime(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        MetBrewer::scale_fill_met_c(name = palette, direction = -1) +
        ggplot2::labs(title = title, x = xlab, y = "", fill = legend_name, caption = caption) +
        lcz_theme +
        ggplot2::theme(
          legend.position = "bottom",  # Position legend at the bottom
          legend.title = ggplot2::element_text(size = 14, face = "bold"),
          legend.text = ggplot2::element_text(size = 12),
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
        ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))

    }

    if(plot_type == "warming_stripes") {

      final_graph <- ggplot2::ggplot(mydata, ggplot2::aes(
        x = .data$date,
        y = 1,  # Use station on the y-axis for clarity
        fill = .data$var_interp)) +
        ggplot2::geom_tile() +  # Add thin gridlines between tiles
        ggplot2::scale_x_datetime(expand = c(0, 0)) +
        ggplot2::geom_tile()+
        ggplot2::scale_fill_stepsn(colors = c("#08306B", "white", "#67000D"),
                                   values =scales::rescale(c(base::min(mydata$var_interp), 0, base::max(mydata$var_interp))),
                                   guide = ggplot2::guide_colorsteps(  # Corrected the usage of guide_colorsteps
                                     barwidth = 16,
                                     barheight = 1,
                                     title.position = "top",
                                     title.hjust = 0.5
                                   )
                                   ) +
        ggplot2::coord_cartesian(expand = FALSE)+
        ggplot2::labs(title = title, x = xlab, y = "", fill = legend_name, caption = caption) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 22, face = "bold", lineheight = 0.8),
          legend.position = "bottom",  # Position legend at the bottom
          legend.title = ggplot2::element_text(size = 14, face = "bold"),
          legend.text = ggplot2::element_text(size = 12),
          legend.box.spacing = ggplot2::unit(13, "pt"),  # Adjust legend spacing
          panel.spacing = ggplot2::unit(2, "lines"),  # Adjust spacing between panels
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.5), margin = ggplot2::margin(t=5, b=10, unit = "pt"), hjust=0.1),
          axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.y = ggplot2::element_blank(),  # Customize y-axis text for stations
          strip.text = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),  # Centered strip text
          strip.background = ggplot2::element_rect(fill = "grey90", color = NA, linetype = "solid"),  # Improved facet strip background
          panel.grid.major = ggplot2::element_line(size = 0.1, color = "grey90"),
          panel.grid.minor = ggplot2::element_blank()
        )

      facet_var <- base::ifelse(facet_plot == "LCZ", "lcz", "station")

      # Apply faceting and hide legend if faceting by "station"
      final_graph <- final_graph +
        ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))

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
      base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))

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
      stop("The 'day' does not work with the argument by")
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
        message("The 'facet_line' plot does not work with the active 'by' argument. Switching to 'basic_line' plot.")
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
          ggplot2::geom_line(lwd =if(smooth==TRUE) 0.6 else 1, alpha=if(smooth==TRUE) 0.3 else 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = "Station (LCZ)",
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption)+
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
          MetBrewer::scale_fill_met_c(name = palette, direction = -1) +
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
          ggplot2::geom_tile()+
          ggplot2::scale_fill_stepsn(colors = c("#08306B", "white", "#67000D"),
                                     values =scales::rescale(c(base::min(mydata$var_interp), 0, base::max(mydata$var_interp))),
                                     guide = ggplot2::guide_colorsteps(  # Corrected the usage of guide_colorsteps
                                       barwidth = 16,
                                       barheight = 1,
                                       title.position = "top",
                                       title.hjust = 0.5
                                     )
          ) +
          ggplot2::coord_cartesian(expand = FALSE)+
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
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
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
          ggplot2::geom_line(lwd = 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = "Station (LCZ)",
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
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
          ggplot2::geom_line(lwd = 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = if(facet_plot == "station") "LCZ class" else "Station (LCZ)",
            values = if(facet_plot == "LCZ") mycolors else lcz.col,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption) +
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
          ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))


      }

      if (plot_type == "warming_stripes") {
        message("The 'warming_stripes' plot does not work with the active by='daylight' argument. Switching to 'heatmap' plot.")
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
          MetBrewer::scale_fill_met_c(name = palette, direction = -1) +

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
            y = if(facet_plot == "LCZ") "LCZ class" else "Station (LCZ)",
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
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
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
        message("The 'facet_line' plot does not work with the active 'by' argument. Switching to 'basic_line' plot.")
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
          ggplot2::geom_line(lwd = 1) +
          ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
          ggplot2::scale_color_manual(
            name = "Station (LCZ)",
            values = mycolors,
            labels = lcz.lables,
            guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
          ) +
          ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption)
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
        message("The 'warming_stripes' plot does not work with the active by='daylight' argument. Switching to 'heatmap' plot.")
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
          MetBrewer::scale_fill_met_c(name = palette, direction = -1) +

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
            y = if(facet_plot == "LCZ") "LCZ class" else "Station (LCZ)",
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
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
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
        ggplot2::geom_line(lwd = 1) +
        ggplot2::scale_x_datetime(expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(
          name = "Station (LCZ)",
          values = mycolors,
          labels = lcz.lables,
          guide = ggplot2::guide_legend(reverse = FALSE, title.position = "top")
        ) +
        ggplot2::labs(title = title, x = xlab, y = ylab, fill = "LCZ", caption = caption)

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
        base::message("Looking at your files in the path:", base::paste0(getwd(), "/", folder))
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
