#' Assess the Urban Heat Island Intensity
#'
#' This function calculates the Urban Heat Island (UHI) intensity based on air temperature measurements and Local Climate Zones (LCZ). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
#'
#' @param x A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()} function.
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
#'   \item \strong{End date}: A string specifying the start date in either end="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
#' }
#' @param extract.method A character string specifying the method used to assign the LCZ class to each station point. The default is `"simple"`. Available methods are:
#'   \itemize{
#'     \item **simple**: Assigns the LCZ class based on the value of the raster cell in which the point falls. It often is used in low-density observational network.
#'     \item **two.step**: Assigns LCZs to stations while filtering out those located in heterogeneous LCZ areas. This method requires that at least 80% of the pixels within a 5 × 5 kernel match the LCZ of the center pixel (Daniel et al., 2017). Note that this method reduces the number of stations. It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
#'     \item **bilinear**: Interpolates the LCZ class values from the four nearest raster cells surrounding the point.
#'   }
#' @param method Method to calculate the UHI intensity. Options include: LCZ and manual. The LCZ method automatically identifies the LCZ build types, starting from LCZ 1 and progressing to LCZ 10, to represent the urban temperature, then it starts from LCZ natural LCZ (11-16) to represent the rural temperature. The manual method is a character string indicating the stations as references for the urban and rural areas.
#' @param Turban Urban station references, if the method "manual" is selected.
#' @param Trural Rural station references, if the method "manual" is selected.
#' @param group If TRUE urban, rural and UHI - related temperatures are grouped in the same plot.
#' @param time.freq The time period to average to. Default is \dQuote{hour}, but can also be \dQuote{day}, \dQuote{week}, \dQuote{month}, or \dQuote{year}.
#' @param by  data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
#'            \dQuote{daylight}, \dQuote{dst} (daylight saving time). Note that the **Daylight option may result in both daytime and nighttime hours being represented in UTC**. See NOAA \url{https://gml.noaa.gov/grad/solcalc/} and argument \emph{type} in openair package: \url{https://openair-project.github.io/book/sections/intro/openair-package.html#the-type-option}
#' @param impute Method to impute missing values. Options include \dQuote{mean}, \dQuote{median}, \dQuote{knn}, or \dQuote{bag}.
#' @param iplot Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.
#' @param isave Set to \code{TRUE} to save all results (plot, time-series, station references) into your directory.
#' @param save_extension A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.
#' @param ylab y-axis label. `NULL` (default) uses a translated "Air temperature (°C)".
#' @param ylab2 Right y-axis label. `NULL` (default) uses a translated "UHI intensity (K)".
#' @param xlab x-axis label. `NULL` (default) uses a translated "Time".
#' @param title The title of the plot. Default is an empty string.
#' @param caption Source caption. `NULL` (default) uses a translated "Source: LCZ4r".
#'
#' @param lang Language used for labels, messages and errors. Available codes:
#'   `"en"` (default), `"pt"`, `"es"`, `"fr"`, `"zh"`, `"de"`, `"it"`, `"ja"`,
#'   `"ko"`, `"ru"`, `"ar"`. See [lcz_langs()].
#'
#' @return A visual representation of the time series of UHI in \code{ggplot} or data frame .csv format.
#'
#' @author
#' Max Anjos (\url{https://github.com/ByMaxAnjos})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate hourly UHI intensity for 2019 using the LCZ method
#' my_uhi <- lcz_uhi_intensity(lcz_map, data_frame = lcz_data, var = "airT",
#'                             station_id = "station", year = 2019,
#'                             time.freq = "hour", method = "LCZ")
#'
#'
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso
#' See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
#'
#' @keywords LCZ, Local Climate Zone, urban climate, spatial analysis


lcz_uhi_intensity <- function(x,
                              data_frame = "",
                              var = "",
                              station_id = "",
                              ...,
                              time.freq = "hour",
                              extract.method = "simple",
                              method = "LCZ",
                              Turban = NULL,
                              Trural = NULL,
                              by = NULL,
                              impute = NULL,
                              group = FALSE,
                              iplot = TRUE,
                              isave = FALSE,
                              save_extension = "png",
                              ylab = NULL,
                              xlab = NULL,
                              ylab2 = NULL,
                              title = NULL,
                              caption = NULL,
                              lang = "en") {
  if (is.null(ylab)) ylab <- lcz_tr("air_temp", lang = lang)
  if (is.null(xlab)) xlab <- lcz_tr("time", lang = lang)
  if (is.null(ylab2)) ylab2 <- lcz_tr("uhi", lang = lang)
  if (is.null(caption)) caption <- lcz_caption(lang)
  if (is.null(title)) title <- ""

  # Check and validate raster inputs -----------------------------------------------

  x <- lcz_check_raster(x, lang = lang)

  # Check data inputs -------------------------------------------------------
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

  if (!("latitude" %in% tolower(colnames(df_processed)))) {
    lcz_abort("err_col", lang = lang, arg = "latitude")
  }

  if (!("longitude" %in% tolower(colnames(df_processed)))) {
    lcz_abort("err_col", lang = lang, arg = "longitude")
  }

  # Rename and define my_id for each lat and long
  df_processed <- df_processed %>%
    dplyr::rename(var_interp = {{ var }}, station = {{ station_id }}) %>%
    dplyr::group_by(.data$latitude, .data$longitude) %>%
    dplyr::mutate(
      station = base::as.factor(.data$station),
      my_id = dplyr::cur_group_id(),
      my_id = base::as.factor(.data$my_id),
      date = lubridate::as_datetime(.data$date)
    ) %>%
    dplyr::select(.data$date, .data$var_interp, .data$station, .data$my_id, .data$longitude, .data$latitude) %>%
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
    stations_lcz <- terra::extract(x, terra::vect(stations_mod))
    stations_lcz$ID <- NULL
    df_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz)
      ) %>%
      dplyr::select(.data$my_id, .data$lcz)

    df_model <- dplyr::inner_join(df_processed, df_model, by="my_id")

  }

  if (extract.method == "bilinear") {
    stations_lcz <- terra::extract(x, terra::vect(stations_mod), method= "bilinear")
    stations_lcz$ID <- NULL
    stations_lcz$lcz <- as.integer(stations_lcz$lcz)
    df_model <- base::cbind(stations_mod, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz)
      ) %>%
      dplyr::select(.data$my_id, .data$lcz)

    df_model <- dplyr::inner_join(df_processed, df_model, by="my_id")
  }

  if (extract.method == "two.step") {
    # Step 2: Define a function to filter stations based on LCZ homogeneity
    filter_homogeneous_lcz <- function(df, raster, kernel_size = 5, threshold = 0.8) {
      # Homogeneity is evaluated only at station locations (exact k x k window)
      # instead of running an R callback over every raster cell with terra::focal().
      df$homogeneity <- lcz_homogeneity(raster, df, kernel_size = kernel_size)
      df[!is.na(df$homogeneity) & df$homogeneity >= threshold, , drop = FALSE]
    }

    # Step 3: Apply the function to filter stations
    df_homogeneous <- filter_homogeneous_lcz(stations_mod, x, kernel_size = 5, threshold = 0.8)
    stations_lcz <- terra::extract(x, terra::vect(df_homogeneous))
    stations_lcz$ID <- NULL
    df_model <- base::cbind(df_homogeneous, stations_lcz) %>%
      sf::st_drop_geometry() %>%
      stats::na.omit() %>%
      dplyr::mutate(
        lcz = base::as.factor(.data$lcz)
      ) %>%
      dplyr::select(.data$my_id, .data$lcz)

    df_model <- dplyr::inner_join(df_processed, df_model, by="my_id")
  }

  # One row per monitored location. The label is made unique per LCZ so that
  # repeated station names at different coordinates cannot collide.
  my_stations <- df_model %>%
    dplyr::distinct(.data$my_id, .data$lcz, .data$station) %>%
    dplyr::mutate(
      station = base::as.character(.data$station),
      station_label = base::paste0(.data$station, " (LCZ ", .data$lcz, ")")
    )

  if (nrow(my_stations) == 0L) {
    lcz_abort("err_no_stations", lang = lang)
  }

  # check urban and rural stations ------------------------------------------

  if (method != "LCZ" && method != "manual") {
    lcz_abort("err_method", lang = lang)
  }

  if (method == "manual" && (is.null(Turban) || is.null(Trural))) {
    lcz_abort("err_manual", lang = lang)
  }


  # Select urban and rural stations -----------------------------------------

  if (method == "LCZ") {
    select_stations <- function(data) {
      urban_preference <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
      rural_preference <- c("11", "12", "13", "14", "15", "16")

      find_station <- function(stations, preferences) {
        for (pref in preferences) {
          station <- stations %>% dplyr::filter(lcz == pref)
          if (nrow(station) > 0) {
            return(station)
          }
        }
        return(NULL)
      }

      urban_station <- find_station(data, urban_preference)
      rural_station <- find_station(data, rural_preference)

      base::list(urban = urban_station, rural = rural_station)
    }

    result <- select_stations(my_stations)
    urban_station <- result$urban
    rural_station <- result$rural

    if (is.null(urban_station) || is.null(rural_station)) {
      classes_found <- sort(unique(base::as.character(my_stations$lcz)))
      lcz_warn(
        "warn_uhi_diagnostic", lang = lang,
        n = nrow(my_stations),
        classes = base::paste(classes_found, collapse = ", ")
      )
      lcz_abort("err_no_urban_rural", lang = lang)
    }

    # Select by location (my_id), not by station name: a name may be reused at
    # several coordinates, which previously collapsed urban and rural groups.
    urban_ids <- base::as.character(urban_station$my_id)
    rural_ids <- base::as.character(rural_station$my_id)

    lcz_model <- df_model %>%
      dplyr::filter(.data$my_id %in% c(urban_ids, rural_ids)) %>%
      dplyr::mutate(
        my_id = base::as.character(.data$my_id),
        reference = ifelse(.data$my_id %in% urban_ids, "urban", "rural")
      )
  }

  if (method == "manual") {
    urban_station <- Turban
    rural_station <- Trural

    lcz_model <- df_model %>%
      dplyr::filter(.data$station %in% c(urban_station, rural_station)) %>%
      dplyr::mutate(reference = ifelse(.data$station %in% c(urban_station), "urban", "rural"))

    if (is.null(lcz_model) || nrow(lcz_model) == 0) {
      lcz_abort("err_manual_not_found", lang = lang)
    }
  }

  # Settings for plots ------------------------------------------------------

  lcz_meta <- tibble::tibble(
    lcz = as.character(c(1:10, 11:17)),
    lcz.name = lcz_class_names(lang),
    lcz.col = unname(lcz_palette())
  )

  # Reuse the reference already assigned in `lcz_model` (urban/rural), which
  # also fixes the old vector-recycling bug with multiple manual stations.
  lcz_df <- dplyr::inner_join(
    lcz_meta,
    dplyr::distinct(
      lcz_model,
      .data$station, .data$reference,
      lcz = base::as.character(.data$lcz)
    ),
    by = "lcz"
  )

  color_values <- lcz_df %>%
    dplyr::select(.data$reference, .data$lcz.col) %>%
    dplyr::distinct(.data$reference, .keep_all = TRUE)

  urban_col <- color_values %>%
    dplyr::filter(.data$reference == "urban") %>%
    dplyr::pull(.data$lcz.col)

  rural_col <- color_values %>%
    dplyr::filter(.data$reference == "rural") %>%
    dplyr::pull(.data$lcz.col)

  uhi_col <- c(uhi = "black")

  # Unified typography + terra-only hemisphere detection
  lab_urban <- lcz_tr("urban_temp", lang = lang)
  lab_rural <- lcz_tr("rural_temp", lang = lang)
  lab_uhi <- "UHI"
  lcz_theme <- lcz_theme_series()
  hemisphere <- lcz_hemisphere(x)

  my_latitude <- lcz_model$latitude[1]
  my_longitude <- lcz_model$longitude[1]

  lcz_model$latitude <- NULL
  lcz_model$longitude <- NULL

  # Define time series frequency with without "by"--------------------------------------------
  if (is.null(by)) {
    mydata <- openair::timeAverage(
      lcz_collapse_stations(lcz_model),
      pollutant = "var_interp",
      avg.time = time.freq,
      type = c("reference")
    ) %>%
      dplyr::ungroup() %>%
      stats::na.omit() %>%
      tidyr::pivot_wider(id_cols = .data$date, names_from = .data$reference, values_from = .data$var_interp) %>%
      stats::na.omit() %>%
      dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2))

        final_graph <- lcz_uhi_figure(
      mydata, group = group,
      x = "date", x_type = "datetime",
      facet = NULL, shading = NULL,
      labels = c(urban = lab_urban, rural = lab_rural),
      colors = c(urban = urban_col, rural = rural_col),
      title = title, xlab = xlab, ylab = ylab, ylab2 = ylab2,
      caption = caption, lang = lang
    )

if (isave == TRUE) {
      # Create a folder name using paste0
      folder <- base::paste0("LCZ4r_output/")

      # Check if the folder exists
      if (!base::dir.exists(folder)) {
        # Create the folder if it does not exist
        base::dir.create(folder)
      }

      file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
      lcz_save_figure(
        final_graph, paste0("lcz4r_uhi_plot.", save_extension),
        width = "double", height = 170, lang = lang, quiet = TRUE
      )
      file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
      utils::write.csv(mydata, file.2)
      file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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

  # Define with by  ---------------------------------------------------------

  if (!is.null(by)) {
    if ("day" %in% by) {
      lcz_abort("err_by_day", lang = lang)
    }

    if (length(by) < 2 && any(c("month", "year", "season", "seasonyear", "yearseason") %in% by)) {
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())

      mydata <- openair::timeAverage(
        lcz_collapse_stations(mydata, by = "my_time"),
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit() %>%
        dplyr::mutate(date = base::as.factor(date))

      label_format <- switch(by,
        "season" = "%b %d",
        "seasonyear" = "%b %d",
        "yearseason" = "%b %d",
        "year" = "%b %d",
        "month" = "%d",
        "%b %d" # Default case if none of the above match
      )

            final_graph <- lcz_uhi_figure(
        mydata, group = group,
        x = "date", x_type = "discrete",
        facet = "time", shading = NULL,
        labels = c(urban = lab_urban, rural = lab_rural),
        colors = c(urban = urban_col, rural = rural_col),
        title = title, xlab = xlab, ylab = ylab, ylab2 = ylab2,
        caption = caption, lang = lang
      )

if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        lcz_save_figure(
        final_graph, paste0("lcz4r_uhi_plot.", save_extension),
        width = "double", height = 170, lang = lang, quiet = TRUE
      )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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
        lcz_collapse_stations(mydata, by = "my_time"),
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

      rec_df <- mydata %>%
        dplyr::group_by(.data$my_time) %>%
        dplyr::summarize(
          xmin = base::min(.data$date),
          xmax = base::max(.data$date),
          labs = base::unique(.data$my_time)
        ) %>%
        dplyr::ungroup()

            final_graph <- lcz_uhi_figure(
        mydata, group = group,
        x = "date", x_type = "datetime",
        facet = NULL, shading = list(data = rec_df, xmin = "xmin", xmax = "xmax", fill = "labs"),
        labels = c(urban = lab_urban, rural = lab_rural),
        colors = c(urban = urban_col, rural = rural_col),
        title = title, xlab = xlab, ylab = ylab, ylab2 = ylab2,
        caption = caption, lang = lang
      )

if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        lcz_save_figure(
        final_graph, paste0("lcz4r_uhi_plot.", save_extension),
        width = "double", height = 170, lang = lang, quiet = TRUE
      )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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
        lcz_collapse_stations(mydata, by = by),
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", by)
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, by), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

      new_var <- rlang::syms(by)

      mydata2 <- mydata %>%
        dplyr::mutate(hour = lubridate::hour(.data$date)) %>%
        dplyr::group_by(!!!new_var, .data$hour) %>%
        dplyr::summarize(
          rural = base::mean(.data$rural),
          urban = base::mean(.data$urban),
          uhi = base::mean(.data$uhi), .groups = "drop"
        ) %>%
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

            final_graph <- lcz_uhi_figure(
        mydata, group = group,
        x = "date", x_type = "datetime",
        facet = by_formula, shading = list(data = rec_df, xmin = "xmin", xmax = "xmax", fill = "labs"),
        labels = c(urban = lab_urban, rural = lab_rural),
        colors = c(urban = urban_col, rural = rural_col),
        title = title, xlab = xlab, ylab = ylab, ylab2 = ylab2,
        caption = caption, lang = lang
      )

if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        lcz_save_figure(
        final_graph, paste0("lcz4r_uhi_plot.", save_extension),
        width = "double", height = 170, lang = lang, quiet = TRUE
      )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata2, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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
      mydata <- openair::cutData(lcz_model,
        type = by,
        hemisphere = hemisphere,
        latitude = my_latitude,
        longitude = my_longitude
      ) %>%
        stats::na.omit() %>%
        dplyr::rename(my_time = dplyr::last_col())
      mydata <- openair::timeAverage(
        lcz_collapse_stations(mydata, by = "my_time"),
        pollutant = "var_interp",
        avg.time = time.freq,
        type = c("reference", "my_time")
      ) %>%
        dplyr::ungroup() %>%
        stats::na.omit() %>%
        tidyr::pivot_wider(id_cols = c(.data$date, .data$my_time), names_from = .data$reference, values_from = .data$var_interp) %>%
        dplyr::mutate(uhi = base::round(.data$urban - .data$rural, digits = 2)) %>%
        stats::na.omit()

            final_graph <- lcz_uhi_figure(
        mydata, group = group,
        x = "date", x_type = "datetime",
        facet = "time", shading = NULL,
        labels = c(urban = lab_urban, rural = lab_rural),
        colors = c(urban = urban_col, rural = rural_col),
        title = title, xlab = xlab, ylab = ylab, ylab2 = ylab2,
        caption = caption, lang = lang
      )

if (isave == TRUE) {
        # Create a folder name using paste0
        folder <- base::paste0("LCZ4r_output/")

        # Check if the folder exists
        if (!base::dir.exists(folder)) {
          # Create the folder if it does not exist
          base::dir.create(folder)
        }

        file.1 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_plot.", save_extension)
        lcz_save_figure(
        final_graph, paste0("lcz4r_uhi_plot.", save_extension),
        width = "double", height = 170, lang = lang, quiet = TRUE
      )
        file.2 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_df.csv")
        utils::write.csv(mydata, file.2)
        file.3 <- base::paste0(getwd(), "/", folder, "lcz4r_uhi_stations.csv")
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

# Internal: journal-grade UHI figure shared by all `by` branches --------------
lcz_uhi_figure <- function(data, group = FALSE,
                           x = "date",
                           x_type = c("datetime", "discrete", "continuous"),
                           facet = NULL, shading = NULL,
                           labels, colors,
                           title = "", xlab = NULL, ylab = NULL, ylab2 = NULL,
                           caption = NULL, lang = NULL) {
  x_type <- match.arg(x_type)
  lab_urban <- labels[["urban"]]
  lab_rural <- labels[["rural"]]
  lab_uhi <- "UHI"

  long <- tidyr::pivot_longer(
    data,
    cols = c("urban", "rural", "uhi"),
    names_to = "series", values_to = "value"
  )
  long$series <- factor(
    long$series,
    levels = c("urban", "rural", "uhi"),
    labels = c(lab_urban, lab_rural, lab_uhi)
  )

  x_sym <- rlang::sym(x)
  x_scale <- switch(x_type,
    datetime = ggplot2::scale_x_datetime(
      expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)
    ),
    discrete = ggplot2::scale_x_discrete(
      expand = c(0, 0), guide = ggplot2::guide_axis(angle = 90, check.overlap = TRUE)
    ),
    continuous = ggplot2::scale_x_continuous(
      expand = c(0, 0), guide = ggplot2::guide_axis(check.overlap = TRUE)
    )
  )

  add_shading <- function(p) {
    if (is.null(shading)) return(p)
    p + ggplot2::geom_rect(
      data = shading$data,
      ggplot2::aes(
        xmin = .data[[shading$xmin]], xmax = .data[[shading$xmax]],
        ymin = -Inf, ymax = Inf, fill = .data[[shading$fill]]
      ),
      inherit.aes = FALSE, alpha = 0.18
    ) +
      ggplot2::scale_fill_manual(
        values = c("lightblue", "grey90"), guide = "none", name = NULL
      )
  }

  add_facet <- function(p) {
    if (is.null(facet)) return(p)
    if (identical(facet, "time")) {
      return(p + ggplot2::facet_wrap(
        ~my_time, scales = "free_x",
        ncol = min(4L, length(unique(data$my_time)))
      ))
    }
    p + ggplot2::facet_wrap(facet, scales = "free_x")
  }

  build <- function(dat, colour = FALSE) {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = !!x_sym, y = .data$value)) +
      ggplot2::geom_line(linewidth = 0.55, colour = "grey10") +
      ggplot2::geom_hline(
        yintercept = 0, linewidth = 0.25, linetype = "22", colour = "grey45"
      ) +
      x_scale
    if (isTRUE(colour)) {
      p <- ggplot2::ggplot(
        dat, ggplot2::aes(x = !!x_sym, y = .data$value, colour = .data$series)
      ) +
        ggplot2::geom_line(linewidth = 0.55) +
        ggplot2::scale_colour_manual(
          values = stats::setNames(
            c(colors[["urban"]], colors[["rural"]]), c(lab_urban, lab_rural)
          ),
          name = NULL
        ) +
        x_scale
    }
    add_facet(add_shading(p))
  }

  if (!isTRUE(group)) {
    return(
      build(dplyr::filter(long, .data$series == lab_uhi)) +
        ggplot2::labs(title = title, x = xlab, y = ylab2, caption = caption) +
        lcz_theme_journal(grid = "y", legend = "none")
    )
  }

  temp <- build(dplyr::filter(long, .data$series != lab_uhi), colour = TRUE) +
    ggplot2::labs(title = title, x = NULL, y = ylab, caption = NULL) +
    lcz_theme_journal(grid = "y", legend = "top") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(6, 10, 0, 6)
    )
  uhi <- build(dplyr::filter(long, .data$series == lab_uhi)) +
    ggplot2::labs(x = xlab, y = ylab2, caption = caption) +
    lcz_theme_journal(grid = "y", legend = "none") +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 6, 6))

  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(
      patchwork::wrap_plots(temp, uhi, ncol = 1, heights = c(2, 1)) +
        patchwork::plot_annotation(tag_levels = "a") &
        ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 9))
    )
  }

  # Fallback without patchwork: one faceted panel with independent y axes
  ggplot2::ggplot(
    long, ggplot2::aes(x = !!x_sym, y = .data$value, colour = .data$series)
  ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(
        c(colors[["urban"]], colors[["rural"]], "grey10"),
        c(lab_urban, lab_rural, lab_uhi)
      ),
      name = NULL
    ) +
    x_scale +
    ggplot2::facet_wrap(~series, scales = "free_y", ncol = 1) +
    ggplot2::labs(title = title, x = xlab, y = NULL, caption = caption) +
    lcz_theme_journal(grid = "y", legend = "none")
}
