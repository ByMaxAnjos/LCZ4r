% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcz_anomaly.R
\name{lcz_anomaly}
\alias{lcz_anomaly}
\title{Explore LCZ Thermal Anomalies}
\usage{
lcz_anomaly(
  x,
  data_frame = "",
  var = "",
  station_id = "",
  ...,
  time.freq = "hour",
  extract.method = "simple",
  plot_type = "diverging_bar",
  by = NULL,
  impute = NULL,
  iplot = TRUE,
  isave = FALSE,
  save_extension = "png",
  inclusive = FALSE,
  palette = "OKeeffe1",
  ylab = "Air temperature anomaly",
  xlab = "Stations",
  title = "",
  caption = "",
  legend_name = "Anomaly"
)
}
\arguments{
\item{x}{A \code{SpatRaster} object containing the LCZ map. The LCZ map can be obtained using the \code{lcz_get_map()} function.}

\item{data_frame}{A data frame containing air temperature measurements and station IDs. The data frame should have a date field in hourly or higher resolution format.}

\item{var}{The name of the variable in the data frame representing air temperature.}

\item{station_id}{The name of the variable in the data frame representing station IDs.}

\item{...}{Additional arguments for the \code{selectByDate} function from the \code{openair} package. These arguments allow for flexible selection of specific time periods (year, month, day, hour). Examples of how to use these arguments include:
\itemize{
\item \strong{Year(s)}: Numeric value(s) specifying the year(s) to select. For example, \code{year = 1998:2004} selects all years between 1998 and 2004 (inclusive), while \code{year = c(1998, 2004)} selects only the years 1998 and 2004.
\item \strong{Month(s)}: Numeric or character value(s) specifying the months to select. Numeric examples: \code{month = 1:6} (January to June), or character examples: \code{month = c("January", "December")}.
\item \strong{Day(s)}: Numeric value(s) specifying the days to select. For instance, \code{day = 1:30} selects days from 1 to 30, or \code{day = 15} selects only the 15th day of the month.
\item \strong{Hour(s)}: Numeric value(s) specifying the hours to select. For example, \code{hour = 0:23} selects all hours in a day, while \code{hour = 9} selects only the 9th hour.
\item \strong{Start date}: A string specifying the start date in either start="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
\item \strong{End date}: A string specifying the end date in either end="DD/MM/YYYY" (e.g., "1/2/1999") or "YYYY-mm-dd" format (e.g., "1999-02-01").
}}

\item{time.freq}{Defines the time period to average to. Default is \dQuote{hour}, but includes \dQuote{day}, \dQuote{week}, \dQuote{month} or \dQuote{year}.}

\item{extract.method}{A character string specifying the method used to assign the LCZ class to each station point. The default is \code{"simple"}. Available methods are:
\itemize{
\item \strong{simple}: Assigns the LCZ class based on the value of the raster cell in which the point falls. It often is used in low-density observational network.
\item \strong{two.step}: Assigns LCZs to stations while filtering out those located in heterogeneous LCZ areas. This method requires that at least 80\% of the pixels within a 5 × 5 kernel match the LCZ of the center pixel (Daniel et al., 2017). Note that this method reduces the number of stations. It often is used in ultra and high-density observational network, especially in LCZ classes with multiple stations.
\item \strong{bilinear}: Interpolates the LCZ class values from the four nearest raster cells surrounding the point.
}}

\item{plot_type}{A character string indicating the type of plot to generate. Options include:
\itemize{
\item \strong{diverging_bar}: A horizontal bar plot that diverges from the center (zero), with positive anomalies extending to the right and negative anomalies to the left. This plot is good for showing the extent and direction of anomalies in a compact format.
\item \strong{bar}:  A bar plot showing the magnitude of the anomaly for each station, colored by whether the anomaly is positive or negative. This plot is good for comparing anomalies across stations.
\item \strong{dot}: A dot plot that displays both the mean temperature values and the reference values, with lines connecting them. The size or color of the dots can indicate the magnitude of the anomaly. Ideal for showing both absolute temperature values and their anomalies.
\item \strong{lollipop}: A lollipop plot where each "stick" represents an anomaly value and the dots at the top represent the size of the anomaly. Useful for clearly showing positive and negative anomalies in a minimalist way.
}}

\item{by}{data frame time-serie split: \dQuote{year}, \dQuote{season}, \dQuote{seasonyear},  \dQuote{month}, \dQuote{monthyear}, \dQuote{weekday}, \dQuote{weekend},  \dQuote{site},
\dQuote{daylight}, \dQuote{dst} (daylight saving time).  Note that the \strong{Daylight option may result in both daytime and nighttime hours being represented in UTC}.See NOAA \url{https://gml.noaa.gov/grad/solcalc/} and argument \emph{type} in openair package: \url{https://openair-project.github.io/book/sections/intro/openair-package.html#the-type-option}}

\item{impute}{Method to impute missing values (\dQuote{mean}, \dQuote{median}, \dQuote{knn}, \dQuote{bag}).}

\item{iplot}{Set to \code{TRUE} to return a plot. If \code{FALSE}, a data frame is returned.}

\item{isave}{Set to \code{TRUE} to save all results (plot, time-series) into your directory.}

\item{save_extension}{A character string indicating the file format for saving the plot. Options include: \dQuote{png}, \dQuote{jpg}, \dQuote{jpeg}, \dQuote{tif}, \dQuote{pdf}, \dQuote{svg}. The default is \dQuote{png}.}

\item{inclusive}{Set to TRUE to a colorblind-friendly palette.}

\item{palette}{A character string specifying the color palette to use. The default is \dQuote{OKeeffe1}. You can choose from palettes available in \code{MetBrewer}: \url{https://github.com/BlakeRMills/MetBrewer?tab=readme-ov-file#palettes}.}

\item{ylab}{y-axis name.}

\item{xlab}{y-axis name. Default is \dQuote{Station}}

\item{title}{y-axis name. Default is \dQuote{" "}.}

\item{caption}{source data. Default can be \dQuote{Source: LCZ4r, Stewart and Oke, 2012; Demuzere et al.2022."}.}

\item{legend_name}{Legend name for dot plot. Default is "Anomaly".}
}
\value{
A visual representation of the anomalies of air temperature of LCZ in \code{ggplot} or data frame .csv format.
}
\description{
This function generates a graphical representation of thermal anomaly for different Local Climate Zones (LCZs). More details: \url{https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html}
}
\examples{
\dontrun{
# Hourly air temperature values in 2019.
my_res <- lcz_anomaly(lcz_map, data_frame = lcz_data, var = "airT",
                       station_id = "station", year = 2020)
}
}
\seealso{
See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
}
\author{
Max Anjos (\url{https://github.com/ByMaxAnjos})
}
\keyword{Climate}
\keyword{LCZ,}
\keyword{Local}
\keyword{Zone,}
\keyword{analysis}
\keyword{climate,}
\keyword{spatial}
\keyword{urban}
