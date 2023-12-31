% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcz_plot_map.R
\name{lcz_plot_map}
\alias{lcz_plot_map}
\title{Visualize the LCZ Map}
\usage{
lcz_plot_map(x, isave = FALSE, inclusive = FALSE, legend = "name", ...)
}
\arguments{
\item{x}{A SpatRaster object containing the LCZ map to be plotted.}

\item{isave}{Logical. Set to TRUE if you want to save the plot in your working directory.}

\item{inclusive}{Set to TRUE to a colorblind-friendly palette.}

\item{legend}{Specify the type of legend to include in the plot. Use "code" to display LCZ codes in the legend.}

\item{...}{An optional modify axis, legend, and plot labels: title, subtitle, and caption.}
}
\value{
A visual representation of the LCZ map in ggplot format
}
\description{
This function generates a graphical representation of an LCZ (Local Climate Zone) map provided as a SpatRaster object.
}
\examples{

# Example: Plot an LCZ map with the legend showing LCZ codes and specify a subtitle.
# myplot <- lcz_plot_map(myLCZmap)

}
\seealso{
See the documentation for lcz_get_map() to obtain an LCZ map.
}
\keyword{Climate}
\keyword{LCZ,}
\keyword{Local}
\keyword{Zone,}
\keyword{analysis}
\keyword{climate,}
\keyword{spatial}
\keyword{urban}
