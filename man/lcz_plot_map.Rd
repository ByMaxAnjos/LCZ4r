% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcz_plot_map.R
\name{lcz_plot_map}
\alias{lcz_plot_map}
\title{Visualize the LCZ Map}
\usage{
lcz_plot_map(
  x,
  isave = FALSE,
  show_legend = TRUE,
  save_extension = "png",
  inclusive = FALSE,
  ...
)
}
\arguments{
\item{x}{A SpatRaster object containing the LCZ map to be plotted.}

\item{isave}{Logical. Set to TRUE to save the plot in your working directory.}

\item{show_legend}{Logical. If TRUE, displays the legend on the plot. If FALSE, the legend will be hidden. Default is TRUE.}

\item{save_extension}{File format for saving the plot. Options: "png", "jpg", "jpeg", "tif", "pdf", "svg" (default is "png").}

\item{inclusive}{Logical. Set to TRUE to use a colorblind-friendly palette.}

\item{...}{Optional arguments to modify axis labels, legend, plot title, subtitle, and caption.}
}
\value{
A visual representation of the LCZ map in ggplot format.
}
\description{
This function generates a graphical representation of a Local Climate Zone (LCZ) map provided as a \code{SpatRaster} object.
}
\examples{
\dontrun{
lcz_plot_map(LCZmap)
}

}
\seealso{
See the documentation for \code{lcz_get_map()} to obtain an LCZ map.
}
\keyword{Climate}
\keyword{LCZ,}
\keyword{Local}
\keyword{Zone,}
\keyword{analysis}
\keyword{climate,}
\keyword{spatial}
\keyword{urban}
