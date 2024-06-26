% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcz_get_map2.R
\name{lcz_get_map2}
\alias{lcz_get_map2}
\title{Obtain the LCZ map from LCZ generator or with downloaded global LCZ map}
\usage{
lcz_get_map2(x, city = NULL, roi = NULL, isave_map = FALSE)
}
\arguments{
\item{x}{A SpatRaster object containing the LCZ map. It can be download using these links: https://lcz-generator.rub.de/ , https://zenodo.org/records/8419340/files/lcz_filter_v3.tif?download=1".
Then, it suggested use terra::rast(path/name.tif") to import the raster .tif into R.}

\item{city}{A character string specifying the name of your target area based on the OpenStreetMap project.}

\item{roi}{Optionally, you can provide a Region of Interest (ROI) in ESRI shapefile format to clip the LCZ map to a custom area.}

\item{isave_map}{Logical. Set to TRUE if you wish to save the resulting clipped map as a raster TIFF file on your local machine.}
}
\value{
A Terra raster TIFF file containing LCZ classes for the specified area of interest.
}
\description{
This function retrieves the Local Climate Zone (LCZ) global mapping dataset
created by Demuzere et al. (2022) and available at https://doi.org/10.5194/essd-14-3835-2022.
It allows you to obtain the LCZ map for a specific area of interest, which can be a city, state, region, or custom-defined shape.
}
\examples{
\dontrun{
# Go to https://lcz-generator.rub.de/
my_map <- terra::rast("path/name.tif")
my_lcz_map <- lcz_get_map2(my_map, city = "Name")

# Load the LCZ map from your PC
my_map <- terra::rast("path/lcz_filter_v3.tif")

# Get LCZ map for a custom region of interest
custom_roi <- sf::st_read("custom_roi.shp")
roi_lcz <- lcz_get_map2(my_map, roi = custom_roi)

# Retrieve the LCZ map for a country (no custom ROI specified)
my_lcz_country <- lcz_get_map2(my_map, city = "Lisbon")
}

}
\keyword{Climate}
\keyword{LCZ,}
\keyword{Local}
\keyword{Zone,}
\keyword{analysis}
\keyword{climate,}
\keyword{spatial}
\keyword{urban}
