#' Air temperature LCZ data
#'
#' A subset of data from the Urban Climate Observatory (UCO) Berlin, Germany.
#' It contains hourly air temperature readings from 23 meteorological stations for the 2019-2020 period.
#'
#' @format A data frame with 386,354 rows and 5 columns:
#' \describe{
#'   \item{date}{date POSIXct, format: "2019-01-01 00:00:00"}
#'   \item{station}{station name}
#'   \item{Latitude}{Latitude station}
#'   \item{Longitue}{Longitude station}
#'   ...
#' }
#' @source <https://uco.berlin/>
#' @examples
#' data(lcz_data)
"lcz_data"
