#' Example Site Coordinates for Halmyran
#'
#' Location of the Halmyran eddy covariance flux tower in Sweden.
#'
#' @format A data frame with 1 row and 2 variables:
#' \describe{
#'   \item{long}{Longitude in decimal degrees (WGS84)}
#'   \item{lat}{Latitude in decimal degrees (WGS84)}
#' }
#' @source Halmyran peatland research site, Sweden
#' @examples
#' data(site_coords_example)
#' site_coords_example
"site_coords_example"

#' Example Eddy Covariance Data from Halmyran
#'
#' Half-hourly eddy covariance measurements from the Halmyran peatland site
#' in northern Sweden, May 2021. Contains wind and turbulence parameters
#' needed for footprint calculations.
#'
#' @format A data frame with ~50 rows and 6 variables:
#' \describe{
#'   \item{datetime}{Timestamp of measurement (POSIXct)}
#'   \item{wind_speed}{Horizontal wind speed (m/s)}
#'   \item{v_var}{Variance of lateral wind velocity component (m²/s²)}
#'   \item{u*}{Friction velocity (m/s)}
#'   \item{wind_dir}{Wind direction (degrees from North)}
#'   \item{L}{Obukhov length (m)}
#' }
#' @source Halmyran peatland research site, Sweden
#' @examples
#' data(ec_data_example)
#' head(ec_data_example)
"ec_data_example"

#' Example Boundary Layer Height Data from ERA5
#'
#' Boundary layer height extracted from ERA5 reanalysis for the Halmyran
#' site location, matched to eddy covariance measurement timestamps.
#'
#' @format A data frame with ~50 rows and 2 variables:
#' \describe{
#'   \item{datetime}{Timestamp (POSIXct)}
#'   \item{blh}{Boundary layer height (m)}
#' }
#' @source ERA5 reanalysis via Copernicus Climate Data Store
#' @examples
#' data(blh_data_example)
#' head(blh_data_example)
"blh_data_example"
