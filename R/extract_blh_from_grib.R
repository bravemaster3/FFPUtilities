#' Extract Boundary Layer Height from ERA5 GRIB File
#'
#' Extracts boundary layer height (BLH) data from ERA5 GRIB files for a specific
#' location and converts hourly ERA5 data to half-hourly resolution to match
#' typical eddy covariance data frequency.
#'
#' @param grib_file Character string. Path to the ERA5 GRIB file containing
#'   boundary layer height data.
#' @param site_long Numeric. Site longitude in decimal degrees (WGS84).
#' @param site_lat Numeric. Site latitude in decimal degrees (WGS84).
#' @param filter_start_date Date or character. Optional. Filter output to extract
#'   only data from dates >= this value. Format: "YYYY-MM-DD". Default is NULL
#'   (extract all available dates).
#' @param filter_end_date Date or character. Optional. Filter output to extract
#'   only data from dates <= this value. Format: "YYYY-MM-DD". Default is NULL
#'   (extract all available dates).
#' @param to_half_hourly Logical. If TRUE (default), converts hourly ERA5 data
#'   to half-hourly by duplicating values. This matches typical EC data frequency.
#'
#' @return A data frame with two columns:
#'   \itemize{
#'     \item datetime: POSIXct timestamp (in UTC)
#'     \item blh: Boundary layer height in meters
#'   }
#'
#' @details
#' ERA5 reanalysis provides boundary layer height at hourly resolution. This
#' function extracts BLH for the nearest grid point to the specified coordinates
#' and optionally converts to half-hourly resolution (the typical frequency of
#' eddy covariance measurements) by duplicating each hourly value.
#'
#' The function reads timestamps directly from the GRIB file metadata and
#' optionally filters to a specific date range.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract all BLH data from GRIB
#' blh_data <- extract_blh_from_grib(
#'   grib_file = "path/to/era5_blh.grib",
#'   site_long = 19.56924,
#'   site_lat = 64.159996
#' )
#'
#' # Extract only May 2021
#' blh_data <- extract_blh_from_grib(
#'   grib_file = "path/to/era5_blh.grib",
#'   site_long = 19.56924,
#'   site_lat = 64.159996,
#'   filter_start_date = "2021-05-01",
#'   filter_end_date = "2021-05-31"
#' )
#' }
extract_blh_from_grib <- function(grib_file,
                                  site_long,
                                  site_lat,
                                  filter_start_date = NULL,
                                  filter_end_date = NULL,
                                  to_half_hourly = TRUE) {

  # Check if file exists
  if(!file.exists(grib_file)) {
    stop("GRIB file not found: ", grib_file)
  }

  message("Reading GRIB file: ", grib_file)

  # Read GRIB file using terra, suppress warnings about incomplete messages
  blh_rast <- suppressWarnings(terra::rast(grib_file))

  n_layers <- terra::nlyr(blh_rast)
  message("GRIB file contains ", n_layers, " time layers")

  # Extract timestamps from GRIB metadata
  grib_times <- terra::time(blh_rast)

  if(is.null(grib_times) || length(grib_times) == 0) {
    stop("Could not extract time information from GRIB file. ",
         "The GRIB file may not contain proper time metadata.")
  }

  # Convert to POSIXct if not already
  if(!inherits(grib_times, "POSIXct")) {
    grib_times <- as.POSIXct(grib_times, tz = "UTC")
  }

  message("Time range in GRIB: ", min(grib_times), " to ", max(grib_times))

  # Create a point for the site location
  site_point <- terra::vect(
    matrix(c(site_long, site_lat), ncol = 2),
    type = "points",
    crs = "EPSG:4326"
  )

  # Extract BLH values at site location for all time layers
  blh_extracted <- terra::extract(blh_rast, site_point, method = "bilinear")

  # Remove the ID column and convert to vector
  blh_values <- as.numeric(blh_extracted[1, -1])

  # Create data frame
  blh_df <- data.frame(
    datetime = grib_times,
    blh = blh_values,
    stringsAsFactors = FALSE
  )

  # Remove any NA values
  blh_df <- blh_df[complete.cases(blh_df), ]

  message("Extracted ", nrow(blh_df), " hourly BLH observations")

  # Filter by date range if specified
  if(!is.null(filter_start_date)) {
    if(is.character(filter_start_date)) {
      filter_start_date <- as.POSIXct(paste(filter_start_date, "00:00:00"), tz = "UTC")
    }
    n_before <- nrow(blh_df)
    blh_df <- blh_df[blh_df$datetime >= filter_start_date, ]
    message("Filtered to dates >= ", filter_start_date,
            " (removed ", n_before - nrow(blh_df), " observations)")
  }

  if(!is.null(filter_end_date)) {
    if(is.character(filter_end_date)) {
      filter_end_date <- as.POSIXct(paste(filter_end_date, "23:59:59"), tz = "UTC")
    }
    n_before <- nrow(blh_df)
    blh_df <- blh_df[blh_df$datetime <= filter_end_date, ]
    message("Filtered to dates <= ", filter_end_date,
            " (removed ", n_before - nrow(blh_df), " observations)")
  }

  # Convert to half-hourly if requested
  if(to_half_hourly) {
    message("Converting hourly data to half-hourly resolution")

    # Duplicate each row
    blh_df_half <- blh_df[rep(seq_len(nrow(blh_df)), each = 2), ]

    # Add 0 or 30 minutes to datetime
    min_add <- rep(c(0, 30), nrow(blh_df_half)/2)
    blh_df_half$datetime <- blh_df_half$datetime + (min_add * 60)

    blh_final <- blh_df_half

  } else {
    blh_final <- blh_df
  }

  message("Final output: ", nrow(blh_final), " BLH observations")

  return(blh_final)
}
