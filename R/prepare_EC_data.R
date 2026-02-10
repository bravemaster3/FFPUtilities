#' Prepare Eddy Covariance Data for Footprint Calculation
#'
#' Merges eddy covariance data with boundary layer height data, handles missing
#' values, filters by date range, and ensures required variables are present for
#' footprint calculations.
#'
#' @param ec_data Data frame containing eddy covariance measurements with columns
#'   for datetime, wind_speed, v_var, u*, wind_dir, and L (Obukhov length).
#' @param blh_data Data frame containing boundary layer height from ERA5 with
#'   columns for datetime and blh.
#' @param datetime_col Character string specifying the name of the datetime column.
#'   Default is "datetime".
#' @param required_vars Character vector of required variable names. Default is
#'   c("wind_speed", "v_var", "u*", "wind_dir", "blh", "L").
#' @param na_value Numeric value to convert to NA. Default is -9999.
#' @param filter_start_date Date or character. Optional. Filter data to dates >= this value.
#'   Format: "YYYY-MM-DD". Default is NULL (no filtering).
#' @param filter_end_date Date or character. Optional. Filter data to dates <= this value.
#'   Format: "YYYY-MM-DD". Default is NULL (no filtering).
#'
#' @return A data frame with merged EC and BLH data, with missing values properly
#'   handled, filtered by date range if specified, and only complete cases retained.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(ec_data_example)
#' data(blh_data_example)
#'
#' # Prepare data for footprint calculation
#' ec_clean <- prepare_EC_data(
#'   ec_data = ec_data_example,
#'   blh_data = blh_data_example,
#'   datetime_col = "datetime"
#' )
#'
#' # Prepare data with date filtering
#' ec_clean <- prepare_EC_data(
#'   ec_data = ec_data_example,
#'   blh_data = blh_data_example,
#'   filter_start_date = "2020-05-01",
#'   filter_end_date = "2020-05-31"
#' )
#' }
prepare_EC_data <- function(ec_data,
                            blh_data,
                            datetime_col = "datetime",
                            required_vars = c("wind_speed", "v_var", "u*", "wind_dir", "blh", "L"),
                            na_value = -9999,
                            filter_start_date = NULL,
                            filter_end_date = NULL) {

  # Convert na_value to NA
  ec_data[ec_data == na_value] <- NA

  # Merge with BLH data
  merged_data <- merge(ec_data, blh_data, by = datetime_col, all = TRUE)

  # Filter by date range if specified
  if(!is.null(filter_start_date) || !is.null(filter_end_date)) {

    # Convert datetime to date for filtering
    date_col <- as.Date(merged_data[[datetime_col]])

    # Convert filter dates to Date objects
    if(!is.null(filter_start_date)) {
      filter_start_date <- as.Date(filter_start_date)
    }
    if(!is.null(filter_end_date)) {
      filter_end_date <- as.Date(filter_end_date)
    }

    # Build filter condition
    n_before <- nrow(merged_data)
    keep <- rep(TRUE, nrow(merged_data))

    if(!is.null(filter_start_date)) {
      keep <- keep & (date_col >= filter_start_date)
    }
    if(!is.null(filter_end_date)) {
      keep <- keep & (date_col <= filter_end_date)
    }

    merged_data <- merged_data[keep & !is.na(keep), ]

    message("Filtered to date range: ",
            ifelse(!is.null(filter_start_date), as.character(filter_start_date), "all"),
            " to ",
            ifelse(!is.null(filter_end_date), as.character(filter_end_date), "all"),
            " (removed ", n_before - nrow(merged_data), " observations)")
  }

  # Check if required variables exist
  missing_vars <- setdiff(required_vars, names(merged_data))
  if(length(missing_vars) > 0) {
    warning("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Select required columns plus datetime
  cols_to_keep <- c(datetime_col, required_vars[required_vars %in% names(merged_data)])
  subset_data <- merged_data[, cols_to_keep, drop = FALSE]

  # Remove incomplete cases
  clean_data <- subset_data[complete.cases(subset_data), ]

  message("Data prepared: ", nrow(clean_data), " complete observations out of ",
          nrow(merged_data), " total observations")

  return(clean_data)
}
