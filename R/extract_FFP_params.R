#' Extract Parameters from EC Data for FFP Calculation
#'
#' Extracts the necessary parameters from a prepared eddy covariance dataset
#' for specific time points to use with calc_footprint_FFP_climatology. This
#' function is designed to work with the FFP climatology model, which accepts
#' vectors of parameters to calculate aggregate footprints.
#'
#' @param ec_data Prepared EC data frame from prepare_EC_data() or similar.
#' @param row_index Integer vector specifying which row(s) to extract. Can be a
#'   single value, a vector of indices, or a range (e.g., 100:200).
#' @param zm Numeric. Measurement height (m) above ground, corrected for
#'   displacement height.
#' @param z0 Numeric. Roughness length (m). Use NaN for automatic calculation
#'   by FFP model. Default is NaN.
#' @param datetime_col Character string specifying the name of the datetime column.
#'   Default is "datetime".
#' @param wind_speed_col Character string. Column name for wind speed. Default is "wind_speed".
#' @param v_var_col Character string. Column name for lateral wind velocity variance.
#'   Default is "v_var".
#' @param ustar_col Character string. Column name for friction velocity.
#'   Default is "u*". Use "ustar" if your data uses that convention.
#' @param wind_dir_col Character string. Column name for wind direction.
#'   Default is "wind_dir".
#' @param blh_col Character string. Column name for boundary layer height.
#'   Default is "blh".
#' @param L_col Character string. Column name for Obukhov length. Default is "L".
#'
#' @return A named list containing vectors of parameters:
#'   \itemize{
#'     \item zm: measurement height (scalar)
#'     \item z0: roughness length (scalar)
#'     \item umean: wind speed (vector)
#'     \item h: boundary layer height (vector)
#'     \item ol: Obukhov length (vector)
#'     \item sigmav: standard deviation of lateral velocity (vector)
#'     \item ustar: friction velocity (vector)
#'     \item wind_dir: wind direction (vector)
#'     \item datetime: datetime of measurements (vector, if available)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard column names
#' params <- extract_FFP_params(
#'   ec_data = ec_clean,
#'   row_index = 100:200,
#'   zm = 2.6
#' )
#'
#' # Custom column names
#' params <- extract_FFP_params(
#'   ec_data = ec_clean,
#'   row_index = 100:200,
#'   zm = 2.6,
#'   ustar_col = "ustar",
#'   wind_dir_col = "wind_dir_corr"
#' )
#' }
extract_FFP_params <- function(ec_data,
                               row_index,
                               zm,
                               z0 = NaN,
                               datetime_col = "datetime",
                               wind_speed_col = "wind_speed",
                               v_var_col = "v_var",
                               ustar_col = "u*",
                               wind_dir_col = "wind_dir",
                               blh_col = "blh",
                               L_col = "L") {

  # Subset to selected row(s)
  selected_data <- ec_data[row_index, ]

  # Check if required columns exist
  required_cols <- c(wind_speed_col, v_var_col, ustar_col, wind_dir_col, blh_col, L_col)
  missing_cols <- setdiff(required_cols, names(selected_data))

  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         "\nAvailable columns: ", paste(names(selected_data), collapse = ", "))
  }

  # Extract datetime if available
  if(datetime_col %in% names(selected_data)) {
    datetime_val <- selected_data[[datetime_col]]
    message("Extracting footprint parameters for ", nrow(selected_data),
            " observations")
    message("  Date range: ", min(datetime_val), " to ", max(datetime_val))
  } else {
    datetime_val <- NULL
    message("Extracting footprint parameters for ", nrow(selected_data),
            " observations")
  }

  # Build parameter list with VECTORS for FFP climatology
  params <- list(
    zm = zm,
    z0 = z0,
    umean = selected_data[[wind_speed_col]],
    h = selected_data[[blh_col]],
    ol = selected_data[[L_col]],
    sigmav = sqrt(selected_data[[v_var_col]]),
    ustar = selected_data[[ustar_col]],
    wind_dir = selected_data[[wind_dir_col]],
    datetime = datetime_val
  )

  return(params)
}
