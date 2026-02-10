#' Convert FFP Climatology Output to Multiple Shapefile Contours
#'
#' Creates spatial polygon shapefiles for each footprint percentage contour
#' from the output of calc_footprint_FFP_climatology. Optionally saves as
#' individual files or as a single combined shapefile.
#'
#' @param FFP Output list from calc_footprint_FFP_climatology containing
#'   xr and yr arrays for each contour percentage.
#' @param site_long Numeric. Site longitude in decimal degrees (WGS84).
#' @param site_lat Numeric. Site latitude in decimal degrees (WGS84).
#' @param radius_vector Numeric vector of footprint percentages that were used
#'   in the FFP calculation (e.g., c(50, 60, 70, 80)).
#' @param project_shapefile Logical. If TRUE, project shapefiles to CRS specified
#'   in project_crs_epsg. Default is FALSE.
#' @param project_crs_epsg Integer. EPSG code for projection. Default is 3006
#'   (SWEREF99 TM).
#' @param save_option Character string. Options: "none" (default, no saving),
#'   "individual" (save each contour separately), "combined" (save as single file),
#'   or "both" (save individual AND combined).
#' @param output_dir Character string. Directory path for saving files.
#'   Only used if save_option is not "none". Default is NULL.
#' @param combined_filename Character string. Filename for combined shapefile
#'   (without extension). Only used if save_option is "combined" or "both".
#'   Default is "footprint_combined".
#' @param reverse_order Logical. If TRUE, reverse the order of output list so
#'   largest contour is first. Default is TRUE.
#'
#' @return A list of sf objects, one for each contour percentage. Each sf object
#'   has a 'contour_pct' attribute. If reverse_order = TRUE, list is ordered
#'   from largest to smallest contour.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires FFP output from calc_footprint_FFP_climatology
#' # See vignette("footprint-workflow") for complete example
#'
#' footprints <- FFP_to_shapefiles(
#'   FFP = FFP,
#'   site_long = 19.56924,
#'   site_lat = 64.159996,
#'   radius_vector = c(50, 60, 70, 80, 90),
#'   save_option = "combined",
#'   output_dir = "output/footprints"
#' )
#' }
FFP_to_shapefiles <- function(FFP,
                              site_long,
                              site_lat,
                              radius_vector,
                              project_shapefile = FALSE,
                              project_crs_epsg = 3006,
                              save_option = "none",
                              output_dir = NULL,
                              combined_filename = "footprint_combined",
                              reverse_order = TRUE) {

  # Validate save_option
  valid_options <- c("none", "individual", "combined", "both")
  if(!save_option %in% valid_options) {
    stop("save_option must be one of: ", paste(valid_options, collapse = ", "))
  }

  # Check if output_dir exists when saving is requested
  if(save_option != "none") {
    if(is.null(output_dir)) {
      stop("output_dir must be specified when save_option is not 'none'")
    }
    if(!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      message("Created output directory: ", output_dir)
    }
  }

  all_footprints_list <- list()

  for (i in seq_along(radius_vector)) {

    # Convert xr, yr to lat/long
    long_lat <- xr_yr_2_lat_long(
      site_long = site_long,
      site_lat = site_lat,
      xr = FFP$xr[i],
      yr = FFP$yr[i]
    )

    # Create data frame
    df <- data.frame(
      xr = FFP$xr[i][[1]],
      yr = FFP$yr[i][[1]],
      long = long_lat$long,
      lat = long_lat$lat
    )

    # Remove NA values
    df <- df[complete.cases(df), ]

    # Check if we have enough points for a polygon (at least 3)
    if(nrow(df) < 3) {
      warning("Contour ", radius_vector[i], "% has insufficient valid points (",
              nrow(df), "). Skipping.")
      next
    }

    # Create shapefile (don't save yet, we'll handle saving later)
    all_footprints_list[[i]] <- long_lat_to_shapefile(
      df,
      project_shapefile = project_shapefile,
      project_crs_epsg = project_crs_epsg,
      saving_file_path = NULL
    )

    # Add contour percentage attribute
    all_footprints_list[[i]]$contour_pct <- radius_vector[i]
  }

  # Remove NULL elements (skipped contours)
  all_footprints_list <- all_footprints_list[!sapply(all_footprints_list, is.null)]

  if(length(all_footprints_list) == 0) {
    stop("No valid footprint contours created. Check FFP output for valid xr/yr coordinates.")
  }

  # Name the list elements
  actual_radii <- sapply(all_footprints_list, function(x) x$contour_pct[1])
  names(all_footprints_list) <- paste0(actual_radii, "pct")

  # Reverse order if requested
  if(reverse_order) {
    all_footprints_list <- rev(all_footprints_list)
    radius_vector_ordered <- rev(actual_radii)
  } else {
    radius_vector_ordered <- actual_radii
  }

  # Handle saving based on save_option
  if(save_option %in% c("individual", "both")) {
    for(i in seq_along(all_footprints_list)) {
      file_path <- file.path(output_dir,
                             paste0("footprint_", radius_vector_ordered[i], "pct.shp"))
      sf::st_write(all_footprints_list[[i]], file_path, append = FALSE, quiet = TRUE)
    }
    message("Saved ", length(all_footprints_list), " individual footprint shapefiles to: ", output_dir)
  }

  if(save_option %in% c("combined", "both")) {
    combined <- do.call(rbind, all_footprints_list)
    rownames(combined) <- NULL
    combined_path <- file.path(output_dir, paste0(combined_filename, ".shp"))
    sf::st_write(combined, combined_path, append = FALSE, quiet = TRUE)
    message("Saved combined footprint shapefile to: ", combined_path)
  }

  message("Created ", length(all_footprints_list), " footprint contours")

  return(all_footprints_list)
}

