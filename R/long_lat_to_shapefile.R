#' Convert Longitude/Latitude Coordinates to Shapefile
#'
#' Converts a data frame containing longitude and latitude coordinates into a
#' spatial polygon shapefile. The function creates a polygon from the coordinate
#' pairs and optionally projects it to a specified coordinate reference system.
#'
#' @param df A data frame containing 'long' and 'lat' columns with longitude
#'   and latitude coordinates in decimal degrees (WGS84, EPSG:4326). Coordinates
#'   should be ordered to form a closed polygon.
#' @param project_shapefile Logical. If TRUE, the output shapefile will be
#'   projected to the CRS specified in `project_crs_epsg`. If FALSE (default),
#'   the shapefile remains in WGS84 (EPSG:4326).
#' @param project_crs_epsg Integer. EPSG code of the target coordinate reference
#'   system for projection. Default is 3006 (SWEREF99 TM, Swedish national grid).
#'
#' @param saving_file_path Character string or NULL. If provided, saves the
#'   shapefile to the specified file path. Should include the filename with
#'   .shp extension. If NULL (default), no file is saved to disk.
#'
#' @return An sf object (simple features polygon). If `project_shapefile = FALSE`,
#'   returns the polygon in WGS84 (EPSG:4326). If `project_shapefile = TRUE`,
#'   returns the polygon projected to the specified CRS.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Create a simple polygon
#' coords <- data.frame(
#'   long = c(19.5, 19.6, 19.6, 19.5, 19.5),
#'   lat = c(64.1, 64.1, 64.2, 64.2, 64.1)
#' )
#'
#' # Convert to shapefile
#' polygon_sf <- long_lat_to_shapefile(
#'   df = coords,
#'   project_shapefile = TRUE,
#'   project_crs_epsg = 3006
#' )
#' }
long_lat_to_shapefile <- function(df,
                                  project_shapefile = FALSE,
                                  project_crs_epsg = 3006,
                                  saving_file_path = NULL){
  sp::coordinates(df) <- ~long+lat
  poly <- sp::Polygon(df)
  polys <- sp::Polygons(list(poly), "s1")
  sp_poly <- sp::SpatialPolygons(list(polys))

  # Use modern EPSG format instead of deprecated +init=epsg:
  sp::proj4string(sp_poly) <- sp::CRS("EPSG:4326")
  sf_poly <- sf::st_as_sf(sp_poly)

  if(isFALSE(project_shapefile)){
    result <- sf_poly
  } else {
    result <- sf::st_transform(sf_poly, paste0("EPSG:", project_crs_epsg))
  }

  # Save to disk if path is provided
  if(!is.null(saving_file_path)){
    sf::st_write(result, saving_file_path, append = FALSE, quiet = TRUE)
  }

  return(result)
}
