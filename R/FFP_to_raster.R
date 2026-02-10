#' Converts Footprint values to a raster
#'
#' @param FFP output of the FFP model, the footprint climatology model
#' @param site_long site longitude
#' @param site_lat site latitude
#' @param project_raster boolean, default is FALSE. Set to TRUE to project the raster into a different CRS. Make sure to set the output EPSG correctly
#' @param project_crs_epsg EPSG code of the output projection of the FFP raster
#'
#' @return a raster in CRS: EPSG 4326, with the footprint values
#' @export
#'
FFP_to_raster <- function(FFP,
                          site_long,
                          site_lat,
                          project_raster=FALSE,
                          project_crs_epsg=3006) {
  long_lat <- xr_yr_2_lat_long(
    site_long = site_long,
    site_lat = site_lat,
    xr = list(FFP$x_2d[1,]),
    yr = list(FFP$y_2d[,1])
  )

  xy <- expand.grid(x = long_lat$long, y = long_lat$lat)
  xy$z <- as.vector(FFP$fclim_2d) # Ensure that Z matches the grid layout

  # Create a raster from the xyz data
  r <- raster::rasterFromXYZ(xy, crs = "+proj=longlat +datum=WGS84")

  if(isFALSE(project_raster)){
    return(r)
  } else {
    proj_crs <- paste0("+init=epsg:", project_crs_epsg)
    return(raster::projectRaster(r, crs = proj_crs))
  }
}
