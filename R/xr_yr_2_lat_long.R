#' Convertion of xr and yr from the FFP model to long and lat
#'
#' @param site_lat site latitude
#' @param site_long site longitude
#' @param xr x-array _A list_ for contour line of r, r is the footprint percentage
#' @param yr y-array _A list_ for contour line of r
#' @param R the radius of the idealized sphere representing the Earth, in meter. Default value according to IUGG is 6371007.1810 m
#'
#' @return a named list of latitudes and longitudes
#' @export
xr_yr_2_lat_long <- function(site_lat, site_long, xr, yr, R=6371007.181) {
  dn=yr # the 90% contour line, change as needed, eg. FFP_single(8).yr and FFP_single(8).xr for 80% contour line
  de=xr
  # Coordinate offsets in radians
  dLat = dn[[1]]/R
  dLon = de[[1]]/(R*cos(pi*site_lat/180));

  # OffsetPosition, decimal degrees
  latO = site_lat + dLat*180/pi
  lonO = site_long + dLon*180/pi

  return(list(lat=latO, long=lonO))
}
