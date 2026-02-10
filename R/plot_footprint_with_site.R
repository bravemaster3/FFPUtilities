#' Plot Footprint Contours with Site Location
#'
#' Creates a ggplot2 visualization of footprint contours with the tower/site
#' location marked. Optionally includes a background raster layer.
#'
#' @param footprint_sf sf object or list of sf objects with footprint contours.
#'   Can be output from FFP_to_shapefiles() or a combined sf object.
#' @param site_coords Data frame with 'long' and 'lat' columns for tower location
#'   in WGS84 (EPSG:4326), or an sf object with point geometry.
#' @param background_raster Optional. A raster object (terra SpatRaster or raster
#'   package) to display as background. Default is NULL.
#' @param raster_layer_name Character string. Name for the raster legend.
#'   Default is "Value".
#' @param raster_colors Character vector of length 2 specifying low and high
#'   colors for raster gradient. Default is c("#00A600", "#F2F2F2").
#' @param contour_fill Color for footprint contours. Default is NA (no fill).
#' @param contour_color Color for contour borders. Default is "black".
#' @param contour_linewidth Line width for contour borders. Default is 0.5.
#' @param site_color Color for site point. Default is "red".
#' @param site_size Size of site point. Default is 3.
#' @param site_shape Shape of site point (see ?pch). Default is 21 (circle).
#' @param title Plot title. Default is "Footprint Contours".
#' @param xlabel X-axis label. Default depends on CRS.
#' @param ylabel Y-axis label. Default depends on CRS.
#' @param plot_crs_epsg Integer. EPSG code for plotting CRS. If NULL, uses CRS
#'   from footprint_sf. Default is NULL.
#' @param show_contour_labels Logical. If TRUE and contours have 'contour_pct'
#'   attribute, labels contours with percentage. Default is FALSE.
#'
#' @return A ggplot2 object that can be further customized or saved.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires footprint shapefiles from FFP_to_shapefiles
#' # See vignette("footprint-workflow") for complete example
#'
#' data(site_coords_example)
#'
#' plot_footprint_with_site(
#'   footprint_sf = footprint_contours,
#'   site_coords = site_coords_example,
#'   title = "Footprint Climatology",
#'   show_contour_labels = TRUE
#' )
#' }
plot_footprint_with_site <- function(footprint_sf,
                                     site_coords,
                                     background_raster = NULL,
                                     raster_layer_name = "Value",
                                     raster_colors = c("#00A600", "#F2F2F2"),
                                     contour_fill = NA,
                                     contour_color = "black",
                                     contour_linewidth = 0.5,
                                     site_color = "red",
                                     site_size = 3,
                                     site_shape = 21,
                                     title = "Footprint Contours",
                                     xlabel = NULL,
                                     ylabel = NULL,
                                     plot_crs_epsg = NULL,
                                     show_contour_labels = FALSE) {

  # Handle footprint_sf if it's a list
  if(is.list(footprint_sf) && !inherits(footprint_sf, "sf")) {
    footprint_sf <- do.call(rbind, footprint_sf)
    rownames(footprint_sf) <- NULL
  }

  # Convert site_coords to sf if it's a data frame
  if(is.data.frame(site_coords) && !inherits(site_coords, "sf")) {
    if(!all(c("long", "lat") %in% names(site_coords))) {
      stop("site_coords must have 'long' and 'lat' columns")
    }
    site_sf <- sf::st_as_sf(site_coords, coords = c("long", "lat"), crs = 4326)
  } else if(inherits(site_coords, "sf")) {
    site_sf <- site_coords
  } else {
    stop("site_coords must be a data frame with long/lat or an sf object")
  }

  # Determine plotting CRS
  if(!is.null(plot_crs_epsg)) {
    target_crs <- plot_crs_epsg
    footprint_sf <- sf::st_transform(footprint_sf, crs = target_crs)
    site_sf <- sf::st_transform(site_sf, crs = target_crs)
  } else {
    target_crs <- sf::st_crs(footprint_sf)$epsg
  }

  # Set default axis labels based on CRS
  if(is.null(xlabel)) {
    xlabel <- ifelse(!is.na(target_crs) && target_crs == 4326,
                     "Longitude", "X (meters)")
  }
  if(is.null(ylabel)) {
    ylabel <- ifelse(!is.na(target_crs) && target_crs == 4326,
                     "Latitude", "Y (meters)")
  }

  # Start building the plot
  p <- ggplot2::ggplot()

  # Add background raster if provided
  if(!is.null(background_raster)) {
    # Handle both terra and raster package objects
    if(inherits(background_raster, "SpatRaster")) {
      # terra package
      raster_df <- as.data.frame(background_raster, xy = TRUE)
      raster_col <- names(raster_df)[3]  # First non-xy column
    } else if(inherits(background_raster, c("RasterLayer", "RasterBrick", "RasterStack"))) {
      # raster package
      raster_df <- as.data.frame(raster::rasterToPoints(background_raster))
      raster_col <- names(raster_df)[3]
    } else {
      warning("background_raster type not recognized. Skipping raster layer.")
      raster_df <- NULL
    }

    if(!is.null(raster_df)) {
      p <- p +
        ggplot2::geom_raster(data = raster_df,
                             ggplot2::aes(x = x, y = y, fill = .data[[raster_col]])) +
        ggplot2::scale_fill_gradient(name = raster_layer_name,
                                     low = raster_colors[1],
                                     high = raster_colors[2])
    }
  }

  # Add footprint contours
  p <- p +
    ggplot2::geom_sf(data = footprint_sf,
                     fill = contour_fill,
                     color = contour_color,
                     linewidth = contour_linewidth)

  # Add contour labels if requested
  if(show_contour_labels && "contour_pct" %in% names(footprint_sf)) {
    centroids <- sf::st_centroid(footprint_sf)
    p <- p +
      ggplot2::geom_sf_text(data = centroids,
                            ggplot2::aes(label = paste0(contour_pct, "%")),
                            size = 3)
  }

  # Add site location
  p <- p +
    ggplot2::geom_sf(data = site_sf,
                     color = site_color,
                     size = site_size,
                     shape = site_shape,
                     fill = site_color)

  # Add labels and theme
  p <- p +
    ggplot2::labs(title = title, x = xlabel, y = ylabel) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(crs = target_crs) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p)
}
