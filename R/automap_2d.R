#' Automatically create 2D maps using USGS and ESRI map data
#'
#' This function takes a latitude/longitude coordinate pair alongside a desired
#' distance to map and retrieves USGS (and optionally ESRI) map data, converts
#' said data into matrices, and runs the data through \code{rayshader} using
#' sensible defaults in order to return a 2D shaded map relief. It requires a
#' functioning internet connection in order to retrieve data.
#'
#' @param lat The latitude for the map's centroid, in decimal degrees or
#' radians.
#' @param lng The longitude for the map's centroid, in decimal degrees or
#' radians.
#' @param distance The distance between the centroid and any corner of the
#' (square) output map to include.
#' @param major.dim Image size in the major direction -- as this function
#' produces square maps, this is equivalent to pixels along any edge.
#' @param overlay ESRI overlay map to include, if any -- see
#' \code{\link[spacey]{get_image_overlay}} for list of options.
#' @param z zscale, passed to various rayshader functions. Defined as
#' the ratio between the x and y spacing (which are assumed to be equal)
#' and the z axis.
#' @param overlay.alpha Alpha value for the optional overlay layer.
#' @param colorscale Color scale for land and water elements. If a vector of
#' length 1, the same color scale will be applied for both land and water. If
#' greater than  length 1, values named "water" or "watercolor" will be used
#' for water coloring, while "land" or "landcolor" will be used for land.
#' @param max.darken Passed to \code{\link[rayshader]{add_shadow}}.
#' @param save.tif Logical -- should the height map be saved?
#' @param save.png Logical -- should the overlay image be saved?
#' @param tif.filename Filename for the heightmap if \code{save.tif} is TRUE.
#' @param png.filename Filename for the overlay if \code{save.png} is TRUE.
#' @param dist.unit Units for the distance argument. All units are converted to
#' kilometers, with some errors in the transition due to floating point
#' arithmetic -- so if high accuracy is needed, convert units to kilometers
#' beforehand.
#' @param coord.unit Units for latitude and longitude, in either decimal degrees
#' or radians.
#' @param sr_bbox Spatial reference code (ISO 19111) for bounding box
#' @param sr_image Spatial reference code (ISO 19111) for image
#' @param print.map Logical -- should the output map be printed?
#'
#' @return A rayshader map object, by default printed and returned invisibly.
#'
#' @importFrom magrittr `%>%`
#'
#' @export

automap_2d <- function(lat,
                       lng,
                       distance = 10,
                       major.dim = 600,
                       overlay = NULL,
                       z = 30,
                       overlay.alpha = 0.75,
                       colorscale = "imhof4",
                       max.darken = 0.5,
                       save.tif = FALSE,
                       save.png = FALSE,
                       tif.filename = NULL,
                       png.filename = NULL,
                       dist.unit = c("km",
                                     "miles",
                                     "m",
                                     "ft"),
                       coord.unit = c("degrees",
                                      "radians"),
                       sr_bbox = 4326,
                       sr_image = 4326,
                       print.map = TRUE) {

  stopifnot(is.logical(print.map))

  if (length(colorscale) == 1) {
    watercolor <- colorscale
    landcolor <- colorscale
  } else {
    if (all(names(colorscale) %in% c("water",
                                     "land",
                                     "watercolor",
                                     "landcolor"))) {
      if (length(names(colorscale) %in% c("water", "watercolor")) != 1) {
        stop("Multiple matches for water colorscale.")
      }
      if (length(names(colorscale) %in% c("land", "landcolor")) != 1) {
        stop("Multiple matches for land colorscale.")
      }
      watercolor <- colorscale[names(colorscale) %in% c("water", "watercolor")]
      landcolor <- colorscale[names(colorscale) %in% c("land", "landcolor")]
    }
  }

  bound_box <- get_centroid_bounding_box(c("lat" = lat,
                                           "lng" = lng),
                                         distance = distance,
                                         dist.unit = dist.unit,
                                         coord.unit = coord.unit)
  heightmap <- get_heightmap(bound_box,
                             major.dim = major.dim,
                             save.tif = save.tif,
                             filename = tif.filename,
                             sr_bbox = sr_bbox,
                             sr_image = sr_image)

  out <- heightmap %>%
    rayshader::sphere_shade(texture = landcolor) %>%
    rayshader::add_water(rayshader::detect_water(heightmap), color = watercolor) %>%
    rayshader::add_shadow(rayshader::ray_shade(heightmap, zscale = z, lambert = TRUE), max_darken = max.darken) %>%
    rayshader::add_shadow(rayshader::ambient_shade(heightmap, zscale = z), max_darken = max.darken)

  if (!is.null(overlay)) {
    overlay_img <- get_image_overlay(bound_box,
                                     overlay = overlay,
                                     major.dim = major.dim,
                                     lat = NULL,
                                     lng = NULL,
                                     save.png = save.png,
                                     png.filename = png.filename,
                                     sr_bbox = sr_bbox,
                                     sr_image = sr_image)
    out <- rayshader::add_overlay(out, overlay_img, alphalayer = overlay.alpha)
  }

  if (print.map) rayshader::plot_map(out)
  invisible(out)

}
