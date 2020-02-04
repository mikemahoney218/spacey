#' Find central point for list of lat/long coordinates
#'
#' @param lat A quoted string indicating what named value in the bounding box
#' represents latitude. If NULL, will be inferred from bounding box names.
#' @param lng A quoted string indicating what named value in the bounding box
#' representes longitude. If NULL, will be inferred from bounding box names.
#' @param coord.unit The unit latitude and longitude are stored in.
#'
#' @export
get_centroid <- function(lat, lng, coord.unit = c("degrees", "radians")) {
  coord.unit <- coord.unit[[1]]
  stopifnot(length(lat) == length(lng))
  if (length(lat) == 1) {
    return(c("lat" = lat, "lng" = lng))
  }
  if (coord.unit == "degrees") {
    lat <- deg_to_rad(lat)
    lng <- deg_to_rad(lng)
  }

  x <- sum(cos(lat) * cos(lng)) / length(lat)
  y <- sum(cos(lat) * sin(lng)) / length(lat)
  z <- sum(sin(lat)) / length(lat)

  lng <- atan2(y, x)
  lat <- atan2(z, sqrt(x * x + y * y))

  if (coord.unit == "degrees") {
    lat <- rad_to_deg(lat)
    lng <- rad_to_deg(lng)
  }

  return(c("lat" = lat, "lng" = lng))
}
