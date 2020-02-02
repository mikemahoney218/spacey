#' Get bounding box for set of coordinate points
#'
#' @param lat A vector of latitudes
#' @param lng A vector of longitudes
#'
#' @return A list of length 2, containing the bottom-left (named "bl") and
#' top-right (named "tr") coordinates of the bounding box.
#'
#' @export
get_coord_bounding_box <- function(lat, lng) {

  stopifnot(length(lat) == length(lng))

  if (any(is.na(lat) | is.na(lng))) {
    warning("NAs present in coordinate data and will be ignored.")
  }

  minlat <- min(lat, na.rm = T)
  minlng <- min(lng, na.rm = T)
  maxlat <- max(lat, na.rm = T)
  maxlng <- max(lng, na.rm = T)

  return(list("bl" = c("lat" = minlat, "lng" = minlng),
              "tr" = c("lat" = maxlat, "lng" = maxlng)))
}


#' Get bounding box for set of coordinate points
#'
#' @param centroid A vector of length 2 containing latitude and longitude
#' values.
#' @param distance The distance from the centroid to extend the bounding box.
#' @param lat A quoted string indicating what named value in the centroid
#' represents latitude. If NULL, will be inferred from centroid names.
#' @param lng A quoted string indicating what named value in the centroid
#' representes longitude. If NULL, will be inferred from centroid names.
#' @param dist.unit A single value representing the units the distance value
#' is in.
#' @param coord.unit A single value representing the units the coordinates are
#' in.
#'
#' @return A list of length 2, containing the bottom-left (named "bl") and
#' top-right (named "tr") coordinates of the bounding box.
#'
#' @export
get_centroid_bounding_box <- function(centroid,
                                      distance,
                                      lat = NULL,
                                      lng = NULL,
                                      dist.unit = c("km",
                                               "miles",
                                               "m",
                                               "ft"),
                                      coord.unit = c("degrees",
                                                        "radians")) {
  stopifnot(length(centroid) == 2)
  names(centroid) <- tolower(names(centroid))
  pi <- base::pi
  if (all(!is.null(lat), !is.null(lng))) {
    lat <- centroid[[lat]]
    lng <- centroid[[lng]]
  } else {
    x <- extract_coords(centroid)
    lat <- x[["lat"]]
    lng <- x[["lng"]]
  }

  dist.unit <- dist.unit[[1]]
  if (dist.unit == "miles") {
    distance <- distance * 5280
    dist.unit <- "ft"
  }
  if (dist.unit == "ft") {
    distance <- distance * 0.3048
    dist.unit <- "m"
  }
  if (dist.unit == "m") {
    distance <- distance / 1000
    dist.unit <- "km"
  }
  stopifnot(dist.unit == "km")

  bl.bearing <- deg_to_rad(225)
  tr.bearing <- deg_to_rad(45)
  coord.unit <- coord.unit[[1]]
  if (coord.unit == "degrees") {
    lat <- deg_to_rad(lat)
    lng <- deg_to_rad(lng)
  }

  R <- 6378.1
  calc_lat <- function(lat, bearing) {
    asin(sin(lat) * cos(distance / R) + cos(lat) * sin(distance / R) *  cos(bearing))
  }
  calc_lng <- function(lat, newlat, lng, bearing) {
    lng + atan2(sin(bearing) * sin(distance / R) * cos(lat),
                cos(distance / R) - sin(lat) * sin(newlat))
  }

  bl.lat <- calc_lat(lat, bl.bearing)
  bl.lng <- calc_lng(lat, bl.lat, lng, bl.bearing)

  tr.lat <- calc_lat(lat, tr.bearing)
  tr.lng <- calc_lng(lat, tr.lat, lng, tr.bearing)

  if (coord.unit == "degrees") {
    bl.lat <- rad_to_deg(bl.lat)
    bl.lng <- rad_to_deg(bl.lng)
    tr.lat <- rad_to_deg(tr.lat)
    tr.lng <- rad_to_deg(tr.lng)
  }

  bl <- c(bl.lat,
          bl.lng)
  names(bl) <- c("lat", "lng")

  tr <- c(tr.lat,
          tr.lng)
  names(tr) <- c("lat", "lng")

  return(list(bl,
              tr))

}
