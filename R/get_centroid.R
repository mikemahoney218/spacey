get_centroid <- function(data, lat, lng, coord.unit = c("degrees", "radians")) {
  coord.unit <- coord.unit[[1]]
  pi <- base::pi
  stopifnot(length(lat) == length(lng))
  if (length(lat) == 1) return(c(lat, lng))
  if (coord.unit == "degrees") {
    lat <- lat * pi / 180
    lng <- lng * pi / 180
  }

  x <- sum(cos(lat) * cos(lng)) / length(lat)
  y <- sum(cos(lat) * sin(lng)) / length(lat)
  z <- sum(sin(lat)) / length(lat)

  lng <- atan2(y, x)
  lat <- atan2(z, (x * x + y * y))

  if (coord.unit == "degrees") {
    lat <- lat * 180 / pi
    lng <- lng * 180 / pi
  }

  return(c("lat" = lat, "lng" = lng))
}
