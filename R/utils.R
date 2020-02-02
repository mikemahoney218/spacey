#' Convert degrees to radians
#'
#' @param deg Value in degrees to be converted
#'
#' @keywords internal
#'
#' @return Value in radians
#'
#' @export
deg_to_rad <- function(deg) {
  deg * pi / 180
}

#' Convert radians to degrees
#'
#' @param rad Value in radians to be converted
#'
#' @keywords internal
#'
#' @return Value in degrees
#'
#' @export
rad_to_deg <- function(rad) {
  rad * 180 / pi
}

#' Extract latitude and longitude from a provided object
#'
#' @param coords An object to try and extract coordinates from
#'
#' @keywords internal
#'
#' @return A vector of length 2 containing object latitude and longitude
#'
#' @export
extract_coords <- function(coords) {
  if (all(names(coords %in% c(
    "lat",
    "lng",
    "long",
    "latitude",
    "longitude"
  )))) {
    if (sum(names(coords) %in% c("lat", "latitude")) != 1) {
      stop("Couldn't infer latitude variable -- try passing a value to lat.")
    } else if (sum(names(coords) %in% c("lng", "long", "longitude")) != 1) {
      stop("Couldn't infer longitude variable -- try passing a value to lng.")
    } else {
      output <- c(
        coords[names(coords) %in% c("lat", "latitude")],
        coords[names(coords) %in% c("lng", "long", "longitude")]
      )
      names(output) <- c("lat", "lng")
      return(output)
    }
  } else {
    stop("Couldn't determine lat/lng values.")
  }
}

#' Calculate image height and width from a bounding box and edge length
#'
#' @param bbox The bounding box to calculate image size for
#' @param lat A quoted string indicating what named value in the bounding box
#' represents latitude. If NULL, will be inferred from bounding box names.
#' @param lng A quoted string indicating what named value in the bounding box
#' representes longitude. If NULL, will be inferred from bounding box names.
#' @param major.dim The length of the (rectangular) image's largest side.
#'
#' @return A length-2 vector of image height and width
#'
#' @keywords internal
#'
#' @export
get_img_size <- function(bbox, lat = NULL, lng = NULL, major.dim = 600) {
  stopifnot(length(bbox) == 2)

  if (all(!is.null(lat), !is.null(lng))) {
    first_corner <- c(bbox[[1]][[lat]], bbox[[1]][[lng]])
    second_corner <- c(bbox[[2]][[lat]], bbox[[2]][[lng]])
  } else {
    first_corner <- extract_coords(bbox[[1]])
    second_corner <- extract_coords(bbox[[2]])
  }

  aspect_ratio <- abs((first_corner[[2]] - second_corner[[2]]) / (first_corner[[1]] - second_corner[[1]]))
  img_width <- round(ifelse(aspect_ratio > 1, major.dim, major.dim * aspect_ratio))
  img_height <- round(ifelse(aspect_ratio < 1, major.dim, major.dim / aspect_ratio))
  c(
    "width" = img_width,
    "height" = img_height
  )
}
