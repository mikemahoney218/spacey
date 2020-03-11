#' Retrieve height map from USGS national map API
#'
#' This function retrieves elevation data from the USGS national map API and
#' converts it into a matrix appropriate for further usage with tools such as
#' rayshader. It requires a functioning internet connection to retrieve data.
#'
#' @param bbox Bounding box to download imagery for
#' @param img.width Image width, in pixels
#' @param img.height Image size, in pixels
#' @param lat A quoted string indicating what named value in the bounding box
#' represents latitude. If NULL, will be inferred from bounding box names.
#' @param lng A quoted string indicating what named value in the bounding box
#' represents longitude. If NULL, will be inferred from bounding box names.
#' @param save.tif Logical: should the downloaded imagery be saved as a file?
#' @param tif.filename If \code{save.tif} is \code{TRUE}, the filepath to save the
#' resulting .tif to.
#' @param sr_bbox Spatial reference code (ISO 19111) for bounding box
#' @param sr_image Spatial reference code (ISO 19111) for image
#' @param verbose Logical: print out debug information while trying to query the
#' elevation map server?
#'
#' @return A matrix object containing elevation data suitable for use with
#' mapping functions. Returned invisibly.
#'
#' @examples
#' \dontrun{
#' bbox <- get_centroid_bounding_box(c(
#'   "lat" = 44.121268,
#'   "lng" = -73.903734
#' ),
#' distance = 10
#' )
#'
#' heightmap <- get_heightmap(bbox)
#' }
#'
#' @export
get_heightmap <- function(bbox,
                          img.width = 600,
                          img.height = 600,
                          lat = NULL,
                          lng = NULL,
                          save.tif = FALSE,
                          tif.filename = NULL,
                          sr_bbox = 4326,
                          sr_image = 4326,
                          verbose = FALSE) {
  stopifnot(is.logical(save.tif))
  if (img.width > 8000 || img.height > 8000) {
    stop(paste(
      "USGS data sets a maximum side length of 8000 pixels -- try",
      "again with smaller img.width or img.height arguments."
    ))
  }
  if (save.tif & is.null(tif.filename)) {
    stop("Must provide tif.filename to save .tif to.")
  }

  if (all(!is.null(lat), !is.null(lng))) {
    first_corner <- c("lat" = bbox[[1]][[lat]], "lng" = bbox[[1]][[lng]])
    second_corner <- c("lat" = bbox[[2]][[lat]], "lng" = bbox[[2]][[lng]])
  } else {
    first_corner <- extract_coords(bbox[[1]])
    second_corner <- extract_coords(bbox[[2]])
  }

  url <- httr::parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")

  counter <- 0

  get_tif <- function() {
    res <- httr::GET(
      url,
      query = list(
        bbox = paste(min(first_corner[["lng"]], second_corner[["lng"]]),
          min(first_corner[["lat"]], second_corner[["lat"]]),
          max(second_corner[["lng"]], first_corner[["lng"]]),
          max(second_corner[["lat"]], first_corner[["lat"]]),
          sep = ","
        ),
        bboxSR = sr_bbox,
        imageSR = sr_image,
        size = paste(img.width, img.height, sep = ","),
        format = "tiff",
        pixelType = "F32",
        noDataInterpretation = "esriNoDataMatchAny",
        interpolation = "+RSP_BilinearInterpolation",
        f = "json"
      )
    )

    if (httr::status_code(res) != 200) stop(paste("Query returned error code",
                                            httr::status_code(res)))

    body <- httr::content(res, type = "application/json")
    img_res <- httr::GET(body$href)
  }

  img_res <- get_tif()
  counter <- 1
  while (httr::status_code(img_res) != 200 && counter < 15) {
    img_res <- get_tif()
    if (verbose) print(paste0("Attempt #", counter, ": status code ",
                              httr::status_code(img_res)))
    counter <- counter + 1
  }

  if (httr::status_code(img_res) != 200) {
    stop(paste("Map server returned error code", httr::status_code(img_res)))
  }

  img_bin <- httr::content(img_res, "raw")

  if (save.tif) {
    writeBin(img_bin, tif.filename)
  } else {
    tif.filename <- tempfile("download", tempdir(), ".tiff")
    writeBin(img_bin, tif.filename)
  }

  raster_read <- raster::raster(tif.filename)
  invisible(
    matrix(
      raster::extract(raster_read, raster::extent(raster_read), buffer = 1000),
      nrow = ncol(raster_read), ncol = nrow(raster_read)
    )
  )
}


#' Load an elevation map from file
#'
#' @param filename The path to the .tif file to import as an elevation map.
#'
#' @return A matrix of elevations for use with further mapping utilities.
#'
#' @examples
#' \dontrun{
#' bbox <- get_centroid_bounding_box(c(
#'   "lat" = 44.121268,
#'   "lng" = -73.903734
#' ),
#' distance = 10
#' )
#'
#' heightmap_file <- tempfile("heightmap_file", fileext = ".tif")
#' get_heightmap(bbox, save.tif = TRUE, filename = heightmap_file)
#' heightmap <- load_heightmap(heightmap_file)
#' }
#'
#' @export
load_heightmap <- function(filename) {
  raster_read <- raster::raster(filename)
  matrix(
    raster::extract(raster_read, raster::extent(raster_read), buffer = 1000),
    nrow = ncol(raster_read), ncol = nrow(raster_read)
  )
}
