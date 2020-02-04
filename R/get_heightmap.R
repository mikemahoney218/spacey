#' Retrieve height map from USGS national map API
#'
#' This function retrieves elevation data from the USGS national map API and
#' converts it into a matrix appropriate for further usage with tools such as
#' rayshader. It requires a functioning internet connection to retrieve data.
#'
#' @param bbox Bounding box to download imagery for
#' @param major.dim The image size in the major dimension
#' @param lat A quoted string indicating what named value in the bounding box
#' represents latitude. If NULL, will be inferred from bounding box names.
#' @param lng A quoted string indicating what named value in the bounding box
#' representes longitude. If NULL, will be inferred from bounding box names.
#' @param save.tif Logical: should the downloaded imagery be saved as a file?
#' @param filename If \code{save.tif} is \code{TRUE}, the filepath to save the
#' resulting .tif to.
#' @param sr_bbox Spatial reference code (ISO 19111) for bounding box
#' @param sr_image Spatial reference code (ISO 19111) for image
#'
#' @return A matrix object containing elevation data suitable for use with
#' mapping functions.
#'
#' @export
get_heightmap <- function(bbox,
                          major.dim = 600,
                          lat = NULL,
                          lng = NULL,
                          save.tif = FALSE,
                          filename = NULL,
                          sr_bbox = 4326,
                          sr_image = 4326) {
  stopifnot(is.logical(save.tif))
  if (save.tif & is.null(filename)) stop("Must provide filename to save .tif to.")

  if (all(!is.null(lat), !is.null(lng))) {
    first_corner <- c("lat" = bbox[[1]][[lat]], "lng" = bbox[[1]][[lng]])
    second_corner <- c("lat" = bbox[[2]][[lat]], "lng" = bbox[[2]][[lng]])
  } else {
    first_corner <- extract_coords(bbox[[1]])
    second_corner <- extract_coords(bbox[[2]])
  }

  img_size <- get_img_size(bbox, major.dim)

  url <- httr::parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
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
      size = paste(img_size[["width"]], img_size[["height"]], sep = ","),
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )

  if (httr::status_code(res) == 200) {
    body <- httr::content(res, type = "application/json")

    img_res <- httr::GET(body$href)
    img_bin <- httr::content(img_res, "raw")

    if (save.tif) {
      writeBin(img_bin, filename)
    } else {
      filename <- tempfile("download", tempdir(), ".tif")
      writeBin(img_bin, filename)
    }
  } else {
    stop(res)
  }

  raster_read <- raster::raster(filename)
  matrix(
    raster::extract(raster_read, raster::extent(raster_read), buffer = 1000),
    nrow = ncol(raster_read), ncol = nrow(raster_read)
  )
}


#' Load an elevation map from file
#'
#' @param filename The path to the .tif file to import as an elevation map.
#'
#' @return A matrix of elevations for use with further mapping utilities.
#'
#' @export
load_heightmap <- function(filename) {
  raster_read <- raster::raster(filename)
  matrix(
    raster::extract(raster_read, raster::extent(raster_read), buffer = 1000),
    nrow = ncol(raster_read), ncol = nrow(raster_read)
  )
}
