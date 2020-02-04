#' Get an ESRI image overlay for a rayshader map
#'
#' This function calls the ESRI ArcGIS API to retrieve map imagery to overlay
#' a rayshader map object. It requires a functioning internet connection to
#' obtain the data.
#'
#' @param bbox The bounding box for your image.
#' @param overlay One of the available overlays from
#' \url{https://services.arcgisonline.com/ArcGIS/rest/services}.
#' @param major.dim Image size in the major (largest) direction.
#' @param lat A quoted string indicating what named value in the bounding box
#' represents latitude. If NULL, will be inferred from bounding box names.
#' @param lng A quoted string indicating what named value in the bounding box
#' representes longitude. If NULL, will be inferred from bounding box names.
#' @param save.png Logical -- should the overlay image be saved?
#' @param png.filename Filename for the overlay if \code{save.png} is TRUE.
#' @param sr_bbox Spatial reference code (ISO 19111) for bounding box.
#' @param sr_image Spatial reference code (ISO 19111) for image.
#'
#' @return A matrix object provided by \code{\link[png]{readPNG}}, suitable for
#' use with \code{rayshader} and similar mapping utilities.
#'
#' @export
get_image_overlay <- function(bbox,
                              overlay = c(
                                "World_Imagery",
                                "NatGeo_World_Map",
                                "USA_Topo_Maps",
                                "World_Physical_Map",
                                "World_Shaded_Relief",
                                "World_Street_Map",
                                "World_Terrain_Base",
                                "World_Topo_Map"
                              ),
                              major.dim = 600,
                              lat = NULL,
                              lng = NULL,
                              save.png = FALSE,
                              png.filename = NULL,
                              sr_bbox = 4326,
                              sr_image = 4326) {
  stopifnot(is.logical(save.png))

  if (all(!is.null(lat), !is.null(lng))) {
    first_corner <- c(bbox[[1]][[lat]], bbox[[1]][[lng]])
    second_corner <- c(bbox[[2]][[lat]], bbox[[2]][[lng]])
  } else {
    first_corner <- extract_coords(bbox[[1]])
    second_corner <- extract_coords(bbox[[2]])
  }

  overlay <- overlay[[1]]

  img_size <- get_img_size(bbox, lat, lng, major.dim)

  url <- httr::parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")

  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(paste0(
          "https://services.arcgisonline.com/ArcGIS/rest/services/",
          overlay,
          "/MapServer"
        )))
      )
    ),
    exportOptions = list(
      outputSize = c(
        img_size[["width"]],
        img_size[["height"]]
      )
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(second_corner[[2]], first_corner[[2]])),
        xmin = jsonlite::unbox(min(first_corner[[2]], second_corner[[2]])),
        ymax = jsonlite::unbox(max(second_corner[[1]], first_corner[[1]])),
        ymin = jsonlite::unbox(min(first_corner[[1]], second_corner[[1]]))
      )
    )
  )

  res <- httr::GET(
    url,
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param)
    )
  )

  if (httr::status_code(res) == 200) {
    body <- httr::content(res, type = "application/json")
    img_res <- httr::GET(body$results[[1]]$value$url)
    img_bin <- httr::content(img_res, "raw")
  } else {
    stop(res)
  }

  if (save.png) {
    writeBin(img_bin, png.filename)
  } else {
    png.filename <- tempfile("download", tempdir(), ".png")
    writeBin(img_bin, png.filename)
  }

  png::readPNG(png.filename)
}

#' Import PNG textures for overlays
#'
#' This is an extremely thin wrapper for \code{\link[png]{readPNG}}, in order
#' for users to not realize they even needed \code{png} for this package's
#' functionality at all.
#'
#' @param filename The path to the PNG file to be imported
#'
#' @return A matrix of values provided by \code{\link[png]{readPNG}}
#'
#' @export
load_overlay <- function(filename) {
  png::readPNG(filename)
}
