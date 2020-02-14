#' Automatically create 2D and 3D maps using USGS and ESRI map data
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
#' @param method Should a 2d or 3d plot be produced?
#' @param img.width Image width, in pixels
#' @param img.height Image height, in pixels
#' @param overlay ESRI overlay map to include, if any -- see
#' \code{\link[spacey]{get_image_overlay}} for list of options. If specifying
#' a local file with \code{from.file}, any non \code{NULL} value will add your
#' local file as an overlay.
#' @param z zscale, passed to various rayshader functions. Defined as
#' the ratio between the x and y spacing (which are assumed to be equal)
#' and the z axis. For contiguous United States, USGS data is generally
#' available at a ~9 meter spacing, with elevation provided in meter increments,
#' resulting in a z value of 9 returning roughly representative maps; decrease
#' this value to exaggerate elevation features.
#' @param overlay.alpha Alpha value for the optional overlay layer.
#' @param colorscale Color scale for land and water elements. If a vector of
#' length 1, the same color scale will be applied for both land and water. If
#' greater than  length 1, values named "water" or "watercolor" will be used
#' for water coloring, while "land" or "landcolor" will be used for land.
#' @param color.intensity Intensity of color mapping -- higher values result in
#' more intense colors.
#' @param max.darken Passed to \code{\link[rayshader]{add_shadow}}. The lower
#' limit for how much the image will be darkened. 0 is completely black, 1 means
#' the shadow map will have no effect.
#' @param sun.angle Angle around the matrix from which lights originate. Values
#' line up with compass directions -- so 0 is directly North, while the default
#' 315 places the sun in the Northwest.
#' @param sun.altitude Angle in degrees from horizon from which light
#' originates. Bounded [0, 90].
#' @param water.cutoff Passed to \code{\link[rayshader]{detect_water}}. Defined
#' therein as the lower limit of the z-component of the unit normal vector to
#' be classified as water.
#' @param water.min.area Passed to \code{\link[rayshader]{detect_water}}. The
#' minimum possible area to consider a body of water.
#' @param water.max.height Passed to \code{\link[rayshader]{detect_water}}. If
#' provided, the maximum height a point can be classified water.
#' @param solid Logical -- should the output be rendered as a solid
#' (\code{TRUE}) or just a surface (\code{FALSE})?
#' @param shadow Logical -- should shadows be rendered?
#' @param water Logical -- should water be rendered?
#' @param waterdepth Water level.
#' @param theta Rotation around z axis.
#' @param phi Azimuth amgle.
#' @param fov Field of view angle.
#' @param zoom Zoom factor.
#' @param save.file Should the heightmap (\code{= "tif"}), overlay
#' (\code{= "png"}), or both (\code{= TRUE}) be saved? Default \code{FALSE}
#' saves neither.
#' @param from.file Should the map be built from local \code{.tif}
#' and \code{.png} files, rather than downloaded data? Accepts logical
#' \code{FALSE} (no local files used) and \code{TRUE} (local files used for both
#' height maps and textures, if requested), as well as strings \code{tif}
#' (only use height map, redownload PNG) and \code{png} (only use texture,
#' redownload tif). Overrides save arguments if local files are used.
#' @param tif.filename If \code{save.tif} is \code{TRUE}, filename to save the
#' \code{.tif} height map to. If \code{from.file} is \code{TRUE}, filename to
#' load the height map from.
#' @param png.filename If \code{save.png} is \code{TRUE}, filename to save the
#' \code{.png} texture to. If \code{from.file} is \code{TRUE}, filename to
#' load the texture from.
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
#' @references
#' Archuleta, C.M., Constance, E.W., Arundel, S.T., Lowe, A.J., Mantey, K.S.,
#' and Phillips, L.A., 2017, The National Map seamless digital elevation model
#' specifications: U.S. Geological Survey Techniques and Methods, book 11,
#' chap. B9, 39 p., https://doi.org/10.3133/tm11B9
#'
#' @export

automap <- function(lat,
                    lng,
                    distance = 10,
                    method = c("2d", "3d"),
                    img.width = 600,
                    img.height = 600,
                    overlay = NULL,
                    z = 9,
                    overlay.alpha = 0.75,
                    colorscale = "imhof4",
                    color.intensity = 1,
                    max.darken = 0.5,
                    sun.angle = 315,
                    sun.altitude = 45,
                    water.cutoff = 0.999,
                    water.min.area = length(heightmap) / 400,
                    water.max.height = NULL,
                    solid = TRUE,
                    shadow = TRUE,
                    water = FALSE,
                    waterdepth = 0,
                    theta = 45,
                    phi = 45,
                    fov = 0,
                    zoom = 1,
                    save.file = c(FALSE, "tif", "png", TRUE),
                    from.file = c(FALSE, "tif", "png", TRUE),
                    tif.filename = NULL,
                    png.filename = NULL,
                    dist.unit = c(
                      "km",
                      "miles",
                      "m",
                      "ft"
                    ),
                    coord.unit = c(
                      "degrees",
                      "radians"
                    ),
                    sr_bbox = 4326,
                    sr_image = 4326,
                    print.map = TRUE) {
  stopifnot(is.logical(print.map))
  stopifnot(length(lat) == length(lng))
  stopifnot(length(z) < 3)
  method <- method[[1]]
  stopifnot(method %in% c("2d", "3d"))

  save.tif <- FALSE
  save.png <- FALSE
  save.file <- save.file[[1]]
  if (save.file == TRUE) {
    save.tif <- TRUE
    if (!is.null(overlay)) {
      save.png <- TRUE
    }
  } else if (save.file == "tif") {
    save.tif <- TRUE
  } else if (save.file == "png") {
    if (!is.null(overlay)) {
      save.png <- TRUE
    } else {
      stop("Cannot save png if overlay isn't specified.")
    }
  }

  from.file <- from.file[[1]]

  if (from.file == TRUE && (save.tif || save.png)) {
    warning("from.file being TRUE overrides save.tif and save.png; files will
  not be overwritten.")
    save.tif <- FALSE
    save.png <- FALSE
  } else if (from.file != FALSE && save.file == TRUE) {
    warning("Can't parse which files you want saved; try again with a different
  save.file argument")
    save.tif <- FALSE
    save.png <- FALSE
  } else if ((from.file == "tif" || from.file == TRUE) && save.tif) {
    warning("Local heightmap used and will not be resaved.")
    save.tif <- FALSE
  } else if ((from.file == "png" || from.file == TRUE) && save.png) {
    warning("Local texture used and will not be resaved.")
    save.png <- FALSE
  }

  if (save.tif && !grepl(".tif$", tif.filename)) {
    stop("Must provide valid filename with .tif extension to save heightmaps.")
  }

  if (save.png && !grepl(".png$", png.filename)) {
    stop("Must provide valid filename with .png extension to save overlays.")
  }

  if (length(z) > 1) {
    if (all(tolower(names(z))) %in% c("water", "land")) {
      stopifnot(sum(grepl("water", names(z))) == 1)
      water.z <- z[["water"]]
      land.z <- z[["land"]]
    }
  } else {
    water.z <- z
    land.z <- z
  }

  if (length(names(colorscale)) < 2) {
    watercolor <- colorscale[[1]]
    landcolor <- colorscale[[1]]
  } else {
    if (all(names(colorscale) %in% c(
      "water",
      "land",
      "watercolor",
      "landcolor"
    ))) {
      if (sum(names(colorscale) %in% c("water", "watercolor")) != 1) {
        stop("Couldn't determine water colorscale.")
      }
      if (sum(names(colorscale) %in% c("land", "landcolor")) != 1) {
        stop("Couldn't determine land colorscale.")
      }
      watercolor <- colorscale[names(colorscale) %in% c("water", "watercolor")][[1]]
      landcolor <- colorscale[names(colorscale) %in% c("land", "landcolor")][[1]]
    }
  }
  if (is.array(watercolor)) {
    warning(paste(
      "Water colors don't work well with gradients; set water to a",
      "single value instead. Defaulting to light blue."
    ))
    watercolor <- "lightblue"
  } else if (grepl("spacey", watercolor)) {
    watercolor <- "lightblue"
  }

  if (!is.array(landcolor)) {
    if (grepl("spacey", landcolor)) landcolor <- get_texture(landcolor)
  }

  if (length(lat) == 1) {
    bound_box <- get_centroid_bounding_box(c(
      "lat" = lat,
      "lng" = lng
    ),
    distance = distance,
    dist.unit = dist.unit,
    coord.unit = coord.unit
    )
  } else {
    if (is.na(distance) || distance == 0) {
      get_centroid_bounding_box(get_centroid(lat, lng, coord.unit),
        distance = distance,
        dist.unit = dist.unit,
        coord.unit = coord.unit
      )
    } else {
      bound_box <- get_coord_bounding_box(lat, lng)
    }
  }

  if (from.file == TRUE || from.file == "tif") {
    heightmap <- load_heightmap(tif.filename)
  } else {
    heightmap <- tryCatch(get_heightmap(bound_box,
      img.width = img.width,
      img.height = img.height,
      save.tif = save.tif,
      filename = tif.filename,
      sr_bbox = sr_bbox,
      sr_image = sr_image
    ),
    error = get_heightmap(bound_box,
      img.width = img.width,
      img.height = img.height,
      save.tif = save.tif,
      filename = tif.filename,
      sr_bbox = sr_bbox,
      sr_image = sr_image
    )
    )
  }

  if (!is.null(overlay)) {
    if (from.file == TRUE || from.file == "png") {
      overlay_img <- load_overlay(png.filename)
    } else {
      overlay_img <- tryCatch(get_image_overlay(bound_box,
        overlay = overlay,
        img.width = img.width,
        img.height = img.height,
        lat = NULL,
        lng = NULL,
        save.png = save.png,
        png.filename = png.filename,
        sr_bbox = sr_bbox,
        sr_image = sr_image
      ),
      error = get_image_overlay(bound_box,
        overlay = overlay,
        img.width = img.width,
        img.height = img.height,
        lat = NULL,
        lng = NULL,
        save.png = save.png,
        png.filename = png.filename,
        sr_bbox = sr_bbox,
        sr_image = sr_image
      )
      )
    }
  }

  if (requireNamespace("future", quietly = TRUE)) {
    base_future <- future::future(heightmap %>%
      rayshader::sphere_shade(
        sunangle = sun.angle,
        colorintensity = color.intensity,
        zscale = z,
        texture = landcolor
      ))
    water_future <- future::future(rayshader::detect_water(heightmap,
      zscale = water.z,
      cutoff = water.cutoff,
      min_area = water.min.area,
      max_height = water.max.height
    ))
    rayshade_future <- future::future(rayshader::ray_shade(heightmap,
      sunaltitude = sun.altitude,
      sunangle = sun.angle,
      zscale = land.z
    ))
    ambient_future <- future::future(rayshader::ambient_shade(heightmap,
      zscale = land.z
    ))
    out <- future::value(base_future) %>%
      rayshader::add_water(future::value(water_future),
        color = watercolor
      ) %>%
      rayshader::add_shadow(future::value(rayshade_future),
        max_darken = max.darken
      ) %>%
      rayshader::add_shadow(future::value(ambient_future),
        max_darken = max.darken
      )
  } else {
    out <- heightmap %>%
      rayshader::sphere_shade(
        sunangle = sun.angle,
        colorintensity = color.intensity,
        zscale = z,
        texture = landcolor
      ) %>%
      rayshader::add_water(rayshader::detect_water(heightmap,
        zscale = water.z,
        cutoff = water.cutoff,
        min_area = water.min.area,
        max_height = water.max.height
      ),
      color = watercolor
      ) %>%
      rayshader::add_shadow(rayshader::ray_shade(heightmap,
        sunaltitude = sun.altitude,
        sunangle = sun.angle,
        zscale = land.z
      ),
      max_darken = max.darken
      ) %>%
      rayshader::add_shadow(rayshader::ambient_shade(heightmap,
        zscale = land.z
      ),
      max_darken = max.darken
      )
  }

  if (!is.null(overlay)) {
    out <- rayshader::add_overlay(out,
      overlay_img,
      alphalayer = overlay.alpha
    )
  }

  if (print.map) {
    if (method == "2d") {
      rayshader::plot_map(out)
    } else {
      rayshader::plot_3d(out,
        heightmap,
        zscale = land.z,
        solid = solid,
        shadow = shadow,
        water = water,
        waterdepth = waterdepth,
        watercolor = watercolor,
        waterlinecolor = NULL,
        theta = theta,
        phi = phi,
        fov = fov,
        zoom = zoom,
        background = "white",
        litbase = FALSE,
        windowsize = c(img.width, img.height)
      )
      Sys.sleep(0.2)
      rayshader::render_snapshot()
    }
  }
  invisible(out)
}
