#' spacey map objects
#'
#' @description
#' This class powers \code{automap} and related functions.
#'
#' @keywords internal
#' @importFrom R6 R6Class
#' @export
spacey_map <- R6::R6Class("spacey_map",
  ##############################################################################
  ############################ PUBLIC FIELDS ###################################
  ##############################################################################
  public = list(
    ############################################################################
    # Fields used in API requests
    ############################################################################
    #' @field lat The latitude for the map's centroid, in decimal degrees or
    #' radians.
    lat = NULL,
    #' @field lng The longitude for the map's centroid, in decimal degrees or
    #' radians.
    lng = NULL,
    #' @field distance The distance between the centroid and any corner of the
    #' (square) output map to include.
    distance = 10,
    #' @field method Should a 2d or 3d plot be produced?
    method = "2d",
    #' @field img.width Image width, in pixels
    img.width = 600,
    #' @field img.height Image height, in pixels
    img.height = 600,
    #' @field overlay ESRI overlay map to include, if any -- see
    #' \code{\link[spacey]{get_image_overlay}} for list of options. If specifying
    #' a local file with \code{from.file}, any non \code{NULL} value will add your
    #' local file as an overlay.
    overlay = NULL,
    ############################################################################
    # Fields used for rayshader calculations
    ############################################################################
    #' @field z zscale, passed to various rayshader functions. Defined as
    #' the ratio between the x and y spacing (which are assumed to be equal)
    #' and the z axis. For contiguous United States, USGS data is generally
    #' available at a ~9 meter spacing, with elevation provided in meter increments,
    #' resulting in a z value of 9 returning roughly representative maps; decrease
    #' this value to exaggerate elevation features.
    z = 9,
    #' @field land.z zscale for land elements.
    land.z = 9,
    #' @field water.z zscale for water elements.
    water.z = 9,
    #' @field overlay.alpha Alpha value for the optional overlay layer.
    overlay.alpha = 0.75,
    #' @field colorscale Color scale for land and water elements. If a vector of
    #' length 1, the same color scale will be applied for both land and water. If
    #' greater than  length 1, values named "water" will be used
    #' for water coloring, while "land" will be used for land.
    colorscale = "imhof4",
    #' @field water.color Color scale for water elements
    water.color = NULL,
    #' @field land.color Color scale for land elements
    land.color = NULL,
    #' @field color.intensity Intensity of color mapping -- higher values result in
    #' more intense colors.
    color.intensity = 1,
    #' @field max.darken Passed to \code{\link[rayshader]{add_shadow}}. The lower
    #' limit for how much the image will be darkened. 0 is completely black, 1 means
    #' the shadow map will have no effect.
    max.darken = 0.5,
    #' @field sun.angle Angle around the matrix from which lights originate. Values
    #' line up with compass directions -- so 0 is directly North, while the default
    #' 315 places the sun in the Northwest.
    sun.angle = 315,
    #' @field sun.altitude Angle in degrees from horizon from which light
    #' originates. Bounded [0, 90].
    sun.altitude = 45,
    ############################################################################
    # Fields used for 3D rendering
    ############################################################################
    #' @field water.cutoff Passed to \code{\link[rayshader]{detect_water}}. Defined
    #' therein as the lower limit of the z-component of the unit normal vector to
    #' be classified as water.
    water.cutoff = 0.999,
    #' @field water.min.area Passed to \code{\link[rayshader]{detect_water}}. The
    #' minimum possible area to consider a body of water.
    water.min.area = NULL,
    #' @field water.max.height Passed to \code{\link[rayshader]{detect_water}}. If
    #' provided, the maximum height a point can be classified water.
    water.max.height = NULL,
    #' @field solid Logical -- should the output be rendered as a solid
    #' (\code{TRUE}) or just a surface (\code{FALSE})?
    solid = TRUE,
    #' @field shadow Logical -- should shadows be rendered?
    shadow = TRUE,
    #' @field water Logical -- should water be rendered?
    water = FALSE,
    #' @field waterdepth Water level.
    waterdepth = 0,
    #' @field theta Rotation around z axis.
    theta = 45,
    #' @field phi Azimuth angle.
    phi = 45,
    #' @field fov Field of view angle.
    fov = 0,
    #' @field zoom Zoom factor for 3D maps.
    zoom = 1,
    #' @field dist.unit Units for the distance argument. All units are converted to
    #' kilometers, with some errors in the transition due to floating point
    #' arithmetic -- so if high accuracy is needed, convert units to kilometers
    #' beforehand.
    dist.unit = c(
      "km",
      "miles",
      "m",
      "ft"
    ),
    #' @field coord.unit Units for latitude and longitude, in either decimal degrees
    #' or radians.
    coord.unit = c(
      "degrees",
      "radians"
    ),
    ############################################################################
    # Fields typically calculated by automap functions
    ############################################################################
    #' @field bbox Bounding box for map data, calculated based on lat/lng and
    #' distance values.
    bbox = NULL,
    #' @field heightmap Heightmap array, either provided by the user or pulled
    #' via the USGS national map API.
    heightmap = NULL,
    #' @field texture Overlay image array, either provided by the user or pulled
    #' from the ESRI map texture API.
    texture = NULL,
    #' @field normals Normal vectors, calculated and cached to speed
    #' recalculation.
    normals = NULL,
    #' @field water_future Array of water values.
    water_future = NULL,
    #' @field base_future Array of base elevation sphere shading.
    base_future = NULL,
    #' @field rayshade_future Array of ray shaded values.
    rayshade_future = NULL,
    #' @field ambient_future Array of ambient shadow shading values.
    ambient_future = NULL,
    #' @field map_obj The final map image.
    map_obj = NULL,
    ############################################################################
    # Initialization function
    ############################################################################

    #' @description
    #' Create a new spacey_map object.
    #' @param ... Fields passed from \code{automap} to initalize the object.
    #' @return A new \code{spacey_map} object.
    initialize = function(...) {

      ##########################################################################
      # Assign inputs
      ##########################################################################
      dots <- list(...)
      for (i in seq_along(dots)) {
        self[[names(dots)[[i]]]] <- dots[[i]]
      }

      names(self$z) <- tolower(names(self$z))
      if (length(self$z) > 1) {
        self$water.z <- self$z["water"]
        self$land.z <- self$z <- self$z["land"]
      } else {
        self$land.z <- self$water.z <- self$z
      }

      names(self$colorscale) <- tolower(names(self$colorscale))
      if (length(self$colorscale) > 1) {
        self$water.color <- self$colorscale["water"]
        self$land.color <- self$colorscale <- self$colorscale["land"]
      } else {
        self$land.color <- self$water.color <- self$colorscale
      }
      if (grepl("^spacey", self$water.color)) self$water.color <- "lightblue"
      self$water.color <- get_texture(self$water.color)
      self$land.color <- get_texture(self$land.color)
      self$colorscale <- get_texture(self$colorscale)

      ##########################################################################
      # Run calculations
      ##########################################################################
      private$new_data(self$heightmap, self$texture, self$overlay)
      ## make the map!
      self$map_obj <- private$make_map()
    },

    ############################################################################
    # Print function
    ############################################################################
    #' @description
    #' Print out the map object within a spacey_map object.
    print = function() {
      # 2d print
      if (self$method == "2d") {
        rayshader::plot_map(self$map_obj)
      } else {
        # 3d print
        rayshader::plot_3d(
          self$map_obj,
          self$heightmap,
          zscale = self$land.z,
          solid = self$solid,
          shadow = self$shadow,
          water = self$water,
          waterdepth = self$waterdepth,
          watercolor = self$watercolor,
          waterlinecolor = NULL,
          theta = self$theta,
          phi = self$phi,
          fov = self$fov,
          zoom = self$zoom,
          background = "white", # kinda weird that we don't let people choose
          litbase = FALSE,
          windowsize = c(self$img.width, self$img.height)
        )
        Sys.sleep(0.2)
        rayshader::render_snapshot()
      }
    },

    ############################################################################
    # Update values function
    ############################################################################
    #' @description
    #' Update values within the map object and recalculate arrays as necessary.
    #'
    #' @param ... Any combination of fields accepted by \code{spacey_map}.
    #' Fields must be named.
    update_values = function(...) {
      dots <- list(...)
      for (i in seq_along(dots)) {
        self[[names(dots)[[i]]]] <- dots[[i]]
      }

      run_land <- run_water <- run_base <- run_ray <- run_map <- FALSE

      if (any(names(dots) %in% c("z", "colorscale"))) {
        if (any(names(dots) %in% c("z"))) {
          names(self$z) <- tolower(names(self$z))
          if (length(self$z) > 1) {
            self$water.z <- self$z["water"]
            self$land.z <- self$z <- self$z["land"]
          } else {
            self$land.z <- self$water.z <- self$z
          }
        }
        if (any(names(dots) %in% c("colorscale"))) {
          names(self$colorscale) <- tolower(names(self$colorscale))
          if (length(self$colorscale) > 1) {
            self$water.color <- self$colorscale["water"]
            self$land.color <- self$colorscale <- self$colorscale["land"]
          } else {
            self$land.color <- self$water.color <- self$colorscale
          }
          if (grepl("^spacey", self$water.color)) self$water.color <- "lightblue"
          self$water.color <- get_texture(self$water.color)
          self$land.color <- get_texture(self$land.color)
          self$colorscale <- get_texture(self$colorscale)
        }
        run_land <- TRUE
        run_water <- TRUE
      } else if (any(names(dots) %in% c("land.z", "land.color"))) {
        run_land <- TRUE
      } else if (any(names(dots) %in% c("water.z", "water.color"))) {
        run_water <- TRUE
      }

      if (any(names(dots) == "sun.angle")) {
        run_base <- TRUE
        run_ray <- TRUE
        # rerun calc_base, calc_rayshade
      } else {
        if (any(names(dots) == "color.intensity")) {
          run_base <- TRUE
        }
        if (any(names(dots) == "sun.altitude")) {
          run_ray <- TRUE
        }
      }

      if (any(names(dots) == "overlay")) {
        self$texture <- private$fetch_texture()
        run_map <- TRUE
      } else if (any(names(dots) == "texture")) {
        run_map <- TRUE
      }

      if (any(names(dots) %in% c(
        "lat",
        "lng",
        "distance",
        "img.width",
        "img.height"
      ))) {
        private$new_data(NULL, NULL, NULL)
        run_map <- TRUE
      }

      if (any(names(dots) %in% c("overlay.alpha", "max.darken"))) {
        run_map <- TRUE
      }

      if (run_land) {
        self$base_future <- private$calc_base()
        self$rayshade_future <- private$calc_rayshade()
        self$ambient_future <- private$calc_ambient()
        run_map <- TRUE
        run_base <- FALSE
        run_ray <- FALSE
      }
      if (run_water) {
        self$water_future <- private$calc_water()
        run_map <- TRUE
      }
      if (run_base) {
        self$base_future <- private$calc_base()
        run_map <- TRUE
      }
      if (run_ray) {
        self$rayshade_future <- private$calc_rayshade()
        run_map <- TRUE
      }
      if (run_map) {
        self$map_obj <- private$make_map()
      }
    }
  ),
  ##############################################################################
  ############################ PRIVATE FIELDS ##################################
  ##############################################################################
  private = list(
    ############################################################################
    # Function to run calculations
    ############################################################################
    new_data = function(heightmap, texture, overlay) {
      ##########################################################################
      # Calculate bounding box
      ##########################################################################
      if (!is.null(self$lat)) {
        self$bbox <- get_centroid_bounding_box(c(
          "lat" = self$lat,
          "lng" = self$lng
        ),
        distance = self$distance,
        dist.unit = self$dist.unit,
        coord.unit = self$coord.unit
        )
      }
      ##########################################################################
      # Download data
      ##########################################################################
      # If the user hasn't provided a heightmap...
      if (is.null(heightmap)) {
        # go get one:
        self$heightmap <- private$fetch_heightmap()
      } else {
        self$heightmap <- heightmap
      }

      if (is.null(self$water.min.area)) {
        self$water.min.area <- length(self$heightmap) / 400
      }

      # If the user hasn't provided an overlay...
      if (!is.null(texture)) {
        self$texture <- texture
        # but has told us they want one...
      } else if (!is.null(overlay)) {
        # go get one:
        self$texture <- private$fetch_texture()
      }

      ##########################################################################
      # Run rayshader functions
      ##########################################################################
      ## give calculating normal vectors a head start, it takes a while
      self$normals <- private$calc_normals()
      ## these don't accept normals as a parameter
      self$rayshade_future <- private$calc_rayshade()
      self$ambient_future <- private$calc_ambient()
      ## now force normals to be evaluated
      self$water_future <- private$calc_water()
      self$base_future <- private$calc_base()
    },
    ############################################################################
    # Force evaluation of future objects, if needed
    ############################################################################
    force_eval = function(object) {
      if ("Future" %in% class(object)) {
        future::value(object)
      } else {
        object
      }
    },
    ############################################################################
    # API Functions
    ############################################################################
    fetch_heightmap = function() {
      tryCatch(get_heightmap(self$bbox,
        img.width = self$img.width,
        img.height = self$img.height,
        save.tif = FALSE,
        tif.filename = NULL,
        sr_bbox = 4326,
        sr_image = 4326
      ),
      error = get_heightmap(self$bbox,
        img.width = self$img.width,
        img.height = self$img.height,
        save.tif = FALSE,
        tif.filename = NULL,
        sr_bbox = 4326,
        sr_image = 4326
      )
      )
    },
    fetch_texture = function() {
      tryCatch(get_image_overlay(self$bbox,
        overlay = self$overlay,
        img.width = self$img.width,
        img.height = self$img.height,
        lat = NULL,
        lng = NULL,
        save.png = FALSE,
        png.filename = NULL,
        sr_bbox = 4326,
        sr_image = 4326
      ),
      error = get_image_overlay(self$bbox,
        overlay = self$overlay,
        img.width = self$img.width,
        img.height = self$img.height,
        lat = NULL,
        lng = NULL,
        save.png = FALSE,
        png.filename = NULL,
        sr_bbox = 4326,
        sr_image = 4326
      )
      )
    },
    ############################################################################
    # Futureized rayshader functions
    ############################################################################
    calc_normals = function() {
      future::future(rayshader::calculate_normal(
        heightmap = self$heightmap,
        zscale = self$land.z,
        progbar = FALSE
      ))
    },
    calc_water = function() {
      self$normals <- private$force_eval(self$normals)

      future::future(
        rayshader::detect_water(
          heightmap = self$heightmap,
          zscale = self$water.z,
          cutoff = self$water.cutoff,
          min_area = self$water.min.area,
          max_height = self$water.max.height,
          normalvectors = self$normals,
          progbar = FALSE
        )
      )
    },
    calc_base = function() {
      self$normals <- private$force_eval(self$normals)

      future::future(
        rayshader::sphere_shade(
          heightmap = self$heightmap,
          sunangle = self$sun.angle,
          texture = self$land.color,
          normalvectors = self$normals,
          colorintensity = self$color.intensity,
          zscale = self$land.z,
          progbar = FALSE
        )
      )
    },
    calc_rayshade = function() {
      future::future(
        rayshader::ray_shade(
          self$heightmap,
          sunaltitude = self$sun.altitude,
          sunangle = self$sun.angle,
          zscale = self$land.z,
          progbar = FALSE
        )
      )
    },
    calc_ambient = function() {
      future::future(
        rayshader::ambient_shade(
          heightmap = self$heightmap,
          zscale = self$land.z
        )
      )
    },
    make_map = function() {
      self$base_future <- private$force_eval(self$base_future)
      self$water_future <- private$force_eval(self$water_future)
      self$rayshade_future <- private$force_eval(self$rayshade_future)
      self$ambient_future <- private$force_eval(self$ambient_future)

      base_map <- function() {
        self$base_future %>%
          rayshader::add_water(self$water_future,
            color = self$water.color
          ) %>%
          rayshader::add_shadow(self$rayshade_future,
            max_darken = self$max.darken
          ) %>%
          rayshader::add_shadow(self$ambient_future,
            max_darken = self$max.darken
          )
      }

      if (!is.null(self$texture)) {
        base_map() %>%
          rayshader::add_overlay(self$texture, alphalayer = self$overlay.alpha)
      } else {
        base_map()
      }
    }
  )
)

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
#' @param phi Azimuth angle.
#' @param fov Field of view angle.
#' @param zoom Zoom factor.
#' @param dist.unit Units for the distance argument. All units are converted to
#' kilometers, with some errors in the transition due to floating point
#' arithmetic -- so if high accuracy is needed, convert units to kilometers
#' beforehand.
#' @param coord.unit Units for latitude and longitude, in either decimal degrees
#' or radians.
#' @param heightmap Heightmap array, either provided by the user or pulled
#' via the USGS national map API.
#' @param texture Overlay image array, either provided by the user or pulled
#' from the ESRI map texture API.
#'
#' @return A rayshader map object, by default printed and returned invisibly.
#'
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' automap(44.121268, -73.903734)
#' automap(44.121268, -73.903734, overlay = "World_Imagery")
#' automap(44.121268, -73.903734, overlay = "World_Imagery", method = "3d")
#' }
#'
#' @references
#' Archuleta, C.M., Constance, E.W., Arundel, S.T., Lowe, A.J., Mantey, K.S.,
#' and Phillips, L.A., 2017, The National Map seamless digital elevation model
#' specifications: U.S. Geological Survey Techniques and Methods, book 11,
#' chap. B9, 39 p., https://doi.org/10.3133/tm11B9
#'
#' @export

automap <- function(lat = NULL, lng = NULL, distance = 10, method = "2d",
                    img.width = 600, img.height = 600, overlay = NULL,
                    z = 9, overlay.alpha = 0.75, colorscale = "imhof4",
                    color.intensity = 1, max.darken = 0.5, sun.angle = 315,
                    sun.altitude = 45, water.cutoff = 0.999,
                    water.min.area = NULL, water.max.height = NULL,
                    solid = TRUE, shadow = TRUE, water = FALSE,
                    waterdepth = 0, theta = 45, phi = 45, fov = 0,
                    zoom = 1, dist.unit = "km", coord.unit = "degrees",
                    heightmap = NULL, texture = NULL) {

  ##########################################################################
  # Validate Inputs
  ##########################################################################
  stopifnot(is.numeric(lat) || is.null(lat))
  stopifnot(is.numeric(lng) || is.null(lng))
  stopifnot(is.numeric(distance) && length(distance) == 1)
  stopifnot(is.numeric(img.width) && length(img.width) == 1)
  stopifnot(is.numeric(img.height) && length(img.height) == 1)
  stopifnot(is.numeric(z) && length(z) < 3)
  stopifnot(is.numeric(overlay.alpha) && length(overlay.alpha) == 1)
  stopifnot(is.numeric(color.intensity) && length(color.intensity) == 1)
  stopifnot(is.numeric(max.darken)) # can this be > 1? rayshade and ambient
  stopifnot(is.numeric(sun.angle)) # can this be > 1? sphereshade and rayshade
  stopifnot(is.numeric(sun.altitude) && length(sun.altitude) == 1)
  stopifnot(is.numeric(water.cutoff) && length(water.cutoff) == 1)
  stopifnot(is.null(water.min.area) ||
    (is.numeric(water.min.area) && length(water.min.area) == 1))
  stopifnot(is.null(water.max.height) ||
    is.numeric(water.max.height) && length(water.max.height == 1))
  stopifnot(is.numeric(waterdepth) && length(waterdepth) == 1)
  stopifnot(is.numeric(theta) && length(theta) == 1)
  stopifnot(is.numeric(phi) && length(phi) == 1)
  stopifnot(is.numeric(fov) && length(fov) == 1)
  stopifnot(is.numeric(zoom) && length(zoom) == 1)

  stopifnot(is.logical(solid) && length(solid) == 1)
  stopifnot(is.logical(shadow) && length(shadow) == 1)
  stopifnot(is.logical(water) && length(water) == 1)

  stopifnot(is.null(overlay) || (is.character(overlay) && length(overlay) == 1))
  stopifnot(tolower(method) %in% c("2d", "3d") && length(method) == 1)
  stopifnot(is.character(colorscale)) ## & all(colorscale %in% c(list_of_colors))
  stopifnot(is.character(coord.unit) &&
    tolower(coord.unit) %in% c("degrees", "radians") &&
    length(coord.unit) == 1)
  stopifnot(is.character(dist.unit) &&
    tolower(dist.unit) %in% c("km", "m", "mi", "ft") &&
    length(dist.unit) == 1)

  stopifnot(is.null(heightmap) ||
    any(class(heightmap) %in% c("matrix", "array")))
  stopifnot(is.null(texture) ||
    any(class(texture) %in% c("matrix", "array")))

  spacey_map$new(
    lat = lat, lng = lng, distance = distance, method = method,
    img.width = img.width, img.height = img.height, overlay = overlay,
    z = z, overlay.alpha = overlay.alpha, colorscale = colorscale,
    color.intensity = color.intensity, max.darken = max.darken,
    sun.angle = sun.angle, sun.altitude = sun.altitude,
    water.cutoff = water.cutoff, water.min.area = water.min.area,
    water.max.height = water.max.height, solid = solid,
    shadow = shadow, water = water, waterdepth = waterdepth,
    theta = theta, phi = phi, fov = fov, zoom = zoom,
    dist.unit = dist.unit, coord.unit = coord.unit,
    heightmap = heightmap, texture = texture
  )
}
