#' Get color textures for a map
#'
#' This function implements a number of new color textures for rayshader maps,
#' allowing users to reference any of the "spacey" palettes without needing to
#' create arrays themselves. It is extremely unlikely users will need to call
#' this function themselves; instead, most users will call it via
#' \code{[automap]}.
#'
#' @param colorscale The color scale to return an RGB array for
#'
#' @return A matrix object provided by \code{\link[png]{readPNG}}, suitable for
#' use with \code{rayshader} and similar mapping utilities.
#'
#' @keywords internal
#'
#' @examples
#' get_texture("spacey1")
#' @export
get_texture <- function(colorscale) {
  if (!grepl("^spacey", colorscale)) {
    return(colorscale)
  }

  if (colorscale == "spacey1") {
    rayshader::create_texture(
      "#53777A",
      "#C02942",
      "#D95B43",
      "#542437",
      "#F7E29E"
    )
  } else if (colorscale == "spacey2") {
    rayshader::create_texture(
      "#99B2B7",
      "#7A6A53",
      "#D5DED9",
      "#948C75",
      "#D9CEB2"
    )
  } else if (colorscale == "spacey3") {
    rayshader::create_texture(
      "#69D2E7",
      "#FA6900",
      "#A7DBD8",
      "#F38630",
      "#E0E4CC"
    )
  } else {
    stop("Spacey texture not found.")
  }
}
