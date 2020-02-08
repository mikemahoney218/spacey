test_that("automap creates the same map twice", {
  skip_on_cran()
  boston_map <- tempfile("boston", fileext = ".png")
  x <- automap(
    lat = 42.3601, lng = -71.0589,
    from.file = "tif", tif.filename = "../data/boston_heights.tif",
    z = 0.3
  )
  rayshader::save_png(x, boston_map)
  expect_equal(
    load_overlay(boston_map),
    load_overlay("../data/boston.png"),
    tolerance = 0.004
  )

  expect_equal(
    automap(
      lat = 42.3601, lng = -71.0589,
      overlay = "World_Imagery",
      colorscale = c(
        "water" = "imhof4",
        "land" = "imhof3"
      )
    ),
    automap(
      lat = 42.3601, lng = -71.0589,
      overlay = "World_Imagery",
      colorscale = c(
        "watercolor" = "imhof4",
        "landcolor" = "imhof3"
      )
    )
  )
  expect_invisible(automap(
    lat = 42.3601, lng = -71.0589, from.file = TRUE,
    tif.filename = "../data/boston_heights.tif",
    png.filename = "../data/boston_overlay.png",
    print.map = FALSE
  ))
})

test_that("automap warnings fire appropriately", {
  skip_on_cran()
  expect_warning(automap(
    lat = 42.3601, lng = -71.0589, from.file = TRUE,
    tif.filename = "../data/boston_heights.tif",
    png.filename = "../data/boston_overlay.png",
    save.file = TRUE
  ))
  expect_warning(automap(
    lat = 42.3601, lng = -71.0589, from.file = "tif",
    tif.filename = "../data/boston_heights.tif",
    save.file = TRUE
  ))
  expect_warning(automap(
    lat = 42.3601, lng = -71.0589, from.file = "png",
    png.filename = "../data/boston_overlay.png",
    save.file = TRUE
  ))
})

test_that("automap errors fire appropriately", {
  expect_error(automap(
    lat = 42.3601, lng = -71.0589, save.file = TRUE,
    tif.filename = "junk.png"
  ))
  expect_error(automap(
    lat = 42.3601, lng = -71.0589, save.file = TRUE,
    png.filename = "junk.tif"
  ))

  expect_error(automap(lat = 42.3601, lng = -71.0589, overlay = TRUE))

  expect_error(automap(
    lat = 42.3601, lng = -71.0589,
    colorscale = c(
      "water" = "imhof4",
      "watercolor" = "imhof4"
    )
  ))
  expect_error(automap(
    lat = 42.3601, lng = -71.0589,
    colorscale = c(
      "water" = "imhof4",
      "other" = "imhof4"
    )
  ))
})
