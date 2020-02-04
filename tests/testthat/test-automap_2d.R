test_that("automap_2d creates the same map twice", {
  skip_on_cran()
  boston_map <- tempfile("boston", fileext = ".png")
  x <- automap_2d(
    lat = 42.3601, lng = -71.0589,
    from.file = "tif", tif.filename = "../data/boston_heights.tif"
  )
  rayshader::save_png(x, boston_map)
  expect_equal(
    load_overlay(boston_map),
    load_overlay("../data/boston.png")
  )

  expect_equal(
    automap_2d(
      lat = 42.3601, lng = -71.0589,
      overlay = "World_Imagery",
      colorscale = c(
        "water" = "imhof4",
        "land" = "imhof3"
      )
    ),
    automap_2d(
      lat = 42.3601, lng = -71.0589,
      overlay = "World_Imagery",
      colorscale = c(
        "watercolor" = "imhof4",
        "landcolor" = "imhof3"
      )
    )
  )
  expect_invisible(automap_2d(
    lat = 42.3601, lng = -71.0589, from.file = TRUE,
    tif.filename = "../data/boston_heights.tif",
    png.filename = "../data/boston_overlay.png",
    print.map = FALSE
  ))
})

test_that("automap_2d warnings fire appropriately", {
  skip_on_cran()
  expect_warning(automap_2d(
    lat = 42.3601, lng = -71.0589, from.file = TRUE,
    tif.filename = "../data/boston_heights.tif",
    png.filename = "../data/boston_overlay.png",
    save.png = TRUE
  ))
  expect_warning(automap_2d(
    lat = 42.3601, lng = -71.0589, from.file = "tif",
    tif.filename = "../data/boston_heights.tif",
    save.tif = TRUE
  ))
  expect_warning(automap_2d(
    lat = 42.3601, lng = -71.0589, from.file = "png",
    png.filename = "../data/boston_overlay.png",
    save.png = TRUE
  ))
})

test_that("automap_2d errors fire appropriately", {
  expect_error(automap_2d(
    lat = 42.3601, lng = -71.0589, save.tif = TRUE,
    tif.filename = "junk.png"
  ))
  expect_error(automap_2d(
    lat = 42.3601, lng = -71.0589, save.png = TRUE,
    png.filename = "junk.tif"
  ))

  expect_error(automap_2d(lat = 42.3601, lng = -71.0589, overlay = TRUE))

  expect_error(automap_2d(
    lat = 42.3601, lng = -71.0589,
    colorscale = c(
      "water" = "imhof4",
      "watercolor" = "imhof4"
    )
  ))
  expect_error(automap_2d(
    lat = 42.3601, lng = -71.0589,
    colorscale = c(
      "water" = "imhof4",
      "other" = "imhof4"
    )
  ))
})

