test_that("automap creates the same map twice", {
  skip_on_cran()
  expect_equal(
    automap(
      lat = 42.3601, lng = -71.0589,
      overlay = "World_Imagery",
      colorscale = c(
        "water" = "imhof4",
        "land" = "imhof3"
      ),
      distance = 1
    ),
    automap(
      lat = 42.3601, lng = -71.0589,
      overlay = "World_Imagery",
      colorscale = c(
        "water" = "imhof4",
        "land" = "imhof3"
      ),
      distance = 1
    ),
    tolerance = 1e-7
  )
})

test_that("automap builds from local files", {
  tif <- load_heightmap("../data/boston_heights.tif")
  overlay <- load_overlay("../data/boston_overlay.png")

  expect_visible(automap(
    heightmap = tif,
    texture = overlay
  ))
})

test_that("automap errors fire appropriately", {
  expect_error(automap(lat = 42.3601, lng = -71.0589, overlay = TRUE))
  expect_error(automap(
    lat = 42.3601, lng = -71.0589,
    colorscale = "spacey10"
  ))
})


test_that("custom color scales work", {
  skip_on_cran()
  expect_error(automap(
    lat = 42.3601, lng = -71.0589,
    colorscale = "spacey1"
  ), NA)
  expect_error(automap(
    lat = 42.3601, lng = -71.0589,
    colorscale = "spacey2"
  ), NA)
  expect_error(automap(
    lat = 42.3601, lng = -71.0589,
    colorscale = "spacey3"
  ), NA)
})

test_that("3d maps work", {
  skip_on_appveyor()
  skip_on_travis()
  skip_on_cran()
  expect_error(
    automap(
      lat = 42.3601, lng = -71.0589,
      colorscale = c(
        land = "spacey2",
        water = "lightblue"
      ),
      method = "3d"
    ),
    NA
  )
})
