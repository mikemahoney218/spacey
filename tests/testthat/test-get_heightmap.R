test_that("get_heightmap stops early for bad image sizes", {
  expect_error(get_heightmap(
    get_coord_bounding_box(lat = 42.3601, lng = -71.0589),
    img.width = 8001, img.height = 8001
  ))
})

test_that("get_heightmap fails appropriately", {
  bbox <- get_centroid_bounding_box(c(
    "lat" = 44.121268,
    "lng" = -73.903734
  ),
  distance = 10
  )

  expect_error(get_heightmap(bbox, save.tif = TRUE))
})

test_that("coordinate extraction methods are equivalent", {
  skip_on_cran()
  bbox <- get_centroid_bounding_box(c(
    "lat" = 44.121268,
    "lng" = -73.903734
  ),
  distance = 10
  )

  expect_equal(
    get_heightmap(bbox, lat = "lat", lng = "lng"),
    get_heightmap(bbox)
  )
})
