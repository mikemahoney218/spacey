test_that("get_heightmap stops early for bad image sizes", {
  expect_error(get_heightmap(
    get_coord_bounding_box(lat = 42.3601, lng = -71.0589),
    img.width = 8001, img.height = 8001
  ))
})
