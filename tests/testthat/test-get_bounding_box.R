test_that("bounding_box functions return identical results", {
  expect_equal(get_centroid_bounding_box(c("lat" = 42.3601, "lng" = -71.0589), 0),
               get_coord_bounding_box(lat = 42.3601, lng = -71.0589))
  expect_equal(get_centroid_bounding_box(c("lat" = 42.3601, "lng" = -71.0589), 1),
               get_coord_bounding_box(lat = c(42.35375,
                                              42.36645),
                                      lng = c(-71.06750,
                                              -71.05030)),
               tolerance = 1e-07)
})
