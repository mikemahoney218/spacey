test_that("get_centroid works correctly for coord pairs", {
  expect_equal(
    get_centroid(
      lat = c(42.35375, 42.36645),
      lng = c(-71.06750, -71.05030)
    ),
    c(
      "lat" = 42.3601,
      "lng" = -71.0589
    )
  )
  expect_equal(
    get_centroid(
      lat = c(42.35375, 42.36645),
      lng = c(-71.06750, -71.05030)
    ),
    rad_to_deg(get_centroid(
      lat = deg_to_rad(c(42.35375, 42.36645)),
      lng = deg_to_rad(c(-71.06750, -71.05030)),
      coord.unit = "radians"
    ))
  )
})

test_that("get_centroid returns early for single coordinates", {
  expect_equal(
    get_centroid(lat = 42, lng = -71),
    c("lat" = 42, "lng" = -71)
  )
})

test_that("get_centroid requires full coord pairs", {
  expect_error(get_centroid(
    lat = c(42.35375),
    lng = c(-71.06750, -71.05030)
  ))
})
