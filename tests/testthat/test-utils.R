test_that("rad_to_deg and deg_to_rad reverse each other", {
  x <- seq(0, 360, by = 1)
  expect_equal(
    x,
    rad_to_deg(deg_to_rad(x))
  )
})


test_that("extract_coords fails appropriately", {
  bbox <- list(
    "bl" = c(44.05771, -73.99212),
    "tr" = c(44.18475, -73.81515)
  )
  expect_error(extract_coords(bbox[[1]]))
  bbox <- list(
    "bl" = c("lat" = 44.05771, -73.99212),
    "tr" = c(44.18475, -73.81515)
  )
  expect_error(extract_coords(bbox[[1]]))
})
