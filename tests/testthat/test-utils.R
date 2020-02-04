test_that("rad_to_deg and deg_to_rad reverse each other", {
  x <- seq(0, 360, by = 1)
  expect_equal(
    x,
    rad_to_deg(deg_to_rad(x))
  )
})
