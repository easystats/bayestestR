context("map_estimate")

test_that("map_estimate", {
  testthat::expect_equal(map_estimate(rnorm_perfect(1000)), 0, tolerance = 0.01)
  testthat::expect_equal(map_estimate(rnorm_perfect(1000), density = TRUE)$MAP[1], 0, tolerance = 0.01)
})
