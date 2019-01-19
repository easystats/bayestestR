context("p_map")

test_that("p_map", {
  testthat::expect_equal(p_map(rnorm_perfect(1000)), 1, tolerance = 0.01)
  testthat::expect_equal(p_map(rnorm_perfect(1000, 1, 1)), 0.62, tolerance = 0.01)
  testthat::expect_equal(p_map(rnorm_perfect(1000, 2, 1)), 0.15, tolerance = 0.01)
  testthat::expect_equal(p_map(rnorm_perfect(1000, 3, 0.01)), 0, tolerance = 0.01)
})
