context("p_direction")

test_that("p_direction", {
  p_direction <- bayestestR::p_direction(bayestestR::rnorm_perfect(100, 1, 1))
  testthat::expect_equal(p_direction, 84, tolerance = 0.2)
})
