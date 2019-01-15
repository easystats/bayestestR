context("odds_h0")

test_that("odds_h0", {

  testthat::expect_equal(odds_h0(rnorm_perfect(1000)), 1, tolerance = 0.01)
  testthat::expect_equal(odds_h0(rnorm_perfect(1000, 1, 1)), 1.60, tolerance = 0.01)
  testthat::expect_equal(odds_h0(rnorm_perfect(1000, 2, 1)), 6.66, tolerance = 0.01)
  testthat::expect_equal(odds_h0(rnorm_perfect(1000, 3, 0.01)), Inf, tolerance = 0.01)
})