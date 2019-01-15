context("hdi")

test_that("hdi", {

  testthat::expect_equal(hdi(rnorm_perfect(1000), prob = 0.9)[1], -1.64, tolerance = 0.02)
  testthat::expect_equal(length(hdi(rnorm_perfect(1000), prob = c(0.8, 0.9, 0.95))), 3, tolerance = 0.01)
})