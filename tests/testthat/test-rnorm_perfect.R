context("rnorm")

test_that("rnorm", {
  x <- bayestestR::rnorm_perfect(10, 0, 1)
  testthat::expect_equal(mean(x), 0, tolerance = 0.02)
})
