context("rnorm")

test_that("rnorm", {
  x <- bayestestR::distribution_normal(10, 0, 1)
  testthat::expect_equal(mean(x), 0, tolerance = 0.02)
})
