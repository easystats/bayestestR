context("pd")

test_that("pd", {
  pd <- bayestestR::pd(bayestestR::rnorm_perfect(100, 1, 1))
  testthat::expect_equal(pd, 84, tolerance = 0.2)
})
