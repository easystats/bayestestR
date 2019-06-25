context("p_direction")

test_that("p_direction", {
  set.seed(333)
  x <- bayestestR::distribution_normal(10000, 1, 1)
  pd <- bayestestR::p_direction(x)
  testthat::expect_equal(as.numeric(pd), 0.842, tolerance = 0.1)
  testthat::expect_equal(as.numeric(p_direction(x, method = "kernel")), 0.842, tolerance = 0.1)
  testthat::expect_equal(nrow(p_direction(data.frame(replicate(4, rnorm(100))))), 4)
  testthat::expect_is(pd, "p_direction")
  testthat::expect_equal(tail(capture.output(print(pd)), 1), "pd = 84.14%")
})
