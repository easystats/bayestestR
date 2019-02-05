context("as.data.frame.density")

test_that("as.data.frame.density", {
  testthat::expect_is(as.data.frame(density(rnorm_perfect(1000))), "data.frame")
})
