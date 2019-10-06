context("overlap")

test_that("overlap", {

  set.seed(333)
  x <- distribution_normal(1000, 2, 0.5)
  y <- distribution_normal(1000, 0, 1)

  testthat::expect_equal(as.numeric(overlap(x, y)), 0.185, tol = 0.01)
})
