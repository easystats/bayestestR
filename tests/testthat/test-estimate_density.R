context("estimate_density")

test_that("estimate_density", {
  library(logspline)
  library(KernSmooth)

  set.seed(333)
  x <- distribution_normal(500, 1)

  #' Methods
  density_kernel <- estimate_density(x, method = "kernel")
  density_logspline <- estimate_density(x, method = "logspline")
  density_KernSmooth <- estimate_density(x, method = "KernSmooth")

  testthat::expect_equal(mean(density_kernel$y - density_logspline$y), 0, tol=0.1)
  testthat::expect_equal(mean(density_kernel$y - density_KernSmooth$y), 0, tol=0.1)
})
