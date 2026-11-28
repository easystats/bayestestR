test_that("estimate_density", {
  skip_if_not_or_load_if_installed("logspline")
  skip_if_not_or_load_if_installed("KernSmooth")
  skip_if_not_or_load_if_installed("mclust")

  set.seed(333)
  x <- distribution_normal(500, 1)

  # Methods
  density_kernel <- estimate_density(x, method = "kernel")
  density_logspline <- estimate_density(x, method = "logspline")
  density_KernSmooth <- estimate_density(x, method = "KernSmooth")
  density_mixture <- estimate_density(x, method = "mixture")

  expect_equal(mean(density_kernel$y - density_logspline$y), 0, tolerance = 0.1)
  expect_equal(mean(density_kernel$y - density_KernSmooth$y), 0, tolerance = 0.1)
  expect_equal(mean(density_kernel$y - density_mixture$y), 0, tolerance = 0.1)

  x <- iris
  x$Fac <- rep_len(c("A", "B"), 150)

  rez <- estimate_density(x, select = "Sepal.Length")
  expect_identical(dim(rez), c(1024L, 3L))

  rez <- estimate_density(x, select = c("Sepal.Length", "Petal.Length"))
  expect_identical(dim(rez), c(2048L, 3L))

  rez <- estimate_density(x, select = "Sepal.Length", by = "Species")
  expect_identical(dim(rez), as.integer(c(1024 * 3, 4)))

  rez <- estimate_density(x, select = c("Sepal.Length", "Petal.Length"), by = "Species")
  expect_identical(dim(rez), as.integer(c(2048 * 3, 4)))

  rez <- estimate_density(x, select = "Sepal.Length", by = c("Species", "Fac"), method = "KernSmooth")
  expect_identical(dim(rez), as.integer(c(1024 * 3 * 2, 5)))
})

test_that("estimate_density uses two-sided normal quantile for CI", {
  x <- distribution_normal(1000)
  h <- stats::density(x, bw = "SJ")$bw
  rk <- 1 / (2 * sqrt(pi))

  d95 <- estimate_density(x, ci = 0.95)
  sd_kde <- sqrt(d95$y * rk / (length(x) * h))
  expect_equal(d95$CI_low, d95$y - stats::qnorm(0.975) * sd_kde, tolerance = 1e-12)
  expect_equal(d95$CI_high, d95$y + stats::qnorm(0.975) * sd_kde, tolerance = 1e-12)

  d50 <- estimate_density(x, ci = 0.50)
  expect_true(all(d50$CI_high > d50$CI_low))

  d25 <- estimate_density(x, ci = 0.25)
  expect_true(all(d25$CI_high > d25$CI_low))
})
