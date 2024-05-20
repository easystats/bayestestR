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
