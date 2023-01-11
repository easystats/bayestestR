if (requiet("logspline") && requiet("KernSmooth") && requiet("mclust")) {
  test_that("estimate_density", {
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
    x$Fac <- rep(c("A", "B"), length.out = 150)

    rez <- estimate_density(x, select = "Sepal.Length")
    expect_equal(dim(rez), c(1024, 3))

    rez <- estimate_density(x, select = c("Sepal.Length", "Petal.Length"))
    expect_equal(dim(rez), c(2048, 3))

    rez <- estimate_density(x, select = "Sepal.Length", at = "Species")
    expect_equal(dim(rez), c(1024 * 3, 4))

    rez <- estimate_density(x, select = c("Sepal.Length", "Petal.Length"), at = "Species")
    expect_equal(dim(rez), c(2048 * 3, 4))

    rez <- estimate_density(x, select = "Sepal.Length", at = c("Species", "Fac"), method = "KernSmooth")
    expect_equal(dim(rez), c(1024 * 3 * 2, 5))
  })
}
