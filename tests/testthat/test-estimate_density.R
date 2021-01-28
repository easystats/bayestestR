if (require("logspline") && require("KernSmooth") && require("mclust")) {
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
  })
}
