if (require("bayesQR", quietly = TRUE)) {
  test_that("bayesQR", {
    library(bayestestR)
    x <- bayesQR::bayesQR(Sepal.Length ~ Petal.Width, data=iris, quantile = 0.1, alasso = TRUE, ndraw=500)

    rez <- p_direction(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 2))

    rez <- p_map(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 2))

    rez <- p_significance(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 2))

    rez <- rope(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 5))

    rez <- hdi(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 4))

    rez <- eti(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 4))

    rez <- map_estimate(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 2))

    rez <- point_estimate(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 4))

    rez <- describe_posterior(x)
    testthat::expect_equal(c(nrow(rez), ncol(rez)), c(2, 10))
  })
}