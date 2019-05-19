context("describe_posterior")

test_that("describe_posterior", {
  set.seed(333)

  # Numeric
  x <- distribution_normal(1000)
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")
  testthat::expect_equal(dim(rez), c(1, 16))
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(2, 16))
  rez <- describe_posterior(x, estimate = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
  testthat::expect_equal(dim(rez), c(1, 4))

  # Dataframes
  x <- data.frame(replicate(4, rnorm(100)))
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")
  testthat::expect_equal(dim(rez), c(4, 16))
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(8, 16))
  rez <- describe_posterior(x, estimate = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
  testthat::expect_equal(dim(rez), c(4, 4))

  # Rstanarm
  library(rstanarm)
  x <- insight::download_model("stanreg_lm_1")
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")
  testthat::expect_equal(dim(rez), c(2, 18))
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(4, 18))
  rez <- describe_posterior(x, estimate = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile", diagnostic = NULL)
  testthat::expect_equal(dim(rez), c(2, 4))

  # Brms
  library(brms)
  x <- insight::download_model("brms_mixed_1")
  # rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")  # doenst work because of BFs
  # testthat::expect_equal(dim(rez), c(4, 16))
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(4, 16))
  rez <- describe_posterior(x, estimate = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile", diagnostic = NULL)
  testthat::expect_equal(dim(rez), c(2, 4))


  # BayesFactor
  # library(BayesFactor)
  # x <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
  # rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")
  # testthat::expect_equal(dim(rez), c(4, 16))
  # rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  # testthat::expect_equal(dim(rez), c(8, 16))
  # rez <- describe_posterior(x, estimate = NULL, dispersion = TRUE, test = NULL, ci_method="quantile")
  # testthat::expect_equal(dim(rez), c(4, 4))
})
