context("describe_posterior")

test_that("describe_posterior", {
  set.seed(333)

  # Numeric
  x <- distribution_normal(1000)
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
  testthat::expect_equal(dim(rez), c(1, 17))
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9)))
  testthat::expect_equal(dim(rez), c(2, 17))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
  testthat::expect_equal(dim(rez), c(1, 4))

  # Dataframes
  x <- data.frame(replicate(4, rnorm(100)))
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
  testthat::expect_equal(dim(rez), c(4, 17))
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9)))
  testthat::expect_equal(dim(rez), c(8, 17))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
  testthat::expect_equal(dim(rez), c(4, 4))
  # rez <- testthat::expect_warning(describe_posterior(x, ci = c(0.8, 0.9)))
  # testthat::expect_equal(dim(rez), c(8, 17))

  # Rstanarm
  library(rstanarm)
  x <- insight::download_model("stanreg_lm_1")
  rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")
  testthat::expect_equal(dim(rez), c(2, 19))
  rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(4, 19))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile", diagnostic = NULL, priors = FALSE)
  testthat::expect_equal(dim(rez), c(2, 4))

  # Brms
  library(brms)
  x <- insight::download_model("brms_mixed_1")
  # rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")  # doenst work because of BFs
  # testthat::expect_equal(dim(rez), c(4, 16))
  rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(4, 16))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile", diagnostic = NULL)
  testthat::expect_equal(dim(rez), c(2, 4))


  # BayesFactor
  # library(BayesFactor)
  # x <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
  # rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")
  # testthat::expect_equal(dim(rez), c(4, 16))
  # rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  # testthat::expect_equal(dim(rez), c(8, 16))
  # rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method="quantile")
  # testthat::expect_equal(dim(rez), c(4, 4))
})


if (require("insight")) {
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("describe_posterior", {
    testthat::expect_equal(
      describe_posterior(m, effects = "all")$Median,
      describe_posterior(p)$Median,
      tolerance = 1e-3
    )
  })

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("describe_posterior", {
    testthat::expect_equal(
      describe_posterior(m, effects = "all", component = "all")$Median,
      describe_posterior(p)$Median,
      tolerance = 1e-3
    )
  })
}