context("describe_posterior")

test_that("describe_posterior", {
  set.seed(333)

  # Numeric
  x <- distribution_normal(1000)
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
  testthat::expect_equal(dim(rez), c(1, 19))
  testthat::expect_equal(colnames(rez), c("Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
                                          "CI_high", "p_map", "pd", "p_ROPE", "ps", "ROPE_CI", "ROPE_low",
                                          "ROPE_high", "ROPE_Percentage", "ROPE_Equivalence", "BF"))
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9)))
  testthat::expect_equal(dim(rez), c(2, 19))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
  testthat::expect_equal(dim(rez), c(1, 4))

  # Dataframes
  x <- data.frame(replicate(4, rnorm(100)))
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
  testthat::expect_equal(dim(rez), c(4, 19))
  rez <- testthat::expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9)))
  testthat::expect_equal(dim(rez), c(8, 19))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
  testthat::expect_equal(dim(rez), c(4, 4))

  # Rstanarm
  library(rstanarm)
  x <- insight::download_model("stanreg_lm_1")
  rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")
  testthat::expect_equal(dim(rez), c(2, 21))
  testthat::expect_equal(colnames(rez), c("Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
                                          "CI_high", "p_MAP", "pd", "p_ROPE", "ps", "ROPE_CI", "ROPE_low",
                                          "ROPE_high", "ROPE_Percentage", "ROPE_Equivalence", "BF", "Rhat",
                                          "ESS"))
  rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(4, 21))
  rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile", diagnostic = NULL, priors = FALSE)
  testthat::expect_equal(dim(rez), c(2, 4))

  # Brms
  library(brms)
  x <- insight::download_model("brms_mixed_1")
  rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(4, 16))
  testthat::expect_equal(colnames(rez), c("Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
                                          "CI_high", "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
                                          "ESS", "Rhat"))
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


test_that("describe_posterior w/ BF+SI", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  x <- insight::download_model("stanreg_lm_1")
  set.seed(555)
  rez <- describe_posterior(x, ci_method = "SI", test = "bf")


  # test si
  set.seed(555)
  rez_si <- si(x)
  testthat::expect_equal(rez$CI_low, rez_si$CI_low, tolerance = 0.1)
  testthat::expect_equal(rez$CI_high, rez_si$CI_high, tolerance = 0.1)

  # test BF
  set.seed(555)
  rez_bf <- bayesfactor_parameters(x)
  testthat::expect_equal(rez$BF, rez_bf$BF, tolerance = 0.1)
})