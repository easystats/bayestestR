if (requireNamespace("BayesFactor", quietly = TRUE)) {
  test_that("weighted_posteriors for BayesFactor", {

    library(BayesFactor)
    # compute Bayes Factor for 31 different regression models
    null_den <- regressionBF(mpg ~ cyl + disp + hp + drat + wt,
                             data = mtcars, progress = FALSE)
    no_null <- null_den[-1]/null_den[1]
    null_not_den <- c(no_null, 1/null_den[1])

    testthat::expect_warning(weighted_posteriors(null_not_den))
    testthat::expect_warning(weighted_posteriors(null_den))
    testthat::expect_is(weighted_posteriors(no_null), "data.frame")
  })
}


if (requireNamespace("brms", quietly = TRUE)) {
  context("weighted_posteriors")

  test_that("weighted_posteriors vs posterior_average", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()

    library(brms)
    fit1 <- brm(rating ~ treat + period + carry,
                data = inhaler,
                refresh = 0,
                save_all_pars = TRUE)
    fit2 <- brm(rating ~ period + carry,
                data = inhaler,
                refresh = 0,
                save_all_pars = TRUE)


    set.seed(444)
    res_BT <- weighted_posteriors(fit1, fit2)

    set.seed(444)
    res_brms <- brms::posterior_average(fit1, fit2, weights = "bma", missing = 0)
    res_brms <- res_brms[, 1:4]

    res_BT1 <- eti(res_BT)
    res_brms1 <- eti(res_brms)

    testthat::expect_equal(res_BT1$Parameter, res_brms1$Parameter)
    testthat::expect_equal(res_BT1$CI, res_brms1$CI)
    testthat::expect_equal(res_BT1$CI_low, res_brms1$CI_low)
    testthat::expect_equal(res_BT1$CI_high, res_brms1$CI_high)

    # plot(res_brms1)
    # plot(res_BT1)
  })
}