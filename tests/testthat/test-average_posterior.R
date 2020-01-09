context("average_posterior")

test_that("average_posterior vs posterior_average", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  library(brms)
  fit1 <- brm(rating ~ treat + period + carry,
              data = inhaler,
              save_all_pars = TRUE)
  fit2 <- brm(rating ~ period + carry,
              data = inhaler,
              save_all_pars = TRUE)


  set.seed(444)
  res_BT <- average_posterior(fit1, fit2)

  set.seed(444)
  res_brms <- brms::posterior_average(fit1, fit2, weights = "marglik", missing = 0)
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
