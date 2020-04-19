if (requireNamespace("BayesFactor", quietly = TRUE)) {
  library(BayesFactor)
  set.seed(123)

  test_that("weighted_posteriors for BayesFactor", {
    # compute Bayes Factor for 31 different regression models
    null_den <- regressionBF(mpg ~ cyl + disp + hp + drat + wt,
                             data = mtcars, progress = FALSE)
    wBF <- weighted_posteriors(null_den)

    testthat::expect_is(wBF, "data.frame")
    testthat::expect_equal(attr(wBF, "weights")$weights,
                           c(0, 13, 9, 0, 0, 55, 11, 4, 4, 1246, 6, 2, 38, 4, 946, 12, 3,
                             3, 209, 3, 491, 174, 4, 134, 7, 293, 1, 123, 35, 92, 51, 27))
  })

  test_that("weighted_posteriors for BayesFactor (intercept)", {
    dat <- data.frame(x1 = rnorm(10),
                      x2 = rnorm(10),
                      y = rnorm(10))
    BFmods <- regressionBF(y ~ x1 + x2, data = dat, progress = FALSE)

    res <- weighted_posteriors(BFmods)
    testthat::expect_equal(attr(res,"weights")$weights,c(1545, 805, 1020, 630))

    wHDI <- hdi(res[c("x1","x2")])
    testthat::expect_equal(wHDI$CI_low, c(-0.425,  -0.172), tol = 1e-3)
    testthat::expect_equal(wHDI$CI_high, c(0.371, 0.579), tol = 1e-3)

  })

  test_that("weighted_posteriors for nonlinear BayesFactor", {
    data(sleep)

    BFS <- ttestBF(x = sleep$extra[sleep$group == 1],
                   y = sleep$extra[sleep$group == 2],
                   nullInterval = c(-Inf,0),
                   paired = TRUE)

    res <- weighted_posteriors(BFS)

    testthat::expect_equal(attributes(res)$weights$weights, c(113, 3876, 11))
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