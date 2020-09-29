if (require("BayesFactor", quietly = TRUE)) {


  test_that("weighted_posteriors for BayesFactor", {
    set.seed(123)
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
    set.seed(123)
    # fails for win old-release
    testthat::skip_on_cran()
    testthat::skip_on_ci()

    dat <- data.frame(x1 = rnorm(10),
                      x2 = rnorm(10),
                      y = rnorm(10))
    BFmods <- regressionBF(y ~ x1 + x2, data = dat, progress = FALSE)

    res <- weighted_posteriors(BFmods)
    testthat::expect_equal(attr(res,"weights")$weights,c(1032, 805, 1388, 775))

    wHDI <- hdi(res[c("x1","x2")], ci = 0.9)
    testthat::expect_equal(wHDI$CI_low, c(-0.519, -0.640), tol = 1e-3)
    testthat::expect_equal(wHDI$CI_high, c(0.150, 0.059), tol = 1e-3)
  })

  test_that("weighted_posteriors for nonlinear BayesFactor", {
    set.seed(123)
    data(sleep)

    BFS <- ttestBF(x = sleep$extra[sleep$group == 1],
                   y = sleep$extra[sleep$group == 2],
                   nullInterval = c(-Inf,0),
                   paired = TRUE)

    res <- weighted_posteriors(BFS)

    testthat::expect_equal(attributes(res)$weights$weights, c(113, 3876, 11))
  })
}

.runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
if (.runThisTest) {
  if (require("brms", quietly = TRUE)) {
    test_that("weighted_posteriors vs posterior_average", {
      testthat::skip_on_cran()

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
}