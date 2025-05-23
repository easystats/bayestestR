skip_on_os("linux")

test_that("weighted_posteriors for BayesFactor", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(123)

  # compute Bayes Factor for 31 different regression models
  null_den <- regressionBF(
    mpg ~ cyl + disp + hp + drat + wt,
    data = mtcars,
    progress = FALSE
  )
  wBF <- weighted_posteriors(null_den)

  expect_s3_class(wBF, "data.frame")
  expect_equal(
    attr(wBF, "weights")$weights,
    c(
      0, 13, 9, 0, 0, 55, 11, 4, 4, 1246, 6, 2, 38, 4, 946, 12, 3,
      3, 209, 3, 491, 174, 4, 134, 7, 293, 1, 123, 35, 92, 51, 27
    ),
    ignore_attr = TRUE
  )
})

test_that("weighted_posteriors for BayesFactor (intercept)", {
  # fails for win old-release
  # skip_on_ci()
  skip_on_cran()
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(123)

  dat <- data.frame(
    x1 = rnorm(10),
    x2 = rnorm(10),
    y = rnorm(10)
  )
  BFmods <- regressionBF(y ~ x1 + x2, data = dat, progress = FALSE)

  res <- weighted_posteriors(BFmods)
  expect_equal(attr(res, "weights")$weights, c(1032, 805, 1388, 775), ignore_attr = TRUE)

  wHDI <- hdi(res[c("x1", "x2")], ci = 0.9)
  expect_equal(wHDI$CI_low, c(-0.519, -0.640), tolerance = 0.01)
  expect_equal(wHDI$CI_high, c(0.150, 0.059), tolerance = 0.01)
})

test_that("weighted_posteriors for nonlinear BayesFactor", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(123)
  data(sleep)

  BFS <- ttestBF(
    x = sleep$extra[sleep$group == 1],
    y = sleep$extra[sleep$group == 2],
    nullInterval = c(-Inf, 0),
    paired = TRUE
  )

  res <- weighted_posteriors(BFS)

  expect_equal(attributes(res)$weights$weights, c(113, 3876, 11), ignore_attr = TRUE)
})

test_that("weighted_posteriors vs posterior_average", {
  skip("Test creates error, must check why...")
  skip_on_cran()
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("brms")

  fit1 <- brm(rating ~ treat + period + carry,
    data = inhaler,
    refresh = 0,
    silent = TRUE,
    save_pars = save_pars(all = TRUE)
  )
  fit2 <- brm(rating ~ period + carry,
    data = inhaler,
    refresh = 0,
    silent = TRUE,
    save_pars = save_pars(all = TRUE)
  )

  set.seed(444)
  expect_warning({
    res_BT <- weighted_posteriors(fit1, fit2)
  })

  set.seed(444)
  res_brms <- brms::posterior_average(fit1, fit2, weights = "bma", missing = 0)
  res_brms <- res_brms[, 1:4]

  res_BT1 <- eti(res_BT)
  res_brms1 <- eti(res_brms)

  expect_equal(res_BT1$Parameter, res_brms1$Parameter, tolerance = 1e-4)
  expect_equal(res_BT1$CI, res_brms1$CI, tolerance = 1e-4)
  expect_equal(res_BT1$CI_low, res_brms1$CI_low, tolerance = 1e-4)
  expect_equal(res_BT1$CI_high, res_brms1$CI_high, tolerance = 1e-4)
})
