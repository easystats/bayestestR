if (require("rstanarm") &&
    require("BayesFactor") &&
    require("testthat") &&
    require("brms")) {

  context("bayesfactor_parameters")

  # bayesfactor_parameters data frame ---------------------------------------

  test_that("bayesfactor_parameters data frame", {

    Xprior <- data.frame(x = distribution_normal(1e4),
                         y = distribution_normal(1e4))
    Xposterior <- data.frame(x = distribution_normal(1e4, mean = 0.5),
                             x = distribution_normal(1e4, mean = -0.5))

    # point
    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = 0)
    testthat::expect_equal(log(bfsd$BF), c(0.12,0.12), tolerance = 0.1)

    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = 1)
    testthat::expect_equal(log(bfsd$BF), c(0.44, -0.35), tolerance = 0.1)

    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = -1)
    testthat::expect_equal(log(bfsd$BF), c(-0.35, 0.44), tolerance = 0.1)

    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0.5, direction = 0)
    testthat::expect_equal(log(bfsd$BF), c(-0.12, 0.37), tolerance = 0.1)

    testthat::expect_warning(bfsd <- bayestestR::bayesfactor_parameters(Xposterior))
    testthat::expect_equal(log(bfsd$BF), c(0, 0), tolerance = 0.1)

    # interval
    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, .1), direction = 0)
    testthat::expect_equal(log(bfsd$BF), c(0.13, 0.13), tolerance = 0.1)

    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, .1), direction = 1)
    testthat::expect_equal(log(bfsd$BF), c(0.47, -0.39), tolerance = 0.1)

    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, .1), direction = -1)
    testthat::expect_equal(log(bfsd$BF), c(-0.39, 0.47), tolerance = 0.1)

    # interval with inf
    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, Inf))
    testthat::expect_equal(log(bfsd$BF), c(-0.81, 0.80), tolerance = 0.1)

    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-Inf, .1))
    testthat::expect_equal(log(bfsd$BF), c(0.80, -0.81), tolerance = 0.1)
  })


  # bayesfactor_parameters RSTANARM -----------------------------------------

  test_that("bayesfactor_parameters RSTANARM", {
    testthat::skip_on_cran()

    library(rstanarm)

    fit <- stan_glm(mpg ~ ., data = mtcars, refresh = 0)

    set.seed(333)
    fit_p <- update(fit, prior_PD = TRUE)
    BF2 <- bayesfactor_parameters(fit, fit_p)

    set.seed(333)
    BF1 <- bayesfactor_parameters(fit)

    testthat::expect_equal(BF1, BF2)

    model_flat <- stan_glm(extra ~ group, data = sleep, prior = NULL, refresh = 0)
    testthat::expect_error(bayesfactor_parameters(model_flat))
  })


  # bayesfactor_parameters BRMS ---------------------------------------------

  if (.runThisTest) {
    test_that("bayesfactor_parameters BRMS", {
      testthat::skip_on_cran()
      testthat::skip_on_travis()

      library(brms)
      brms_mixed_6 <- insight::download_model("brms_mixed_6")

      set.seed(222)
      brms_mixed_6_p <- update(brms_mixed_6, sample_prior = "only", refresh = 0)
      bfsd1 <- bayesfactor_parameters(brms_mixed_6, brms_mixed_6_p, effects = "fixed")

      set.seed(222)
      bfsd2 <- bayesfactor_parameters(brms_mixed_6, effects = "fixed")

      testthat::expect_equal(log(bfsd1$BF), log(bfsd2$BF))


      brms_mixed_1 <- insight::download_model("brms_mixed_1")
      testthat::expect_error(bayesfactor_parameters(brms_mixed_1))
    })
  }
}