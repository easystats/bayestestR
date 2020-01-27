if (requireNamespace("rstanarm", quietly = TRUE) &&
    require("BayesFactor") &&
    require("testthat") &&
    requireNamespace("brms", quietly = TRUE)) {
  context("bayesfactor_parameters")

  test_that("bayesfactor_parameters numeric", {
    testthat::skip_on_cran()

    set.seed(444)
    Xprior <- rnorm(1000)
    Xposterior <- rnorm(1000, 0.7, 0.2)

    # point
    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = 0)
    testthat::expect_equal(log(bfsd$BF), 3.7, tolerance = 0.1)

    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = 1)
    testthat::expect_equal(log(bfsd$BF), 4.3, tolerance = 0.1)

    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = -1)
    testthat::expect_equal(log(bfsd$BF), -2.5, tolerance = 0.1)

    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = 1, direction = 0)
    testthat::expect_equal(log(bfsd$BF), -0.84, tolerance = 0.1)

    testthat::expect_warning(bfsd <- bayestestR::bayesfactor_parameters(Xposterior))
    testthat::expect_equal(log(bfsd$BF), 0, tolerance = 0.1)

    # interval
    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, .1), direction = 0)
    testthat::expect_equal(log(bfsd$BF), 3.7, tolerance = 0.1)

    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, .1), direction = 1)
    testthat::expect_equal(log(bfsd$BF), 4.3, tolerance = 0.1)

    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, .1), direction = -1)
    testthat::expect_equal(log(bfsd$BF), -3.88, tolerance = 0.1)

    # interval with inf
    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-.1, Inf))
    testthat::expect_equal(log(bfsd$BF), -7.94, tolerance = 0.1)

    bfsd <- bayestestR::bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-Inf, .1))
    testthat::expect_equal(log(bfsd$BF), 5.97, tolerance = 0.1)
  })

  test_that("bayesfactor_parameters RSTANARM", {
    testthat::skip_on_cran()

    library(rstanarm)
    set.seed(333)
    model <- stan_glm(extra ~ group, data = sleep, refresh = 0)
    bfsd <- bayestestR::bayesfactor_parameters(model)
    testthat::expect_equal(log(bfsd$BF), c(-2.69, -0.14), tolerance = 0.2)

    bfsd <- bayestestR::bayesfactor_parameters(model, null = rope_range(model))
    testthat::expect_equal(log(bfsd$BF), c(-2.96, -0.18), tolerance = 0.2)

    model_p <- update(model, prior_PD = TRUE, refresh = 0)
    bfsd <- bayestestR::bayesfactor_parameters(model, model_p)
    testthat::expect_equal(log(bfsd$BF), c(-2.69, -0.14), tolerance = 0.2)

    model_flat <- stan_glm(extra ~ group, data = sleep, prior = NULL, refresh = 0)
    testthat::expect_error(bayesfactor_parameters(model_flat))
  })

  test_that("bayesfactor_parameters BRMS", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()

    library(brms)
    brms_mixed_6 <- insight::download_model("brms_mixed_6")
    set.seed(222)
    bfsd <- bayestestR::bayesfactor_parameters(brms_mixed_6, effects = "fixed")
    testthat::expect_equal(log(bfsd$BF), c(-6.0, -5.8, 0.7, -2.7, -7.4), tolerance = 0.2)

    bfsd <- bayestestR::bayesfactor_parameters(brms_mixed_6, null = rope_range(brms_mixed_6))
    testthat::expect_equal(log(bfsd$BF), c(-6.33, -12.8, -36.48, -2.6, -29.88), tolerance = 0.2)

    brms_mixed_1 <- insight::download_model("brms_mixed_1")
    testthat::expect_error(bayesfactor_parameters(brms_mixed_1))
  })
}