context("bayesfactor_savagedickey")

test_that("bayesfactor_savagedickey", {
  testthat::skip_on_cran()

  set.seed(444)
  Xprior <- rnorm(1000)
  Xposterior <- rnorm(1000, 0.7, 0.2)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = 0)
  testthat::expect_equal(log(bfsd$BF), 3.7, tolerance = 0.1)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = 1)
  testthat::expect_equal(log(bfsd$BF), 4.3, tolerance = 0.1)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = -1)
  testthat::expect_equal(log(bfsd$BF), -2.5, tolerance = 0.1)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 1, direction = 0)
  testthat::expect_equal(log(bfsd$BF), -0.8, tolerance = 0.1)

  testthat::expect_warning(bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior))
  testthat::expect_equal(log(bfsd$BF), 0, tolerance = 0.1)

  library(rstanarm)
  set.seed(333)
  junk <- capture.output(model <- stan_glm(extra ~ group, data = sleep))
  bfsd <- bayestestR::bayesfactor_savagedickey(model)
  testthat::expect_equal(log(bfsd$BF), c(-2.69, -0.14), tolerance = 0.2)

  # SKIP FOR TRAVIS
  # library(brms)
  # brms_mixed_6 <- insight::download_model("brms_mixed_6")
  # set.seed(222)
  # bfsd <- bayesfactor_savagedickey(brms_mixed_6, effects = "fixed")
  # testthat::expect_equal(log(bfsd$BF), c(-6.0, -5.8, 0.7, -2.7, -7.4), tolerance = 0.2)
  #
  # brms_mixed_1 <- insight::download_model("brms_mixed_1")
  # testthat::expect_error(bayesfactor_savagedickey(brms_mixed_1))
})