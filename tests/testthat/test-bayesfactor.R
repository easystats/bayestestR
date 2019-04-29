context("bayesfactor_savagedickey")

test_that("bayesfactor_savagedickey", {
  set.seed(444)
  Xprior <- rnorm(1000)
  Xposterior <- rnorm(1000,0.7,0.2)

  bayesfactor_savagedickey <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = 0)
  testthat::expect_equal(as.numeric(bayesfactor_savagedickey), 39.6, tolerance = 0.1)

  bayesfactor_savagedickey <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = 1)
  testthat::expect_equal(as.numeric(bayesfactor_savagedickey), 76.8, tolerance = 0.1)

  bayesfactor_savagedickey <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 1, direction = 0)
  testthat::expect_equal(as.numeric(bayesfactor_savagedickey), 0.4, tolerance = 0.1)
})