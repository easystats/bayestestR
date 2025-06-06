test_that("bayesfactor_parameters data frame", {
  skip_if_not_or_load_if_installed("logspline", "2.1.21")

  Xprior <- data.frame(
    x = distribution_normal(1e4),
    y = distribution_normal(1e4)
  )
  Xposterior <- data.frame(
    x = distribution_normal(1e4, mean = 0.5),
    y = distribution_normal(1e4, mean = -0.5)
  )

  # point
  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = 0, verbose = FALSE)
  expect_equal(bfsd$log_BF, c(0.12, 0.12), tolerance = 0.1)

  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = 1, verbose = FALSE)
  expect_equal(bfsd$log_BF, c(0.44, -0.35), tolerance = 0.1)

  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0, direction = -1, verbose = FALSE)
  expect_equal(bfsd$log_BF, c(-0.35, 0.44), tolerance = 0.1)

  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = 0.5, direction = 0, verbose = FALSE)
  expect_equal(bfsd$log_BF, c(-0.12, 0.37), tolerance = 0.1)

  expect_warning(bayesfactor_parameters(Xposterior, Xprior))

  w <- capture_warnings(bfsd <- bayesfactor_parameters(Xposterior))
  expect_match(w, "Prior", all = FALSE)
  expect_match(w, "40", all = FALSE)
  expect_equal(bfsd$log_BF, c(0, 0), tolerance = 0.1)

  # interval
  expect_warning(
    bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-0.1, 0.1), direction = 0),
    regexp = NA
  )
  expect_equal(bfsd$log_BF, c(0.13, 0.13), tolerance = 0.1)

  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-0.1, 0.1), direction = 1)
  expect_equal(bfsd$log_BF, c(0.47, -0.39), tolerance = 0.1)

  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-0.1, 0.1), direction = -1)
  expect_equal(bfsd$log_BF, c(-0.39, 0.47), tolerance = 0.1)

  # interval with inf
  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-0.1, Inf))
  expect_equal(bfsd$log_BF, c(-0.81, 0.80), tolerance = 0.1)

  bfsd <- bayesfactor_parameters(Xposterior, prior = Xprior, null = c(-Inf, 0.1))
  expect_equal(bfsd$log_BF, c(0.80, -0.81), tolerance = 0.1)
})


test_that("bayesfactor_parameters RSTANARM", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("logspline", "2.1.21")
  skip_if_not_or_load_if_installed("rstanarm")

  fit <- suppressMessages(stan_glm(mpg ~ ., data = mtcars, refresh = 0))

  set.seed(333)
  fit_p <- unupdate(fit, verbose = FALSE)
  expect_warning(BF2 <- bayesfactor_parameters(fit, fit_p))

  set.seed(333)
  BF1 <- bayesfactor_parameters(fit, verbose = FALSE)
  BF3 <- bayesfactor_parameters(insight::get_parameters(fit), insight::get_parameters(fit_p), verbose = FALSE)

  expect_equal(BF1, BF2)
  expect_equal(BF1[["Parameter"]], BF3[["Parameter"]])
  expect_equal(BF1[["log_BF"]], BF3[["log_BF"]])

  model_flat <- suppressMessages(
    stan_glm(extra ~ group, data = sleep, prior = NULL, refresh = 0)
  )
  suppressMessages(
    expect_error(bayesfactor_parameters(model_flat))
  )

  skip_on_ci()
  fit10 <- update(fit, chains = 10, iter = 5100, warmup = 100)
  suppressMessages(
    expect_warning(bayesfactor_parameters(fit10), regexp = NA)
  )
})


# bayesfactor_parameters BRMS ---------------------------------------------

test_that("bayesfactor_parameters BRMS", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("logspline", "2.1.21")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("cmdstanr")
  skip_if_not(dir.exists(cmdstanr::cmdstan_default_install_path()))

  brms_mixed_6 <- insight::download_model("brms_mixed_6")

  set.seed(222)
  brms_mixed_6_p <- unupdate(brms_mixed_6)
  bfsd1 <- suppressWarnings(bayesfactor_parameters(brms_mixed_6, brms_mixed_6_p, effects = "fixed"))

  set.seed(222)
  bfsd2 <- suppressWarnings(bayesfactor_parameters(brms_mixed_6, effects = "fixed"))

  expect_equal(bfsd1$log_BF, bfsd2$log_BF, tolerance = 0.11)


  brms_mixed_1 <- insight::download_model("brms_mixed_1")
  expect_error(bayesfactor_parameters(brms_mixed_1))
})
