test_that("sensitivity_to_prior brms, per-coefficient priors", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  # wt has a negative effect, am and qsec positive ones
  set.seed(123)
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + am + qsec,
    data = mtcars,
    prior = c(
      brms::set_prior("normal(0, 2)", class = "b", coef = "wt"),
      brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "am"),
      brms::set_prior("cauchy(0, 0.5)", class = "b", coef = "qsec")
    ),
    chains = 1,
    iter = 1000,
    refresh = 0
  )))

  # record the prior handed to the refit, and fix the seed of the refit
  refit_brms <- .refit_brms
  captured <- NULL
  refit <- NULL
  local_mocked_bindings(.refit_brms = function(model, prior) {
    captured <<- prior
    set.seed(456)
    refit <<- refit_brms(model, prior)
    refit
  })

  out <- sensitivity_to_prior(model, index = c("Median", "MAP"), magnitude = 5)
  expect_s3_class(out, "data.frame")
  expect_identical(out$Parameter, c("b_wt", "b_am", "b_qsec"))
  expect_named(out, c("Parameter", "Sensitivity_Median", "Sensitivity_MAP"))

  # antagonistic location = -sign(Median) * magnitude * scale
  # wt:   -(-1) * 5 * 2   = 10
  # am:   -(+1) * 5 * 1   = -5
  # qsec: -(+1) * 5 * 0.5 = -2.5
  captured <- as.data.frame(captured)
  captured <- captured[order(captured$coef), ]
  expect_identical(captured$coef, c("am", "qsec", "wt"))
  expect_identical(captured$class, c("b", "b", "b"))
  expect_identical(
    captured$prior,
    c("student_t(3, -5, 1)", "cauchy(-2.5, 0.5)", "normal(10, 2)")
  )

  # sensitivity recomputed from an independent refit with the same priors and seed
  hand_priors <- c(
    brms::set_prior("normal(10, 2)", class = "b", coef = "wt"),
    brms::set_prior("student_t(3, -5, 1)", class = "b", coef = "am"),
    brms::set_prior("cauchy(-2.5, 0.5)", class = "b", coef = "qsec")
  )
  set.seed(456)
  own_refit <- suppressWarnings(suppressMessages(stats::update(model, prior = hand_priors, refresh = 0)))
  slopes <- c("b_wt", "b_am", "b_qsec")
  median_original <- apply(as.matrix(model)[, slopes], 2, stats::median)
  median_refit <- apply(as.matrix(own_refit)[, slopes], 2, stats::median)
  expected <- abs(median_refit - median_original) / abs(median_original)
  expect_equal(out$Sensitivity_Median, unname(expected), tolerance = 1e-8)

  # the refit really uses the shifted priors
  used <- as.data.frame(brms::prior_summary(refit))
  used <- used[used$class == "b" & used$coef %in% c("am", "qsec", "wt"), ]
  used <- used[order(used$coef), ]
  expect_identical(used$prior, c("student_t(3, -5, 1)", "cauchy(-2.5, 0.5)", "normal(10, 2)"))
  # ... and a refit that ignored them is caught
  expect_error(
    .brms_check_refit_priors(model, captured),
    "did not use the shifted priors for these slopes: am, qsec, wt"
  )

  # other indices, reusing the refit above
  local_mocked_bindings(.refit_brms = function(model, prior) refit)
  out <- sensitivity_to_prior(model, magnitude = 5)
  expect_identical(out$Parameter, slopes)
  expect_named(out, c("Parameter", "Sensitivity_Median"))
  out <- sensitivity_to_prior(model, index = "MAP", magnitude = 5)
  expect_identical(out$Parameter, slopes)
  expect_named(out, c("Parameter", "Sensitivity_Median", "Sensitivity_MAP"))
})


test_that("sensitivity_to_prior brms, class-level prior", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  set.seed(123)
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + cyl,
    data = mtcars,
    prior = brms::set_prior("normal(0, 5)", class = "b"),
    chains = 1,
    iter = 1000,
    refresh = 0
  )))

  captured <- NULL
  local_mocked_bindings(
    .refit_brms = function(model, prior) {
      captured <<- prior
      model
    },
    .brms_check_refit_priors = function(model, new_priors) invisible(NULL)
  )

  out <- sensitivity_to_prior(model, index = c("Median", "MAP"))
  expect_identical(out$Parameter, c("b_wt", "b_cyl"))
  expect_named(out, c("Parameter", "Sensitivity_Median", "Sensitivity_MAP"))

  # the class-level prior becomes one prior per slope; both slopes are negative
  captured <- as.data.frame(captured)
  captured <- captured[order(captured$coef), ]
  expect_identical(captured$coef, c("cyl", "wt"))
  expect_identical(captured$prior, c("normal(50, 5)", "normal(50, 5)"))
})


test_that("sensitivity_to_prior brms, unsupported priors", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  # wt keeps the flat default prior, am gets a lognormal prior
  set.seed(123)
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + am,
    data = mtcars,
    prior = brms::set_prior("lognormal(0, 1)", class = "b", coef = "am"),
    chains = 1,
    iter = 500,
    refresh = 0
  )))
  priors <- insight::get_priors(model)
  expect_identical(priors$Distribution[priors$Parameter == "b_wt"], "uniform")

  refitted <- FALSE
  local_mocked_bindings(.refit_brms = function(model, prior) {
    refitted <<- TRUE
    model
  })
  expect_error(sensitivity_to_prior(model), "b_wt.*b_am")
  expect_false(refitted)
})


test_that("sensitivity_to_prior brms, thresholds of ordinal models are not shifted", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  d <- mtcars
  d$y3 <- cut(d$mpg, 3, labels = FALSE)
  set.seed(123)
  model <- suppressWarnings(suppressMessages(brms::brm(
    y3 ~ wt,
    data = d,
    family = brms::cumulative(),
    prior = brms::set_prior("normal(0, 2)", class = "b"),
    chains = 1,
    iter = 500,
    refresh = 0
  )))

  captured <- NULL
  local_mocked_bindings(
    .refit_brms = function(model, prior) {
      captured <<- prior
      model
    },
    .brms_check_refit_priors = function(model, new_priors) invisible(NULL)
  )
  out <- sensitivity_to_prior(model)
  expect_identical(out$Parameter, "b_wt")
  expect_identical(as.data.frame(captured)$coef, "wt")
})


test_that("sensitivity_to_prior brms, parameters that are not regular slopes", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  # unfitted models; get_priors() is mocked because it needs posterior draws
  prior_rows <- function(parameters) {
    data.frame(
      Parameter = parameters,
      Distribution = "normal",
      Location = 0,
      Scale = 1,
      df = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  d <- mtcars
  d$g <- as.integer(d$gear)

  # no slopes
  model <- suppressWarnings(suppressMessages(brms::brm(mpg ~ 1, data = d, empty = TRUE)))
  local_mocked_bindings(get_priors = function(...) prior_rows("b_Intercept"), .package = "insight")
  expect_error(sensitivity_to_prior(model), "at least one slope")

  # a monotonic effect
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + mo(g),
    data = d,
    prior = brms::set_prior("normal(0, 1)", class = "b"),
    empty = TRUE
  )))
  local_mocked_bindings(
    get_priors = function(...) prior_rows(c("b_Intercept", "b_wt", "bsp_mog")),
    .package = "insight"
  )
  expect_error(sensitivity_to_prior(model), "not supported: bsp_mog")

  # a non-linear parameter
  model <- suppressWarnings(suppressMessages(brms::brm(
    brms::bf(mpg ~ a * wt, a ~ 1, nl = TRUE),
    data = d,
    prior = brms::set_prior("normal(0, 1)", nlpar = "a"),
    empty = TRUE
  )))
  local_mocked_bindings(get_priors = function(...) prior_rows("b_a_Intercept"), .package = "insight")
  expect_error(sensitivity_to_prior(model), "not supported: b_a_Intercept")

  # a bounded class-level prior
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + am,
    data = d,
    prior = brms::set_prior("normal(0, 1)", class = "b", lb = 0),
    empty = TRUE
  )))
  local_mocked_bindings(get_priors = function(...) prior_rows(c("b_Intercept", "b_wt", "b_am")), .package = "insight")
  expect_error(sensitivity_to_prior(model), "bounded priors: b_wt, b_am")
})


test_that("sensitivity_to_prior brms, multivariate", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  model <- suppressWarnings(suppressMessages(brms::brm(
    brms::mvbind(mpg, qsec) ~ wt,
    data = mtcars,
    empty = TRUE
  )))
  expect_error(sensitivity_to_prior(model), "Multivariate models are not supported")
})
