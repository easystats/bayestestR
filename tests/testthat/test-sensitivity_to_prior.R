# unfitted models need get_priors() mocked, because it needs posterior draws
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


test_that("sensitivity_to_prior brms, shifted priors", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  # wt has the class-level prior and a negative effect, am and qsec positive ones
  set.seed(123)
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + am + qsec,
    data = mtcars,
    prior = c(
      brms::set_prior("normal(0, 2)", class = "b"),
      brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "am"),
      brms::set_prior("cauchy(0, 0.5)", class = "b", coef = "qsec")
    ),
    chains = 1,
    iter = 1000,
    refresh = 0
  )))

  captured <- NULL
  out <- with_mocked_bindings(
    sensitivity_to_prior(model, index = c("Median", "MAP"), magnitude = 5),
    .refit_brms = function(model, prior) {
      captured <<- prior
      model
    },
    .brms_check_refit_priors = function(model, new_priors) invisible(NULL)
  )
  expect_identical(out$Parameter, c("b_wt", "b_am", "b_qsec"))
  expect_named(out, c("Parameter", "Sensitivity_Median", "Sensitivity_MAP"))

  # location = -sign(Median) * magnitude * scale
  captured <- as.data.frame(captured)
  captured <- captured[order(captured$coef), ]
  expect_identical(captured$coef, c("am", "qsec", "wt"))
  expect_identical(
    captured$prior,
    c("student_t(3, -5, 1)", "cauchy(-2.5, 0.5)", "normal(10, 2)")
  )

  expect_error(
    .brms_check_refit_priors(model, captured),
    "did not use the shifted priors for these slopes: am, qsec, wt"
  )
})


test_that("sensitivity_to_prior brms, refit silences only the unused prior warning", {
  local_mocked_bindings(
    update = function(...) {
      warning("Prior 'normal(0, 2)' will not be used in the model.")
      warning("other")
    },
    .package = "stats"
  )
  expect_identical(capture_warnings(.refit_brms(NULL, NULL)), "other")
})


test_that("sensitivity_to_prior brms, thresholds of ordinal models are not shifted", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  d <- mtcars
  d$y3 <- cut(d$mpg, 3, labels = FALSE)
  model <- suppressWarnings(suppressMessages(brms::brm(
    y3 ~ wt,
    data = d,
    family = brms::cumulative(),
    prior = brms::set_prior("normal(0, 2)", class = "b"),
    empty = TRUE
  )))
  local_mocked_bindings(
    get_priors = function(...) prior_rows(c("b_Intercept[1]", "b_Intercept[2]", "b_wt")),
    .package = "insight"
  )
  local_mocked_bindings(
    .extract_parameters = function(...) data.frame(Parameter = "b_wt", Median = -1),
    .refit_brms = function(model, prior) model,
    .brms_check_refit_priors = function(model, new_priors) invisible(NULL)
  )
  expect_identical(sensitivity_to_prior(model)$Parameter, "b_wt")
})


test_that("sensitivity_to_prior brms, unsupported parameters and priors", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  d <- mtcars
  d$g <- as.integer(d$gear)

  # no slopes
  model <- suppressWarnings(suppressMessages(brms::brm(mpg ~ 1, data = d, empty = TRUE)))
  local_mocked_bindings(
    get_priors = function(...) prior_rows("b_Intercept"),
    .package = "insight"
  )
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
  local_mocked_bindings(
    get_priors = function(...) prior_rows("b_a_Intercept"),
    .package = "insight"
  )
  expect_error(sensitivity_to_prior(model), "not supported: b_a_Intercept")

  # a bounded class-level prior
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + am,
    data = d,
    prior = brms::set_prior("normal(0, 1)", class = "b", lb = 0),
    empty = TRUE
  )))
  local_mocked_bindings(
    get_priors = function(...) prior_rows(c("b_Intercept", "b_wt", "b_am")),
    .package = "insight"
  )
  expect_error(sensitivity_to_prior(model), "bounded priors: b_wt, b_am")

  # a flat prior and a lognormal prior
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + am,
    data = d,
    prior = brms::set_prior("lognormal(0, 1)", class = "b", coef = "am"),
    empty = TRUE
  )))
  priors <- prior_rows(c("b_wt", "b_am"))
  priors$Distribution <- c("uniform", "lognormal")
  local_mocked_bindings(get_priors = function(...) priors, .package = "insight")
  expect_error(sensitivity_to_prior(model), "flat or other priors: b_wt, b_am")

  # a slope that insight does not report, such as a predictor named "sigma_x"
  d$sigma_x <- d$qsec
  model <- suppressWarnings(suppressMessages(brms::brm(
    mpg ~ wt + sigma_x,
    data = d,
    prior = brms::set_prior("normal(0, 1)", class = "b"),
    empty = TRUE
  )))
  local_mocked_bindings(
    get_priors = function(...) prior_rows(c("b_Intercept", "b_wt")),
    .package = "insight"
  )
  expect_error(
    sensitivity_to_prior(model),
    "Could not find the priors of these slopes: sigma_x"
  )

  # multivariate models
  model <- suppressWarnings(suppressMessages(brms::brm(
    brms::mvbind(mpg, qsec) ~ wt,
    data = mtcars,
    empty = TRUE
  )))
  expect_error(sensitivity_to_prior(model), "Multivariate models are not supported")
})
