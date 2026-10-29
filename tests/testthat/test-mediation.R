skip_on_cran()
skip_if_not_or_load_if_installed("brms")

withr::local_seed(123)
n <- 300
d <- data.frame(x = rnorm(n))
d$m_a <- 0.5 * d$x + rnorm(n)
d$y <- 0.3 * d$x + 0.6 * d$m_a + rnorm(n)
d$ycens <- pmin(d$y, 1)
d$cens <- d$y >= 1
d$mo_m <- factor(
  cut(d$m_a, breaks = c(-Inf, -0.5, 0.5, Inf), labels = FALSE),
  ordered = TRUE
)

fit_brm <- function(f) {
  skip_if_fit_failed(brms::brm(
    f,
    data = d,
    chains = 1,
    seed = 123,
    refresh = 0,
    silent = 2
  ))
}

# per-draw direct, indirect, mediator and total effects
.draws_effects <- function(model, outcome, mediator_resp, treatment, mediator) {
  p <- insight::get_parameters(model)
  direct <- p[[sprintf("b_%s_%s", outcome, treatment)]]
  med <- p[[sprintf("b_%s_%s", outcome, mediator)]]
  indirect <- p[[sprintf("b_%s_%s", mediator_resp, treatment)]] * med
  list(direct = direct, indirect = indirect, mediator = med, total = direct + indirect)
}


test_that("mediation, brms responses keep transformations, not addition terms", {
  m <- brms::brm(
    bf(log(m_pos) | cens(cens) ~ x) + bf(y ~ x + m_pos) + set_rescor(FALSE),
    data = transform(d, m_pos = exp(m_a)),
    empty = TRUE
  )
  out <- .mediation_response(m)
  expect_identical(unname(out), c("log(m_pos)", "y"))
  expect_identical(names(out), names(insight::find_formula(m)))
})


test_that("mediation, censored outcome", {
  m <- fit_brm(bf(ycens | cens(cens) ~ x + m_a) + bf(m_a ~ x) + set_rescor(FALSE))
  draws <- .draws_effects(m, "ycens", "ma", "x", "m_a")

  out <- mediation(m)
  expect_equal(
    out$Estimate[1:4],
    unname(vapply(draws, stats::median, numeric(1))),
    tolerance = 1e-8
  )
  expect_identical(attr(out, "response"), "ycens")
})


test_that("mediation, mo() mediator gives informative error", {
  m <- fit_brm(
    bf(mo_m ~ x, family = cumulative()) + bf(y ~ x + mo(mo_m)) + set_rescor(FALSE)
  )
  err <- tryCatch(mediation(m), error = function(e) e)
  expect_match(conditionMessage(err), "mo()", fixed = TRUE)
  expect_match(conditionMessage(err), "mo_m(?![0-9])", perl = TRUE)
})


test_that("mediation, two equations, and missing coefficients", {
  m <- fit_brm(bf(y ~ x + m_a) + bf(m_a ~ x) + set_rescor(FALSE))
  draws <- .draws_effects(m, "y", "ma", "x", "m_a")

  out <- mediation(m)
  expect_equal(
    out$Estimate[1:4],
    unname(vapply(draws, stats::median, numeric(1))),
    tolerance = 1e-8
  )
  expect_equal(
    out$CI_low[1:4],
    unname(vapply(draws, stats::quantile, numeric(1), probs = 0.025)),
    tolerance = 1e-8
  )
  expect_equal(
    out$CI_high[1:4],
    unname(vapply(draws, stats::quantile, numeric(1), probs = 0.975)),
    tolerance = 1e-8
  )

  # frequentist estimates on the same data
  b_y <- coef(lm(y ~ x + m_a, data = d))
  b_m <- coef(lm(m_a ~ x, data = d))
  expect_lt(abs(out$Estimate[1] - b_y[["x"]]), 0.05)
  expect_lt(abs(out$Estimate[3] - b_y[["m_a"]]), 0.05)
  expect_lt(abs(out$Estimate[2] - b_m[["x"]] * b_y[["m_a"]]), 0.05)

  err <- tryCatch(
    mediation(m, treatment = "w", mediator = "m_a"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "b_y_w", fixed = TRUE)
  expect_match(conditionMessage(err), "b_ma_w", fixed = TRUE)
})


# several mediators ---------------------------------

d2 <- withr::with_seed(
  456,
  local({
    x <- rnorm(n)
    m_a <- 0.5 * x + rnorm(n)
    m_b <- 0.4 * x + 0.5 * m_a + rnorm(n)
    y <- 0.3 * x + 0.6 * m_a + 0.4 * m_b + rnorm(n)
    data.frame(x, m_a, m_b, y)
  })
)

# fit each model once, and only when a test needs it
.fits_d2 <- new.env()
fit_d2 <- function(type) {
  if (is.null(.fits_d2[[type]])) {
    f <- switch(
      type,
      par = bf(y ~ x + m_a + m_b) + bf(m_a ~ x) + bf(m_b ~ x) + set_rescor(FALSE),
      ser = bf(y ~ x + m_a + m_b) + bf(m_a ~ x) + bf(m_b ~ x + m_a) + set_rescor(FALSE)
    )
    .fits_d2[[type]] <- skip_if_fit_failed(brms::brm(
      f,
      data = d2,
      chains = 1,
      seed = 123,
      refresh = 0,
      silent = 2
    ))
  }
  .fits_d2[[type]]
}

# model structure given by hand: predictors, coefficient names and data
.paths_brms <- function(
  predictors,
  response,
  coefs = character(0),
  treatment = NULL,
  mediator = NULL,
  data = NULL
) {
  if (is.null(data)) {
    vars <- unique(c(unlist(predictors), unname(response)))
    data <- as.data.frame(stats::setNames(rep(list(c(0, 1)), length(vars)), vars))
  }
  params <- as.data.frame(stats::setNames(
    rep(list(numeric(0)), length(coefs)),
    coefs
  ))
  .mediation_paths_brms(NULL, treatment, mediator, response, params, predictors, data)
}

test_that("mediation, parallel mediators", {
  m <- fit_d2("par")
  p <- insight::get_parameters(m)
  ind_a <- p$b_ma_x * p$b_y_m_a
  ind_b <- p$b_mb_x * p$b_y_m_b

  out <- mediation(m, treatment = "x", mediator = c("m_a", "m_b"))
  expect_identical(attr(out, "mediator"), c("m_a", "m_b"))
  expect_identical(
    out$Effect,
    c(
      "Direct Effect (ADE)",
      "Indirect Effect (ACME): m_a",
      "Indirect Effect (ACME): m_b",
      "Total Indirect Effect",
      "Mediator Effect: m_a",
      "Mediator Effect: m_b",
      "Total Effect",
      "Proportion Mediated"
    )
  )
  expected <- vapply(
    list(
      p$b_y_x,
      ind_a,
      ind_b,
      ind_a + ind_b,
      p$b_y_m_a,
      p$b_y_m_b,
      p$b_y_x + (ind_a + ind_b)
    ),
    stats::median,
    numeric(1)
  )
  expect_equal(out$Estimate[1:7], expected, tolerance = 1e-8)
  expect_equal(out$Estimate[8], expected[4] / expected[7], tolerance = 1e-8)

  draws <- as.data.frame(out)
  expect_identical(colnames(draws), out$Effect)
  expect_equal(draws[["Indirect Effect (ACME): m_a"]], ind_a, tolerance = 1e-8)
})


test_that("mediation, serial mediators", {
  m <- fit_d2("ser")
  p <- insight::get_parameters(m)
  ind_a <- p$b_ma_x * p$b_y_m_a
  ind_b <- p$b_mb_x * p$b_y_m_b
  ind_ab <- p$b_ma_x * p$b_mb_m_a * p$b_y_m_b

  out <- mediation(m, treatment = "x", mediator = c("m_a", "m_b"))
  ind_rows <- grep("^Indirect Effect", out$Effect)
  expect_identical(
    out$Effect[ind_rows],
    c(
      "Indirect Effect (ACME): m_a",
      "Indirect Effect (ACME): m_b",
      "Indirect Effect (ACME): m_a -> m_b"
    )
  )
  expect_equal(
    out$Estimate[ind_rows],
    vapply(list(ind_a, ind_b, ind_ab), stats::median, numeric(1)),
    tolerance = 1e-8
  )

  # m_a -> m_b goes through m_b, which is not named
  out_a <- mediation(m, treatment = "x", mediator = "m_a")
  expect_equal(out_a$Estimate[2], stats::median(ind_a), tolerance = 1e-8)
})


test_that("mediation, input errors for brms models", {
  m <- fit_d2("par")

  expect_error(
    mediation(m, centrality = "all"),
    "needs one centrality index",
    fixed = TRUE
  )
  expect_error(
    mediation(m, centrality = c("median", "mean")),
    "needs one centrality index",
    fixed = TRUE
  )
  expect_error(
    mediation(m, treatment = c("x", "m_a")),
    "must be the name of one variable",
    fixed = TRUE
  )
  expect_error(
    mediation(m, mediator = character(0)),
    "must name at least one variable",
    fixed = TRUE
  )
  expect_error(mediation(m), "specify both", fixed = TRUE)
  expect_error(mediation(m, treatment = "x"), "specify both", fixed = TRUE)
  expect_error(
    mediation(m, treatment = "x", mediator = "x"),
    "not a mediator",
    fixed = TRUE
  )
  expect_error(
    mediation(m, treatment = "w", mediator = c("m_a", "m_b")),
    "Could not find a path",
    fixed = TRUE
  )

  # `response` is named by formula name ("ma"), not by response ("m_a")
  expect_error(
    mediation(
      m,
      treatment = "x",
      mediator = c("m_a", "m_b"),
      response = c(y = "y", m_a = "m_a", m_b = "m_b")
    ),
    "names of `response`.*`y`,\\s+`ma`,\\s+`mb`"
  )
  expect_error(
    mediation(m, response = c(y = "y", ma = "m_a")),
    "names of `response`",
    fixed = TRUE
  )

  # each mediator needs a coefficient in the outcome model
  p <- insight::get_parameters(m)
  local_mocked_bindings(
    get_parameters = function(...) p[setdiff(colnames(p), "b_y_m_a")],
    .package = "insight"
  )
  expect_error(
    mediation(m, treatment = "x", mediator = c("m_a", "m_b")),
    "`b_y_m_a`",
    fixed = TRUE
  )

  m1 <- brms::brm(bf(y ~ x + m_a), data = d, empty = TRUE)
  expect_error(mediation(m1), "needs a multivariate model", fixed = TRUE)
})


test_that("mediation, stanmvreg with two equations", {
  skip_if_not_installed("rstanarm")
  d_grp <- transform(d, grp = factor(rep(1:10, length.out = n)))
  m <- skip_if_fit_failed(rstanarm::stan_mvmer(
    list(m_a ~ x + (1 | grp), y ~ x + m_a + (1 | grp)),
    data = d_grp,
    chains = 1,
    iter = 1000,
    seed = 123,
    refresh = 0
  ))
  p <- insight::get_parameters(m)
  indirect <- p[["y1|x"]] * p[["y2|m_a"]]
  expected <- c(
    stats::median(p[["y2|x"]]),
    stats::median(indirect),
    stats::median(p[["y2|m_a"]]),
    stats::median(p[["y2|x"]] + indirect)
  )

  out <- suppressWarnings(mediation(m))
  expect_equal(out$Estimate[1:4], expected, tolerance = 1e-8)

  expect_error(
    suppressWarnings(mediation(m, treatment = "x", mediator = "zz")),
    "`zz` is not a mediator",
    fixed = TRUE
  )
  expect_error(
    suppressWarnings(mediation(m, treatment = "x", mediator = c("m_a", "y"))),
    "needs\\s+exactly\\s+one\\s+mediator"
  )
  expect_error(
    .mediation_paths_default(m, NULL, NULL, c(y1 = "m1", y2 = "m2", y3 = "y")),
    "two model formulas only",
    fixed = TRUE
  )
})


test_that("mediation, model structures with several mediators", {
  .paths <- .paths_brms

  # the outcome is the response that no other formula uses, not the first one
  out <- .paths(
    list("x", c("x", "m_a", "m_b"), "x"),
    c(ma = "m_a", y = "y", mb = "m_b"),
    treatment = "x",
    mediator = c("m_a", "m_b")
  )
  expect_identical(out$outcome, "y")
  expect_error(
    .paths(
      list(c("x", "m_a"), "x", "x"),
      c(y = "y", ma = "m_a", z = "z"),
      treatment = "x",
      mediator = "m_a"
    ),
    "2\\s+such\\s+responses:\\s+`y`,\\s+`z`"
  )

  # a variable is matched by its predictor name only: "b_mb_w2" is level "2"
  # of the factor "w", not the variable "w2"
  out <- .paths(
    list(c("x", "w2", "mb"), "x", c("x", "w")),
    c(y = "y", w2 = "w2", mb = "mb"),
    coefs = c("b_y_x", "b_y_w2", "b_y_mb", "b_w2_x", "b_mb_x", "b_mb_w2"),
    treatment = "x",
    mediator = c("w2", "mb"),
    data = data.frame(x = 0, w = factor(1:2), w2 = 0, m = 0, mb = 0, y = 0)
  )
  expect_identical(out$paths, list("w2", "mb"))

  # a name that is not a variable is a coefficient name: "gb" is level "b" of
  # the factor "g"
  out <- .paths(
    list(c("g", "m_a", "m_b"), "g", c("g", "m_a")),
    c(y2 = "y2", ma = "m_a", mb = "m_b"),
    coefs = c("b_y2_gb", "b_y2_m_a", "b_y2_m_b", "b_ma_gb", "b_mb_gb", "b_mb_m_a"),
    treatment = "gb",
    mediator = c("m_a", "m_b"),
    data = data.frame(g = factor(c("a", "b")), m_a = 0, m_b = 0, y2 = 0)
  )
  expect_identical(out$paths, list("m_a", "m_b", c("m_a", "m_b")))

  # mo() terms
  expect_error(
    .paths(
      list(c("x", "m_a", "mo_m"), "x", c("x", "m_a")),
      c(y = "y", ma = "m_a", mom = "mo_m"),
      coefs = "bsp_y_momo_m",
      treatment = "x",
      mediator = c("m_a", "mo_m")
    ),
    "`mo_m` enters the outcome model",
    fixed = TRUE
  )
  expect_error(
    .paths(
      list(c("x", "m_b"), "x", c("x", "mo_m")),
      c(y = "y", mom = "mo_m", mb = "m_b"),
      coefs = "bsp_mb_momo_m",
      treatment = "x",
      mediator = c("mo_m", "m_b")
    ),
    "`mo_m` enters the `m_b` model",
    fixed = TRUE
  )
  out <- .paths(
    list(c("x", "m_a", "m_b"), "x", c("x", "m_a")),
    c(y = "y", ma = "m_a", mb = "m_b"),
    coefs = "bsp_mb_mom_a",
    treatment = "x",
    mediator = "m_a"
  )
  expect_identical(out$paths, list("m_a"))

  # a variable that is not a predictor, but shares a coefficient name
  expect_error(
    .paths(
      list(c("w", "m_a", "m_b"), "w2", "w2"),
      c(y = "y", ma = "m_a", mb = "m_b"),
      coefs = c("b_y_w2", "b_y_m_a", "b_y_m_b", "b_ma_w2", "b_mb_w2"),
      treatment = "w2",
      mediator = c("m_a", "m_b"),
      data = data.frame(w = factor(1:2), w2 = 0, m_a = 0, m_b = 0, y = 0)
    ),
    "(?s)`w2`\\s+is\\s+not\\s+a\\s+predictor.*`b_y_w2`",
    perl = TRUE
  )
  expect_error(
    .paths(
      list(c("w2", "m", "z"), "w", "w2"),
      c(y = "y", m = "m", z = "z"),
      coefs = c("b_y_w2", "b_y_m", "b_y_z", "b_m_w2", "b_z_w2"),
      treatment = "w2",
      mediator = "m",
      data = data.frame(w = factor(1:2), w2 = 0, m = 0, z = 0, y = 0)
    ),
    "(?s)`w2`\\s+is\\s+not\\s+a\\s+predictor.*`b_m_w2`",
    perl = TRUE
  )

  # paths
  serial_response <- c(y = "y", ma = "m_a", mb = "m_b")
  out <- .paths(
    list(c("x", "m_a", "m_b"), "x", c("x", "m_a")),
    serial_response,
    treatment = "x",
    mediator = c("m_b", "m_a")
  )
  expect_identical(out$paths, list("m_b", "m_a", c("m_a", "m_b")))
  out <- .paths(
    list(c("x", "m_b"), "x", "m_a"),
    serial_response,
    treatment = "x",
    mediator = c("m_a", "m_b")
  )
  expect_identical(out$paths, list(c("m_a", "m_b")))
  expect_error(
    .paths(
      list(c("x", "m_a", "z"), "x", "w"),
      c(y = "y", ma = "m_a", z = "z"),
      treatment = "x",
      mediator = c("m_a", "z")
    ),
    "through `z` to",
    fixed = TRUE
  )
})
