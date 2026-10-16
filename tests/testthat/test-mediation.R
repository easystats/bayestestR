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
  expected <- vapply(draws, stats::median, numeric(1))

  out_auto <- mediation(m)
  out_explicit <- mediation(m, treatment = "x", mediator = "m_a")

  for (out in list(out_auto, out_explicit)) {
    expect_equal(out$Estimate[1:4], unname(expected), tolerance = 1e-8)
    expect_identical(attr(out, "response"), "ycens")
  }
})


test_that("mediation, mo() mediator gives informative error", {
  m <- fit_brm(
    bf(mo_m ~ x, family = cumulative()) + bf(y ~ x + mo(mo_m)) + set_rescor(FALSE)
  )
  err_auto <- tryCatch(mediation(m), error = function(e) e)
  err_explicit <- tryCatch(
    mediation(m, treatment = "x", mediator = "mo_m"),
    error = function(e) e
  )

  for (err in list(err_auto, err_explicit)) {
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "mo()", fixed = TRUE)
    expect_match(conditionMessage(err), "mo_m(?![0-9])", perl = TRUE)
  }
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

d2 <- withr::with_seed(456, local({
  x <- rnorm(n)
  m_a <- 0.5 * x + rnorm(n)
  m_b <- 0.4 * x + 0.5 * m_a + rnorm(n)
  y <- 0.3 * x + 0.6 * m_a + 0.4 * m_b + rnorm(n)
  data.frame(x, m_a, m_b, y)
}))

# a factor treatment `g` and a logical mediator `mb`
d_fac <- withr::with_seed(789, local({
  g <- factor(sample(c("a", "b"), n, replace = TRUE))
  x <- rnorm(n)
  mb <- 0.8 * (g == "b") + 0.5 * x + rnorm(n) > 0
  m_a <- 0.5 * (g == "b") + rnorm(n)
  y2 <- 0.3 * (g == "b") + 0.6 * mb + 0.4 * m_a + rnorm(n)
  data.frame(g, x, mb, m_a, y2)
}))

# fit each model once, and only when a test needs it
.fits_d2 <- new.env()
fit_d2 <- function(type) {
  if (is.null(.fits_d2[[type]])) {
    f <- switch(
      type,
      par = bf(y ~ x + m_a + m_b) + bf(m_a ~ x) + bf(m_b ~ x) + set_rescor(FALSE),
      ser = bf(y ~ x + m_a + m_b) + bf(m_a ~ x) + bf(m_b ~ x + m_a) + set_rescor(FALSE),
      chain = bf(y ~ x + m_b) + bf(m_a ~ x) + bf(m_b ~ m_a) + set_rescor(FALSE),
      fac = bf(y2 ~ g + mb + m_a) +
        bf(mb ~ g + x, family = bernoulli()) +
        bf(m_a ~ g) +
        set_rescor(FALSE)
    )
    .fits_d2[[type]] <- skip_if_fit_failed(brms::brm(
      f,
      data = if (type == "fac") d_fac else d2,
      chains = 1,
      seed = 123,
      refresh = 0,
      silent = 2
    ))
  }
  .fits_d2[[type]]
}

# the detection rules on a model structure given by hand: the predictors of
# each formula, the coefficient names, and the model data, whose columns are
# the variables. By default, every predictor and response is a numeric column
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
  tryCatch(
    .mediation_paths_brms(NULL, treatment, mediator, response, params, predictors, data),
    error = function(e) e
  )
}

test_that("mediation, parallel mediators", {
  m <- fit_d2("par")
  p <- insight::get_parameters(m)
  ind_a <- p$b_ma_x * p$b_y_m_a
  ind_b <- p$b_mb_x * p$b_y_m_b

  out <- mediation(m)
  expect_identical(attr(out, "treatment"), "x")
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
  # proportion mediated: median total indirect effect / median total effect
  expect_equal(out$Estimate[8], expected[4] / expected[7], tolerance = 1e-8)

  draws <- list(
    p$b_y_x,
    ind_a,
    ind_b,
    ind_a + ind_b,
    p$b_y_m_a,
    p$b_y_m_b,
    p$b_y_x + (ind_a + ind_b)
  )
  expect_equal(
    out$CI_low[1:7],
    vapply(draws, stats::quantile, numeric(1), probs = 0.025, names = FALSE),
    tolerance = 1e-8
  )
  expect_equal(
    out$CI_high[1:7],
    vapply(draws, stats::quantile, numeric(1), probs = 0.975, names = FALSE),
    tolerance = 1e-8
  )

  # other centrality indices
  out_mean <- mediation(m, centrality = "mean")
  expect_equal(
    out_mean$Estimate[1:7],
    vapply(draws, mean, numeric(1)),
    tolerance = 1e-8
  )
  out_map <- mediation(m, centrality = "MAP")
  expect_equal(
    out_map$Estimate[1:7],
    vapply(draws, function(i) as.numeric(map_estimate(i)), numeric(1)),
    tolerance = 1e-8
  )

  # the detection rules on model structures where other rules would give
  # other answers (predictors given by hand)
  # the outcome is the response that no other formula uses, not the first
  # formula
  out <- .paths_brms(
    list("x", c("x", "m_a", "m_b"), "x"),
    c(ma = "m_a", y = "y", mb = "m_b")
  )
  expect_identical(out$outcome, "y")
  # mediators come in the order of the model formulas, not in alphabetical
  # order or in the order of the outcome's predictors
  out <- .paths_brms(
    list(c("x", "m_a", "m_b"), "x", "x"),
    c(y = "y", mb = "m_b", ma = "m_a")
  )
  expect_identical(out$mediator, c("m_b", "m_a"))
  # the treatment is the first suitable predictor of the outcome: "m" is a
  # response and "z" is in no mediator model, so "c" comes before "x"
  out <- .paths_brms(list(c("x", "c"), c("m", "z", "c", "x")), c(m = "m", y = "y"))
  expect_identical(out$treatment, "c")

  # a variable is matched by its predictor name only, never by a coefficient
  # name: "b_mb_w2" is level "2" of the factor "w", not the variable "w2"
  d_w <- data.frame(x = 0, w = factor(1:2), w2 = 0, m = 0, mb = 0, y = 0)
  for (med in list(NULL, c("w2", "mb"))) {
    out <- .paths_brms(
      list(c("x", "w2", "mb"), "x", c("x", "w")),
      c(y = "y", w2 = "w2", mb = "mb"),
      coefs = c("b_y_x", "b_y_w2", "b_y_mb", "b_w2_x", "b_mb_x", "b_mb_w2"),
      mediator = med,
      data = d_w
    )
    expect_identical(out$paths, list("w2", "mb"))
  }
  out <- .paths_brms(
    list(c("w2", "x", "m"), c("x", "w")),
    c(y = "y", m = "m"),
    coefs = c("b_y_w2", "b_y_x", "b_y_m", "b_m_x", "b_m_w2"),
    data = d_w
  )
  expect_identical(out$treatment, "x")
  out <- .paths_brms(
    list("x", c("x", "m")),
    c(m = "m", x2 = "x2"),
    coefs = c("b_m_x2", "b_x2_x2", "b_x2_m"),
    data = data.frame(x = factor(1:2), m = 0, x2 = 0)
  )
  expect_identical(out$outcome, "x2")
  expect_identical(out$treatment, "x2")
  expect_identical(out$paths, list("m"))
  # a coefficient named after a variable adds no path where that variable is
  # not a predictor: "b_mb_m_a" does not put "m_a" in the "mb" formula
  out <- .paths_brms(
    list(c("x", "m_a", "m_b"), "x", "x"),
    c(y = "y", ma = "m_a", mb = "m_b"),
    coefs = "b_mb_m_a",
    mediator = c("m_a", "m_b")
  )
  expect_identical(out$paths, list("m_a", "m_b"))

  # a treatment name that is not a variable is a coefficient name: "gb" is
  # level "b" of the factor "g"
  for (treat in list("gb", NULL)) {
    out <- .paths_brms(
      list(c("g", "m_a", "m_b"), "g", c("g", "m_a")),
      c(y2 = "y2", ma = "m_a", mb = "m_b"),
      coefs = c("b_y2_gb", "b_y2_m_a", "b_y2_m_b", "b_ma_gb", "b_mb_gb", "b_mb_m_a"),
      treatment = treat,
      data = data.frame(g = factor(c("a", "b")), m_a = 0, m_b = 0, y2 = 0)
    )
    expect_identical(out$treatment, "gb")
    expect_identical(out$paths, list("m_a", "m_b", c("m_a", "m_b")))
  }

  # the coefficient names that brms gives a factor treatment and a logical
  # mediator
  m_fac <- fit_d2("fac")
  p_fac <- insight::get_parameters(m_fac)
  out_fac <- suppressMessages(mediation(m_fac))
  expect_identical(attr(out_fac, "treatment"), "gb")
  expect_identical(attr(out_fac, "mediator"), c("mbTRUE", "m_a"))
  expect_equal(
    out_fac$Estimate[1:3],
    vapply(
      list(
        p_fac$b_y2_gb,
        p_fac$b_mb_gb * p_fac$b_y2_mbTRUE,
        p_fac$b_ma_gb * p_fac$b_y2_m_a
      ),
      stats::median,
      numeric(1)
    ),
    tolerance = 1e-8
  )
  out_gb <- suppressMessages(mediation(m_fac, treatment = "gb"))
  expect_identical(out_gb$Estimate, out_fac$Estimate)
})


test_that("mediation, serial mediators", {
  m <- fit_d2("ser")
  p <- insight::get_parameters(m)
  ind_a <- p$b_ma_x * p$b_y_m_a
  ind_b <- p$b_mb_x * p$b_y_m_b
  ind_ab <- p$b_ma_x * p$b_mb_m_a * p$b_y_m_b

  out <- mediation(m)
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
  expect_equal(
    out$Estimate[out$Effect == "Total Effect"],
    stats::median(p$b_y_x + ind_a + ind_b + ind_ab),
    tolerance = 1e-8
  )

  # each mediator must be a predictor in the outcome model: here, m_a is not
  m_chain <- fit_d2("chain")
  err <- tryCatch(mediation(m_chain), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find the coefficient", fixed = TRUE)
  expect_match(conditionMessage(err), "`b_y_m_a`", fixed = TRUE)
})


test_that("mediation, several mediators agree with lavaan", {
  skip_if_not_installed("lavaan")
  lavaan_models <- list(
    par = "
      y ~ c*x + b1*m_a + b2*m_b
      m_a ~ a1*x
      m_b ~ a2*x
      ind_a := a1*b1
      ind_b := a2*b2
      total := c + a1*b1 + a2*b2
    ",
    ser = "
      y ~ c*x + b1*m_a + b2*m_b
      m_a ~ a1*x
      m_b ~ a2*x + a12*m_a
      ind_a := a1*b1
      ind_b := a2*b2
      ind_ab := a1*a12*b2
      total := c + a1*b1 + a2*b2 + a1*a12*b2
    "
  )
  rows <- list(
    par = c(
      c = "Direct Effect (ADE)",
      ind_a = "Indirect Effect (ACME): m_a",
      ind_b = "Indirect Effect (ACME): m_b",
      total = "Total Effect"
    ),
    ser = c(
      c = "Direct Effect (ADE)",
      ind_a = "Indirect Effect (ACME): m_a",
      ind_b = "Indirect Effect (ACME): m_b",
      ind_ab = "Indirect Effect (ACME): m_a -> m_b",
      total = "Total Effect"
    )
  )

  for (type in c("par", "ser")) {
    pe <- lavaan::parameterEstimates(lavaan::sem(lavaan_models[[type]], data = d2))
    out <- mediation(fit_d2(type))
    for (label in names(rows[[type]])) {
      est_lavaan <- pe$est[pe$label == label][1]
      est_brms <- out$Estimate[out$Effect == rows[[type]][[label]]]
      expect_length(est_brms, 1)
      expect_lt(abs(est_brms - est_lavaan), 0.05)
    }
  }
})


test_that("mediation, subsets of mediators", {
  m <- fit_d2("par")
  p <- insight::get_parameters(m)
  out_auto <- mediation(m)

  out_both <- mediation(m, mediator = c("m_a", "m_b"))
  expect_identical(out_both$Effect, out_auto$Effect)
  expect_identical(out_both$Estimate, out_auto$Estimate)

  out_a <- mediation(m, mediator = "m_a")
  expect_identical(
    out_a$Effect,
    c(
      "Direct Effect (ADE)",
      "Indirect Effect (ACME)",
      "Mediator Effect",
      "Total Effect",
      "Proportion Mediated"
    )
  )
  ind_a <- p$b_ma_x * p$b_y_m_a
  expect_equal(out_a$Estimate[2], stats::median(ind_a), tolerance = 1e-8)
  expect_equal(out_a$Estimate[4], stats::median(p$b_y_x + ind_a), tolerance = 1e-8)
  expect_identical(
    colnames(as.data.frame(out_a)),
    c("effect_direct", "effect_indirect", "effect_mediator", "effect_total", "proportion_mediated")
  )

  # in the serial model, mediator = "m_a" keeps only the path through m_a
  # alone: m_a -> m_b goes through m_b, which is not named
  m_ser <- fit_d2("ser")
  p_ser <- insight::get_parameters(m_ser)
  out_ser_a <- mediation(m_ser, mediator = "m_a")
  ind_ser_a <- p_ser$b_ma_x * p_ser$b_y_m_a
  expect_identical(out_ser_a$Effect, out_a$Effect)
  expect_equal(out_ser_a$Estimate[2], stats::median(ind_ser_a), tolerance = 1e-8)
  expect_equal(
    out_ser_a$Estimate[4],
    stats::median(p_ser$b_y_x + ind_ser_a),
    tolerance = 1e-8
  )

  for (cent in list("all", c("median", "mean"))) {
    err <- tryCatch(mediation(m, centrality = cent), error = function(e) e)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "needs one centrality index", fixed = TRUE)
  }
  for (treat in list(c("x", "m_a"), character(0))) {
    err <- tryCatch(mediation(m, treatment = treat), error = function(e) e)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "must be the name of one variable", fixed = TRUE)
  }
  err <- tryCatch(mediation(m, mediator = character(0)), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "must name at least one variable", fixed = TRUE)

  err <- tryCatch(mediation(m, mediator = "x"), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "not a mediator", fixed = TRUE)

  err <- tryCatch(mediation(m, treatment = "w"), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find a mediator", fixed = TRUE)

  err <- tryCatch(
    mediation(m, treatment = "w", mediator = c("m_a", "m_b")),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find a path", fixed = TRUE)
})


test_that("mediation, draws of several mediators", {
  m <- fit_d2("par")
  p <- insight::get_parameters(m)
  out <- mediation(m)
  draws <- as.data.frame(out)

  expect_identical(colnames(draws), out$Effect)
  expect_equal(
    unname(vapply(draws[1:7], stats::median, numeric(1))),
    out$Estimate[1:7],
    tolerance = 1e-8
  )
  expect_equal(
    draws[["Indirect Effect (ACME): m_a"]],
    p$b_ma_x * p$b_y_m_a,
    tolerance = 1e-8
  )
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

  # insight does not find `d_grp` from inside the test and reads the data
  # from the model frame instead
  out <- withCallingHandlers(
    mediation(m),
    warning = function(w) {
      if (grepl("Could not recover model data", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_identical(
    out$Effect[1:4],
    c("Direct Effect (ADE)", "Indirect Effect (ACME)", "Mediator Effect", "Total Effect")
  )
  expect_equal(out$Estimate[1:4], expected, tolerance = 1e-8)

  # a mediator that is not a response
  for (med in c("zz", "x")) {
    err <- tryCatch(
      suppressWarnings(mediation(m, treatment = "x", mediator = med)),
      error = function(e) e
    )
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), sprintf("`%s` is not a mediator", med), fixed = TRUE)
  }

  # one mediator only
  err <- tryCatch(
    suppressWarnings(mediation(m, treatment = "x", mediator = c("m_a", "y"))),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "needs exactly one mediator", fixed = TRUE)

  # the input errors for all models
  for (cent in list("all", c("median", "mean"))) {
    err <- tryCatch(suppressWarnings(mediation(m, centrality = cent)), error = function(e) e)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "needs one centrality index", fixed = TRUE)
  }
  for (treat in list(c("x", "m_a"), character(0))) {
    err <- tryCatch(suppressWarnings(mediation(m, treatment = treat)), error = function(e) e)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "must be the name of one variable", fixed = TRUE)
  }
  err <- tryCatch(
    suppressWarnings(mediation(m, mediator = character(0))),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "must name at least one variable", fixed = TRUE)

  # more than two model formulas are not supported
  err <- tryCatch(
    .mediation_paths_default(m, NULL, NULL, c(y1 = "m1", y2 = "m2", y3 = "y")),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "two model formulas only", fixed = TRUE)
})


test_that("mediation, model structures with several mediators", {
  .paths <- .paths_brms

  err <- .paths(
    list(c("x", "m_a"), "x", "x"),
    c(y = "y", ma = "m_a", z = "z")
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find the outcome", fixed = TRUE)
  expect_match(conditionMessage(err), "2\\s+such\\s+responses:\\s+`y`,\\s+`z`")
  expect_match(conditionMessage(err), "use\\s+`response`")

  err <- .paths(list("m_a", "x"), c(y = "y", ma = "m_a"))
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find the treatment", fixed = TRUE)

  err <- .paths(
    list(c("x", "m_a", "mo_m"), "x", c("x", "m_a")),
    c(y = "y", ma = "m_a", mom = "mo_m"),
    coefs = "bsp_y_momo_m"
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "`mo_m` enters the outcome model", fixed = TRUE)

  err <- .paths(
    list(c("x", "m_b"), "x", c("x", "mo_m")),
    c(y = "y", mom = "mo_m", mb = "m_b"),
    coefs = "bsp_mb_momo_m"
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "`mo_m` enters the `m_b` model", fixed = TRUE)

  # a mo() term in the model of a mediator that is not chosen is no error
  out <- .paths(
    list(c("x", "m_a", "m_b"), "x", c("x", "m_a")),
    c(y = "y", ma = "m_a", mb = "m_b"),
    coefs = "bsp_mb_mom_a",
    mediator = "m_a"
  )
  expect_identical(out$paths, list("m_a"))

  # a named mediator on no path from the treatment to the outcome
  err <- .paths(
    list(c("x", "m_a", "z"), "x", "w"),
    c(y = "y", ma = "m_a", z = "z"),
    mediator = c("m_a", "z")
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find a path", fixed = TRUE)
  expect_match(conditionMessage(err), "through `z` to", fixed = TRUE)

  # the controls give paths, not errors
  serial <- list(c("x", "m_a", "m_b"), "x", c("x", "m_a"))
  serial_response <- c(y = "y", ma = "m_a", mb = "m_b")
  out <- .paths(serial, serial_response)
  expect_identical(out$paths, list("m_a", "m_b", c("m_a", "m_b")))
  out <- .paths(serial, serial_response, mediator = c("m_b", "m_a"))
  expect_identical(out$paths, list("m_b", "m_a", c("m_a", "m_b")))
  # a mediator that the treatment reaches only through another mediator
  out <- .paths(list(c("x", "m_b"), "x", "m_a"), serial_response)
  expect_identical(out$mediator, c("m_a", "m_b"))
  expect_identical(out$paths, list(c("m_a", "m_b")))

  # a logical mediator is found as the variable "mb" and named "mbTRUE" only
  # at the end
  d_mb <- data.frame(x = 0, mb = c(FALSE, TRUE), y = 0)
  out <- .paths(
    list(c("x", "mb"), "x"),
    c(y = "y", mb = "mb"),
    coefs = c("b_y_x", "b_y_mbTRUE", "b_mb_x"),
    data = d_mb
  )
  expect_identical(out$mediator, "mbTRUE")
  expect_identical(out$equation, c(mbTRUE = "mb"))

  # a `response` value can be the predictor name of a mediator
  out <- .paths(
    list("x", c("x", "m_center")),
    c(m = "m_center", y = "y"),
    coefs = c("b_m_x", "b_y_x", "b_y_m_center"),
    data = data.frame(x = 0, m = 0, m_center = 0, y = 0)
  )
  expect_identical(out$outcome, "y")
  expect_identical(out$equation, c(m_center = "m"))

  # a `response` value can also be a coefficient name, such as the level name
  # of a logical mediator, in either order of the outcome's predictors
  for (outcome_predictors in list(c("x", "mb"), c("mb", "x"))) {
    for (med in list("mbTRUE", NULL)) {
      out <- .paths(
        list(outcome_predictors, "x"),
        c(y = "y", mb = "mbTRUE"),
        coefs = c("b_y_x", "b_y_mbTRUE", "b_mb_x"),
        mediator = med,
        data = d_mb
      )
      expect_identical(out$outcome, "y")
      expect_identical(out$treatment, "x")
      expect_identical(out$paths, list("mbTRUE"))
    }
  }

  # a transformed mediator response is not its predictor name, so there are
  # two outcomes unless `response` gives the predictor name
  d_pos <- data.frame(x = 0, m_pos = 1, y = 0)
  err <- .paths(
    list("x", c("x", "m_pos")),
    c(mpos = "log(m_pos)", y = "y"),
    data = d_pos
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find the outcome", fixed = TRUE)
  expect_match(conditionMessage(err), "use\\s+`response`")
  out <- .paths(list("x", c("x", "m_pos")), c(mpos = "m_pos", y = "y"), data = d_pos)
  expect_identical(out$paths, list("m_pos"))
})


