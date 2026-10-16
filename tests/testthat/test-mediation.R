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

d2 <- withr::with_seed(456, {
  x <- rnorm(n)
  m_a <- 0.5 * x + rnorm(n)
  m_b <- 0.4 * x + 0.5 * m_a + rnorm(n)
  y <- 0.3 * x + 0.6 * m_a + 0.4 * m_b + rnorm(n)
  data.frame(x, m_a, m_b, y)
})

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


test_that("mediation, model structures with several mediators", {
  m <- fit_d2("par")
  # predictors and coefficient names of other model structures, given by
  # hand, with the variables of `m` for factor names
  .paths <- function(predictors, response, coefs = character(0), mediator = NULL) {
    params <- as.data.frame(stats::setNames(
      rep(list(numeric(0)), length(coefs)),
      coefs
    ))
    tryCatch(
      .mediation_paths_brms(m, NULL, mediator, response, params, predictors),
      error = function(e) e
    )
  }

  err <- .paths(
    list(c("x", "m_a"), "x", "x"),
    c(y = "y", ma = "m_a", z = "z")
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not find the outcome", fixed = TRUE)
  expect_match(conditionMessage(err), "2\\s+such\\s+responses:\\s+`y`,\\s+`z`")

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

  # the controls give paths, not errors
  serial <- list(c("x", "m_a", "m_b"), "x", c("x", "m_a"))
  serial_response <- c(y = "y", ma = "m_a", mb = "m_b")
  out <- .paths(serial, serial_response)
  expect_identical(out$paths, list("m_a", "m_b", c("m_a", "m_b")))
  out <- .paths(serial, serial_response, mediator = c("m_b", "m_a"))
  expect_identical(out$paths, list("m_b", "m_a", c("m_a", "m_b")))
  # a mediator that only the treatment reaches through another mediator
  out <- .paths(list(c("x", "m_b"), "x", "m_a"), serial_response)
  expect_identical(out$mediator, c("m_a", "m_b"))
  expect_identical(out$paths, list(c("m_a", "m_b")))
})


