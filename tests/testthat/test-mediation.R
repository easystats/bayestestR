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
