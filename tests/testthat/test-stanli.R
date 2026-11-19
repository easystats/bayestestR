skip_on_cran()
skip_if_not_installed("stanli")
skip_if_not_installed("cmdstanr")
skip_if_not_installed("posterior")
skip_if_not_installed("bridgesampling")

bernoulli_data <- list(N = 10, y = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 1))

stan_file <- function(prior) {
  path <- tempfile(fileext = ".stan")
  writeLines(
    c(
      "data { int<lower=0> N; array[N] int<lower=0, upper=1> y; }",
      "parameters { real<lower=0, upper=1> theta; }",
      "model { theta ~ beta(%s); y ~ bernoulli(theta); }"
    ) |>
      sprintf(prior),
    path
  )
  path
}

fit_model <- function(prior) {
  mod <- stanli::stanli_model(file = stan_file(prior), data = bernoulli_data)
  stanli::sample_model(mod, chains = 2, refresh = 0, seed = 123)
}

test_that("stanli_fit: draws-based functions", {
  m <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  skip_if(is.null(m), "stanli model could not be compiled or sampled")

  draws <- stanli::as_draws_array(m)

  expect_equal(point_estimate(m)$Median, point_estimate(draws)$Median, ignore_attr = TRUE)
  expect_equal(hdi(m)$CI_low, hdi(draws)$CI_low, ignore_attr = TRUE)
  expect_equal(eti(m)$CI_high, eti(draws)$CI_high, ignore_attr = TRUE)
  expect_equal(bci(m)$CI_low, bci(draws)$CI_low, ignore_attr = TRUE)
  expect_equal(ci(m)$CI_high, ci(draws)$CI_high, ignore_attr = TRUE)
  expect_equal(map_estimate(m)$MAP_Estimate, map_estimate(draws)$MAP_Estimate, ignore_attr = TRUE)
  expect_equal(p_direction(m)$pd, p_direction(draws)$pd, ignore_attr = TRUE)
  expect_equal(p_map(m)$p_MAP, p_map(draws)$p_MAP, ignore_attr = TRUE)
  expect_equal(p_significance(m)$ps, p_significance(draws)$ps, ignore_attr = TRUE)
  expect_equal(
    rope(m, range = c(-0.1, 0.1))$ROPE_Percentage,
    rope(draws, range = c(-0.1, 0.1))$ROPE_Percentage,
    ignore_attr = TRUE
  )
  expect_equal(
    equivalence_test(m, range = c(-0.1, 0.1))$ROPE_Percentage,
    equivalence_test(draws, range = c(-0.1, 0.1))$ROPE_Percentage,
    ignore_attr = TRUE
  )
  expect_s3_class(estimate_density(m), "data.frame")
  expect_identical(describe_posterior(m)$Parameter, "theta")
  expect_equal(describe_posterior(m)$Median, describe_posterior(draws)$Median, ignore_attr = TRUE)
  expect_null(describe_prior(m))
})

test_that("stanli_fit: bayesfactor_parameters / restricted / si", {
  m <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  skip_if(is.null(m), "stanli model could not be compiled or sampled")
  prior <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  draws <- stanli::as_draws_array(m)
  draws_prior <- stanli::as_draws_array(prior)

  expect_equal(
    bayesfactor_parameters(m, prior = prior, null = 0.5, verbose = FALSE)$log_BF,
    bayesfactor_parameters(draws, prior = draws_prior, null = 0.5, verbose = FALSE)$log_BF,
    ignore_attr = TRUE
  )
  expect_equal(
    bayesfactor_restricted(m, hypothesis = "theta < 0.5", prior = prior)$log_BF,
    bayesfactor_restricted(draws, hypothesis = "theta < 0.5", prior = draws_prior)$log_BF,
    ignore_attr = TRUE
  )
  expect_equal(
    si(m, prior = prior, verbose = FALSE)$CI_low,
    si(draws, prior = draws_prior, verbose = FALSE)$CI_low,
    ignore_attr = TRUE
  )
})

test_that("stanli_fit: describe_posterior with bf_prior", {
  m <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  skip_if(is.null(m), "stanli model could not be compiled or sampled")
  prior <- fit_model("1, 1")
  draws <- stanli::as_draws_array(m)
  draws_prior <- stanli::as_draws_array(prior)

  out <- describe_posterior(m, bf_prior = prior, test = "bf", verbose = FALSE)
  expect_true("log_BF" %in% colnames(out))
  expect_equal(
    out$log_BF,
    describe_posterior(draws, bf_prior = draws_prior, test = "bf", verbose = FALSE)$log_BF,
    ignore_attr = TRUE
  )
})

test_that("stanli_fit: diagnostics", {
  m <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  skip_if(is.null(m), "stanli model could not be compiled or sampled")

  diag <- diagnostic_posterior(m)
  expect_identical(diag$Parameter, "theta")
  expect_named(diag, c("Parameter", "ESS_tail", "ESS_bulk", "Rhat", "MCSE"))
  expect_gt(diag$ESS_tail, 100)
  expect_lt(diag$Rhat, 1.05)
  expect_named(effective_sample(m), c("Parameter", "ESS_bulk", "ESS_tail"))
  expect_identical(effective_sample(m, parameters = "^th")$Parameter, "theta")
  expect_identical(nrow(effective_sample(m, parameters = "^zzz")), 0L)
  expect_named(mcse(m), c("Parameter", "MCSE"))
  summ <- posterior::summarize_draws(
    stanli::as_draws_array(m),
    ess_tail = posterior::ess_tail,
    rhat = posterior::rhat,
    mcse_mean = posterior::mcse_mean
  )
  expect_equal(diag$ESS_tail, round(summ$ess_tail), ignore_attr = TRUE)
  expect_equal(diag$Rhat, summ$rhat, ignore_attr = TRUE)
  expect_equal(
    mcse(m, centrality = "mean")$MCSE,
    summ$mcse_mean,
    ignore_attr = TRUE
  )
  expect_equal(mcse(m, "mean")$MCSE, summ$mcse_mean, ignore_attr = TRUE)
  expect_equal(
    diagnostic_posterior(m, diagnostic = "MCSE", centrality = "mean")$MCSE,
    summ$mcse_mean,
    ignore_attr = TRUE
  )
  expect_named(diagnostic_posterior(m, diagnostic = "ESS"), c("Parameter", "ESS_tail"))
  expect_identical(diagnostic_posterior(m, parameters = "^th")$Parameter, "theta")
  expect_identical(nrow(diagnostic_posterior(m, parameters = "^zzz")), 0L)
  expect_named(diagnostic_posterior(m, diagnostic = NULL), "Parameter")
})

test_that("stanli_fit: bayesfactor_models", {
  m1 <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  skip_if(is.null(m1), "stanli model could not be compiled or sampled")
  m2 <- fit_model("2, 2")

  expect_identical(insight::find_algorithm(m1)$chains, 2L)

  expect_warning(
    bf <- bayesfactor_models(m2, denominator = m1, verbose = TRUE),
    regexp = "precise"
  )
  expect_s3_class(bf, "bayesfactor_models")
  expect_identical(nrow(bf), 2L)
  expect_true(is.finite(bf$log_BF[1]))
  expect_equal(bf$log_BF[2], 0)
})

test_that("stanli_fit: get_parameters and weighted_posteriors", {
  m1 <- tryCatch(fit_model("1, 1"), error = function(e) NULL)
  skip_if(is.null(m1), "stanli model could not be compiled or sampled")
  m2 <- fit_model("2, 2")

  params <- insight::get_parameters(m1)
  expect_identical(colnames(params), "theta")
  expect_identical(nrow(params), 2000L)
  expect_identical(colnames(insight::get_parameters(m1, parameters = "^th")), "theta")

  suppressWarnings(
    wp <- weighted_posteriors(m1, m2, verbose = FALSE)
  )
  expect_s3_class(wp, "data.frame")
  expect_true("theta" %in% colnames(wp))
  expect_gt(nrow(wp), 0)
})
