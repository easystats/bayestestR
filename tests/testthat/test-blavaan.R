test_that("blavaan, all", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("blavaan")
  skip_if_not_or_load_if_installed("lavaan")
  skip_if_not_or_load_if_installed("cmdstanr")
  skip_if_not_or_load_if_installed("rstan")

  data("PoliticalDemocracy", package = "lavaan")

  model <- "
    # latent variable definitions
    dem60 =~ y1 + a*y2
    dem65 =~ y5 + a*y6

    # regressions
    dem65 ~ dem60

    # residual correlations
    y1 ~~ y5
  "

  model2 <- "
    # latent variable definitions
    dem60 =~ y1 + a*y2
    dem65 =~ y5 + a*y6

    # regressions
    dem65 ~ 0*dem60

    # residual correlations
    y1 ~~ 0*y5
  "
  suppressWarnings(capture.output({
    bfit <- blavaan::bsem(model,
                          data = PoliticalDemocracy,
                          n.chains = 1, burnin = 50, sample = 100
    )
    bfit2 <- blavaan::bsem(model2,
                           data = PoliticalDemocracy,
                           n.chains = 1, burnin = 50, sample = 100
    )
  }))

  x <- point_estimate(bfit, centrality = "all", dispersion = TRUE)
  expect_true(all(c("Median", "MAD", "Mean", "SD", "MAP", "Component") %in% colnames(x)))
  expect_equal(nrow(x), 14)

  x <- eti(bfit)
  expect_equal(nrow(x), 14)

  x <- hdi(bfit)
  expect_equal(nrow(x), 14)

  x <- p_direction(bfit)
  expect_equal(nrow(x), 14)

  x <- rope(bfit, range = c(-.1, .1))
  expect_equal(nrow(x), 14)

  x <- p_rope(bfit, range = c(-.1, .1))
  expect_equal(nrow(x), 14)

  x <- p_map(bfit)
  expect_equal(nrow(x), 14)

  x <- p_significance(bfit, threshold = c(-.1, .1))
  expect_equal(nrow(x), 14)

  x <- equivalence_test(bfit, range = c(-.1, .1))
  expect_equal(nrow(x), 14)

  x <- estimate_density(bfit)
  expect_equal(length(unique(x$Parameter)), 14)


  ## Bayes factors ----
  expect_warning(bayesfactor_models(bfit, bfit2))
  x <- suppressWarnings(bayesfactor_models(bfit, bfit2))
  expect_true(x$log_BF[2] < 0)

  expect_warning(weighted_posteriors(bfit, bfit2))
  x <- suppressWarnings(weighted_posteriors(bfit, bfit2))
  expect_equal(ncol(x), 14)

  # bfit_prior <- unupdate(bfit)
  # capture.output(x <- expect_warning(bayesfactor_parameters(bfit, prior = bfit_prior)))
  # expect_equal(nrow(x), 14)
  #
  # x <- expect_warning(si(bfit, prior = bfit_prior))
  # expect_equal(nrow(x), 14)
  #
  # ## Prior/posterior checks ----
  # suppressWarnings(x <- check_prior(bfit))
  # expect_equal(nrow(x), 13)
  #
  # x <- check_prior(bfit, simulate_priors = FALSE)
  # expect_equal(nrow(x), 14)

  x <- diagnostic_posterior(bfit)
  expect_equal(nrow(x), 14)

  x <- simulate_prior(bfit)
  expect_equal(ncol(x), 13)
  # YES this is 13! We have two parameters with the same prior.

  x <- describe_prior(bfit)
  expect_equal(nrow(x), 13)
  # YES this is 13! We have two parameters with the same prior.

  # x <- describe_posterior(bfit, test = "all", rope_range = c(-.1, .1))
  # expect_equal(nrow(x), 14)
})
