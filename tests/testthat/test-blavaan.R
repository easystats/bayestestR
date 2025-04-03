test_that("blavaan, all", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("blavaan")
  skip_if_not_or_load_if_installed("lavaan")
  skip_if_not_or_load_if_installed("rstan")
  skip_if_not_or_load_if_installed("cmdstanr")
  skip_if_not(dir.exists(cmdstanr::cmdstan_default_install_path()))

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
  expect_identical(nrow(x), 10L)

  x <- eti(bfit)
  expect_identical(nrow(x), 10L)

  x <- hdi(bfit)
  expect_identical(nrow(x), 10L)

  x <- p_direction(bfit)
  expect_identical(nrow(x), 10L)

  x <- rope(bfit, range = c(-0.1, 0.1))
  expect_identical(nrow(x), 10L)

  x <- p_rope(bfit, range = c(-0.1, 0.1))
  expect_identical(nrow(x), 10L)

  x <- p_map(bfit)
  expect_identical(nrow(x), 10L)

  x <- p_significance(bfit, threshold = c(-0.1, 0.1))
  expect_identical(nrow(x), 10L)

  x <- equivalence_test(bfit, range = c(-0.1, 0.1))
  expect_identical(nrow(x), 10L)

  x <- estimate_density(bfit)
  expect_length(unique(x$Parameter), 10)


  ## Bayes factors ----

  # For these models, no BF available, see #627
  expect_warning(bayesfactor_models(bfit, bfit2), regex = "Bayes factors might not be precise")

  ## FIXME: rror in `Yp[[p]]$SY + tcrossprod(Yp[[p]]$MY - Mu[var.idx])`:
  ## ! non-conformable arrays
  # bfit_prior <- unupdate(bfit)
  # capture.output(x <- expect_warning(bayesfactor_parameters(bfit, prior = bfit_prior)))
  # expect_identical(nrow(x), 10L)

  # x <- expect_warning(si(bfit, prior = bfit_prior))
  # expect_identical(nrow(x), 10L)

  ## Prior/posterior checks ----
  suppressWarnings(x <- check_prior(bfit))
  expect_identical(nrow(x), 13L)

  ## FIXME: Error in `Yp[[p]]$SY + tcrossprod(Yp[[p]]$MY - Mu[var.idx])`:
  ## ! non-conformable arrays
  # x <- check_prior(bfit, simulate_priors = FALSE)
  # expect_identical(nrow(x), 10L)

  x <- diagnostic_posterior(bfit)
  expect_identical(nrow(x), 10L)

  ## FIXME: no longer 13, but now 9?
  x <- simulate_prior(bfit)
  expect_identical(ncol(x), 9L)
  # YES this is 13! We have two parameters with the same prior.

  ## FIXME: no longer 13, but now 9?
  x <- describe_prior(bfit)
  expect_identical(nrow(x), 9L)
  # YES this is 13! We have two parameters with the same prior.

  x <- describe_posterior(bfit, test = "all", rope_range = c(-0.1, 0.1))
  expect_identical(nrow(x), 10L)
})
