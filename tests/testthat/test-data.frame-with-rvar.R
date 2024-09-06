test_that("data.frame w/ rvar_col descrive_posterior etc", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("posterior")

  dfx <- data.frame(mu = c(0, 0.5, 1), sigma = c(1, 0.5, 0.25))
  dfx$my_rvar <- posterior::rvar_rng(rnorm, 3, mean = dfx$mu, sd = dfx$sigma)
  dfx$other_rvar <- posterior::rvar_rng(rnorm, 3, mean = dfx$mu + 0.5, sd = dfx$sigma - 0.1)
  dfx

  ## Errors
  expect_error(p_direction(dfx, rvar_col = "mu"))
  expect_error(p_direction(dfx, rvar_col = "my_rvarrrrrr"))


  ## describe_posterior
  res <- describe_posterior(dfx,
    rvar_col = "my_rvar",
    centrality = "MAP", ci_method = "hdi", ci = 0.8,
    test = c("pd", "p_map", "rope", "equivalence_test"),
    rope_ci = 1, rope_range = c(-1, 0.5)
  )
  res.ref <- describe_posterior(dfx$my_rvar,
    centrality = "MAP", ci_method = "hdi", ci = 0.8,
    test = c("pd", "p_map", "rope", "equivalence_test"),
    rope_ci = 1, rope_range = c(-1, 0.5)
  )
  expect_true(all(c("mu", "sigma") %in% colnames(res)))
  expect_equal(res[setdiff(colnames(res), c("mu", "sigma"))],
    res.ref[setdiff(colnames(res.ref), "Parameter")],
    ignore_attr = TRUE
  )

  ## CIs
  res <- eti(dfx, rvar_col = "my_rvar")
  res.ref <- eti(dfx$my_rvar)
  expect_true(all(c("mu", "sigma") %in% colnames(res)))
  expect_identical(nrow(format(res)), 3L)
  expect_identical(ncol(format(res)), 3L)
  expect_equal(res[setdiff(colnames(res), c("mu", "sigma"))],
    res.ref[setdiff(colnames(res.ref), "Parameter")],
    ignore_attr = TRUE
  )

  res <- eti(dfx, rvar_col = "my_rvar", ci = c(0.8, 0.95))
  res.ref <- eti(dfx$my_rvar, ci = c(0.8, 0.95))
  expect_true(all(c("mu", "sigma") %in% colnames(res)))
  expect_identical(nrow(format(res)), 3L)
  expect_identical(ncol(format(res)), 4L)
  expect_equal(res[setdiff(colnames(res), c("mu", "sigma"))],
    res.ref[setdiff(colnames(res.ref), "Parameter")],
    ignore_attr = TRUE
  )

  ## estimate_density
  res <- estimate_density(dfx, rvar_col = "my_rvar")
  res.ref <- estimate_density(dfx$my_rvar)
  expect_true(all(c("mu", "sigma") %in% colnames(res)))
  expect_equal(res[setdiff(colnames(res), c("mu", "sigma"))],
    res.ref[setdiff(colnames(res.ref), "Parameter")],
    ignore_attr = TRUE
  )
})

test_that("data.frame w/ rvar_col bayesfactors", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("posterior")
  skip_if_not_installed("logspline")

  dfx <- data.frame(mu = c(0, 0.5, 1), sigma = c(1, 0.5, 0.25))
  dfx$my_rvar <- posterior::rvar_rng(rnorm, 3, mean = dfx$mu, sd = dfx$sigma)
  dfx$other_rvar <- posterior::rvar_rng(rnorm, 3, mean = dfx$mu + 0.5, sd = dfx$sigma - 0.1)
  dfx



  ## SIs
  res <- si(dfx, rvar_col = "my_rvar", prior = "other_rvar", verbose = FALSE)
  res.ref <- si(dfx$my_rvar, prior = dfx$other_rvar, verbose = FALSE)
  expect_true(all(c("mu", "sigma") %in% colnames(res)))
  expect_identical(nrow(format(res)), 3L)
  expect_identical(ncol(format(res)), 3L)
  expect_equal(res[setdiff(colnames(res), c("mu", "sigma"))],
    res.ref[setdiff(colnames(res.ref), "Parameter")],
    ignore_attr = TRUE
  )

  res <- si(dfx,
    rvar_col = "my_rvar", prior = "other_rvar",
    BF = c(1, 3), verbose = FALSE
  )
  res.ref <- si(dfx$my_rvar,
    prior = dfx$other_rvar,
    BF = c(1, 3), verbose = FALSE
  )
  expect_true(all(c("mu", "sigma") %in% colnames(res)))
  expect_identical(nrow(format(res)), 3L)
  expect_identical(ncol(format(res)), 4L)
  expect_equal(format(res[setdiff(colnames(res), c("mu", "sigma"))]),
    format(res.ref[setdiff(colnames(res.ref), "Parameter")]),
    ignore_attr = TRUE
  )


  ## bayesfactor_parameters
  res <- bayesfactor_parameters(dfx,
    rvar_col = "my_rvar", prior = "other_rvar",
    verbose = FALSE
  )
  res.ref <- bayesfactor_parameters(dfx$my_rvar,
    prior = dfx$other_rvar,
    verbose = FALSE
  )
  expect_equal(res[setdiff(colnames(res), c("mu", "sigma"))],
    res.ref[setdiff(colnames(res.ref), "Parameter")],
    ignore_attr = TRUE
  )
})
