test_that("marginaleffects descrive_posterior", {
  # skip_on_ci()
  skip_on_cran()

  skip_if_not_installed("rstanarm")
  skip_if_not_installed("marginaleffects")

  data("mtcars")
  mtcars$cyl <- factor(mtcars$cyl)
  mod <- rstanarm::stan_glm(mpg ~ cyl + hp * am, data = mtcars, refresh = 0)

  mfx <- marginaleffects::avg_slopes(mod, by = "am")
  mfx_samps <- as.data.frame(t(attr(mfx, "posterior_draws")))

  results <- describe_posterior(mfx,
    centrality = "MAP", ci_method = "hdi",
    test = c("pd", "rope", "p_map", "equivalence_test")
  )
  results_draws <- describe_posterior(mfx_samps,
    centrality = "MAP", ci_method = "hdi",
    test = c("pd", "rope", "p_map", "equivalence_test")
  )

  expect_true(all(c("term", "contrast") %in% colnames(results)))
  expect_equal(results[setdiff(colnames(results), c("term", "contrast"))],
    results_draws[setdiff(colnames(results_draws), "Parameter")],
    ignore_attr = TRUE
  )

  # estimate_density
  mfx <- marginaleffects::comparisons(mod,
    variables = "cyl",
    newdata = marginaleffects::datagrid(hp = 100, am = 0)
  )
  samps <- insight::get_parameters(mod)[c("cyl6", "cyl8")]

  res <- estimate_density(mfx)
  resref <- estimate_density(samps)
  expect_equal(res[intersect(colnames(res), colnames(resref))],
    resref[intersect(colnames(res), colnames(resref))],
    ignore_attr = TRUE
  )
})

test_that("marginaleffects bayesfactors", {
  # skip_on_ci()
  skip_on_cran()

  skip_if_not_installed("rstanarm")
  skip_if_not_installed("marginaleffects")

  data("mtcars")
  mtcars$cyl <- factor(mtcars$cyl)
  mod <- rstanarm::stan_glm(mpg ~ cyl + hp * am, data = mtcars, refresh = 0)
  modp <- unupdate(mod, verbose = FALSE)

  mfx <- marginaleffects::avg_slopes(mod, by = "am")
  mfxp <- marginaleffects::avg_slopes(modp, by = "am")

  mfx_samps <- as.data.frame(t(attr(mfx, "posterior_draws")))
  mfxp_samps <- as.data.frame(t(attr(mfxp, "posterior_draws")))

  # SI
  outsi <- si(mfx, prior = mfxp, verbose = FALSE)
  outsiref <- si(mfx_samps, prior = mfxp_samps, verbose = FALSE)

  expect_true(all(c("term", "contrast") %in% colnames(outsi)))
  expect_equal(outsi[setdiff(colnames(outsi), c("term", "contrast"))],
    outsiref[setdiff(colnames(outsiref), "Parameter")],
    ignore_attr = TRUE
  )

  # bayesfactor_parameters
  bfp <- bayesfactor_parameters(mfx, prior = mfxp, verbose = FALSE)
  bfpref <- bayesfactor_parameters(mfx_samps, prior = mfxp_samps, verbose = FALSE)
  expect_equal(bfp[setdiff(colnames(bfp), c("term", "contrast"))],
    bfpref[setdiff(colnames(bfpref), "Parameter")],
    ignore_attr = TRUE
  )
})
