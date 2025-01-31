skip_on_cran()
skip_if_not_installed("withr")
skip_if_not_installed("rstanarm")
skip_if_not_installed("marginaleffects", minimum_version = "0.24.1")
skip_if_not_installed("collapse")

withr::with_environment(
  new.env(),
  test_that("marginaleffects descrive_posterior", {
    # skip_on_ci()

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
    expect_equal(results[setdiff(colnames(results), c("term", "contrast", "am"))],
      results_draws[setdiff(colnames(results_draws), "Parameter")],
      ignore_attr = TRUE
    )

    # multi ci levels
    res <- hdi(mfx, ci = c(0.8, 0.9))
    expect_identical(
      as.data.frame(res[1:3]),
      data.frame(
        term = c(
          "am", "am", "am", "am", "cyl", "cyl",
          "cyl", "cyl", "cyl", "cyl", "cyl", "cyl",
          "hp", "hp", "hp", "hp"
        ),
        contrast = c(
          "1 - 0", "1 - 0", "1 - 0", "1 - 0",
          "6 - 4", "6 - 4", "8 - 4", "8 - 4",
          "6 - 4", "6 - 4", "8 - 4", "8 - 4",
          "dY/dX", "dY/dX", "dY/dX", "dY/dX"
        ),
        am = c(
          0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0,
          1, 1
        ), stringsAsFactors = FALSE
      )
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
)

withr::with_environment(
  new.env(),
  test_that("marginaleffects bayesfactors", {
    # skip_on_ci()

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

    expect_true(all(c("term", "contrast", "am") %in% colnames(outsi)))
    expect_equal(outsi[setdiff(colnames(outsi), c("term", "contrast", "am"))],
      outsiref[setdiff(colnames(outsiref), "Parameter")],
      ignore_attr = TRUE
    )

    # bayesfactor_parameters
    bfp <- bayesfactor_parameters(mfx, prior = mfxp, verbose = FALSE)
    bfpref <- bayesfactor_parameters(mfx_samps, prior = mfxp_samps, verbose = FALSE)
    expect_equal(bfp[setdiff(colnames(bfp), c("term", "contrast", "am"))],
      bfpref[setdiff(colnames(bfpref), "Parameter")],
      ignore_attr = TRUE
    )
  })
)
