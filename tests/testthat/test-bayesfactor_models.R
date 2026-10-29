# bayesfactor_models BIC --------------------------------------------------
test_that("bayesfactor_models BIC", {
  skip_if_not_or_load_if_installed("lme4")
  set.seed(444)
  void <- suppressMessages(capture.output({
    mo1 <- lme4::lmer(Sepal.Length ~ (1 | Species), data = iris)
    mo2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
    mo3 <- lme4::lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
    mo4 <- lme4::lmer(
      Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species),
      data = iris
    )
    mo5 <- lme4::lmer(
      Sepal.Length ~ Petal.Length * Petal.Width + (Petal.Length | Species),
      data = iris
    )
    mo4_e <- lme4::lmer(
      Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species),
      data = iris[-1, ]
    )
  }))

  # both uses of denominator
  BFM1 <<- bayesfactor_models(mo2, mo3, mo4, mo1, denominator = 4)
  BFM2 <- bayesfactor_models(mo2, mo3, mo4, denominator = mo1)
  BFM3 <- bayesfactor_models(mo2, mo3, mo4, mo1, denominator = mo1)
  BFM4 <<- bayesfactor_models(mo2, mo3, mo4, mo5, mo1, denominator = mo1)

  expect_equal(BFM1, BFM2, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(BFM1, BFM3, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    BFM1,
    bayesfactor_models(list(mo2 = mo2, mo3 = mo3, mo4 = mo4, mo1 = mo1), denominator = 4),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # only on same data!
  expect_warning(bayesfactor_models(mo1, mo2, mo4_e))

  # update models
  expect_equal(update(BFM2, subset = c(1, 2))$log_BF, c(1, 57.3, 54.52), tolerance = 0.1)

  # update reference
  expect_equal(
    update(BFM2, reference = 1)$log_BF,
    c(0, -2.8, -6.2, -57.4),
    tolerance = 0.1
  )
})


test_that("bayesfactor_models BIC, transformed responses", {
  skip_if_not_or_load_if_installed("lme4")

  m1 <- lm(mpg ~ 1, mtcars)
  m2 <- lm(sqrt(mpg) ~ 1, mtcars)

  BF1 <- bayesfactor_models(m1, m2, check_response = TRUE)
  expect_equal(BF1$log_BF[2], 2.4404 / 2, tolerance = 0.01)

  BF2 <- bayesfactor_models(m1, m2, check_response = FALSE)
  expect_false(isTRUE(all.equal(BF1, BF2)))
})

test_that("bayesfactor_models BIC (unsupported / diff nobs)", {
  skip_if_not_or_load_if_installed("lme4")

  skip_on_cran()
  set.seed(444)

  fit1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, iris)
  fit2a <- lm(Sepal.Length ~ Sepal.Width, iris[-1, ]) # different number of objects
  fit2b <- lm(Sepal.Length ~ Sepal.Width, iris) # not supported
  class(fit2b) <- "NOTLM"
  logLik.NOTLM <<- function(...) {
    stats:::logLik.lm(...)
  }

  # Should warm
  expect_warning(bayesfactor_models(fit1, fit2a))

  # Should fail
  suppressWarnings(expect_message(bayesfactor_models(fit1, fit2b), "Unable"))
})

test_that("bayesfactor_models | bayesfactor_matrix", {
  data("mtcars")
  lm1 <- lm(mpg ~ 1, data = mtcars)
  lm2 <- lm(mpg ~ hp, data = mtcars)
  lm3 <- lm(mpg ~ hp + drat, data = mtcars)
  lm4 <- lm(mpg ~ hp * drat, data = mtcars)
  BFM1 <- bayesfactor_models(lm1, lm2, lm3, lm4, denominator = 1)
  BFM2 <- update(BFM1, reference = 2)

  bfmat <- as.matrix(BFM1)
  expect_identical(as.matrix(BFM2), bfmat)

  expect_identical(unname(diag(bfmat)), rep(0, 4))
  expect_identical(-t(bfmat)[upper.tri(bfmat)], bfmat[upper.tri(bfmat)])

  expect_output(print(bfmat), regexp = "Denominator\\\\Numerator")
})


# bayesfactor_models STAN ---------------------------------------------

test_that("bayesfactor_models STAN", {
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("bridgesampling")
  skip_if_not_or_load_if_installed("brms")

  skip_on_cran()
  set.seed(333)
  stan_bf_0 <- rstanarm::stan_glm(
    Sepal.Length ~ 1,
    data = iris,
    refresh = 0,
    iter = 500,
    diagnostic_file = file.path(tempdir(), "df0.csv")
  )
  stan_bf_1 <- suppressWarnings(rstanarm::stan_glm(
    Sepal.Length ~ Species,
    data = iris,
    refresh = 0,
    iter = 500,
    diagnostic_file = file.path(tempdir(), "df1.csv")
  ))

  set.seed(333) # compare against bridgesampling
  bridge_BF <- bridgesampling::bayes_factor(
    bridgesampling::bridge_sampler(stan_bf_1, silent = TRUE),
    bridgesampling::bridge_sampler(stan_bf_0, silent = TRUE)
  )

  set.seed(333)
  suppressMessages({
    expect_warning({
      stan_models <- bayesfactor_models(stan_bf_0, stan_bf_1)
    })
  })
  expect_s3_class(stan_models, "bayesfactor_models")
  expect_length(stan_models$log_BF, 2)
  expect_equal(stan_models$log_BF[2], log(bridge_BF$bf), tolerance = 0.1)
})


# bayesfactor_models STAN, bridge sampling options ----------------------

# records the options that bridge_sampler() receives and returns known log
# marginal likelihoods for each model
.mock_bridge_sampler <- function(models, logml, env) {
  function(samples, ...) {
    env$calls[[length(env$calls) + 1]] <- list(...)
    i <- which(vapply(models, identical, logical(1), y = samples))
    structure(list(logml = logml[[i]]), class = "bridge")
  }
}

# fake models need no fit; insight is mocked where it would need one
.local_fake_stan_models <- function(env = parent.frame()) {
  local_mocked_bindings(
    find_algorithm = function(x, ...) list(iterations = 1000, warmup = 500, chains = 1),
    get_response = function(x, ...) NULL,
    .package = "insight",
    .env = env
  )
  local_mocked_bindings(.find_full_formula = function(mod) "", .env = env)
}

test_that("bayesfactor_models STAN, bridge_options and median of repeated estimates", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("bridgesampling")
  .local_fake_stan_models()

  m0 <- structure(list(id = 0), class = "stanreg")
  m1 <- structure(list(id = 1), class = "stanreg")
  env <- new.env()
  local_mocked_bindings(
    bridge_sampler = .mock_bridge_sampler(
      list(m0, m1),
      list(c(-20, NA, -21, -30, -22), c(-10, -12, -17)),
      env
    ),
    .package = "bridgesampling"
  )
  opts <- list(repetitions = 3, silent = FALSE, my_option = "x")
  out <- bayesfactor_models(
    m1,
    m0,
    denominator = 2,
    verbose = FALSE,
    bridge_options = opts
  )
  expect_identical(out$log_BF, c(-12 - -21.5, 0))
  expect_identical(env$calls, list(opts, opts))

  # without `silent`, `silent = !verbose` is passed
  env$calls <- list()
  suppressWarnings(suppressMessages(bayesfactor_models(m1, m0)))
  expect_identical(env$calls, list(list(silent = FALSE), list(silent = FALSE)))
})

test_that("bayesfactor_models STAN, bridge_options must be a named list", {
  skip_if_not_installed("bridgesampling")
  fake_stanreg <- structure(list(), class = "stanreg")
  mods <- list(a = fake_stanreg, b = fake_stanreg)
  bad_options <- list(
    list(3),
    list(repetitions = 3, 2),
    c(repetitions = 3),
    stats::setNames(list(3), NA_character_)
  )
  for (opts in bad_options) {
    expect_error(
      .bayesfactor_models_stan_REG(mods, denominator = 1, bridge_options = opts),
      "`bridge_options` must be a list in which every element has a name",
      fixed = TRUE
    )
  }
})

test_that("bayesfactor_models blavaan does not check bridge_options", {
  skip_if_not_installed("blavaan")
  .local_fake_stan_models()
  local_mocked_bindings(
    .bayesfactor_models_stan_SEM = function(mods, denominator, verbose = TRUE) {
      data.frame(Model = names(mods), log_BF = c(0, 1), stringsAsFactors = FALSE)
    }
  )
  fake_blavaan <- structure(list(), class = "blavaan")
  out <- bayesfactor_models(
    fake_blavaan,
    fake_blavaan,
    verbose = FALSE,
    bridge_options = list(3)
  )
  expect_identical(out$log_BF, c(0, 1))
})

test_that("bayesfactor_models STAN, precision warning names repetitions", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("brms")
  skip_if_not_installed("bridgesampling")
  .local_fake_stan_models()
  local_mocked_bindings(
    bridge_sampler = function(...) structure(list(logml = -1), class = "bridge"),
    .package = "bridgesampling"
  )

  m <- structure(list(), class = "stanreg")
  expect_warning(suppressMessages(bayesfactor_models(m, m)), "bridge_options")

  # no hint when every model has a stored marglik; NA estimates are left out
  m <- structure(
    list(criteria = list(marglik = list(logml = c(-3, NA, -1, -2, -10)))),
    class = "brmsfit"
  )
  w <- capture_warnings(suppressMessages(bayesfactor_models(m, m)))
  expect_match(w, "40,000")
  expect_no_match(w, "bridge_options")
  expect_identical(.get_marglik(m, verbose = TRUE), -2.5)
})

test_that("bayesfactor_models BRMS", {
  # Checks for brms models
  skip_on_cran()
  # skip_on_ci()

  skip(message = "Currently fails, due to bridgesampling?")
  #   1. ├─base::suppressWarnings(...) at tests/testthat/test-bayesfactor_models.R:176:3
  #   2. │ └─base::withCallingHandlers(...)
  #   3. ├─base::suppressMessages(...)
  #   4. │ └─base::withCallingHandlers(...)
  #   5. ├─testthat::expect_message(...)
  #   6. │ └─testthat:::expect_condition_matching_(...)
  #   7. │   └─testthat:::quasi_capture(...)
  #   8. │     ├─testthat (local) .capture(...)
  #   9. │     │ └─base::withCallingHandlers(...)
  #  10. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
  #  11. ├─bayestestR::bayesfactor_models(stan_brms_model_0, stan_brms_model_1) at tests/testthat/test-bayesfactor_models.R:179:9
  #  12. └─bayestestR:::bayesfactor_models.brmsfit(stan_brms_model_0, stan_brms_model_1) at bayestestR/R/bayesfactor_models.R:164:3
  #  13.   └─bayestestR:::.bayesfactor_models_stan(...) at bayestestR/R/bayesfactor_models.R:399:3
  #  14.     └─bayestestR:::.bayesfactor_models_stan_REG(...) at bayestestR/R/bayesfactor_models.R:307:5
  #  15.       └─base::lapply(mods, .get_marglik, verbose = verbose) at bayestestR/R/bayesfactor_models.R:343:3
  #  16.         └─bayestestR (local) FUN(X[[i]], ...)
  #  17.           ├─bridgesampling::bridge_sampler(mod, silent = TRUE) at bayestestR/R/bayesfactor_models.R:605:3
  #  18.           └─brms:::bridge_sampler.brmsfit(mod, silent = TRUE)
  #  19.             └─brms:::update_misc_env(samples, recompile = recompile)
  #  20.               └─brms::add_rstan_model(x, overwrite = TRUE)
  #  21.                 ├─base::suppressMessages(...)
  #  22.                 │ └─base::withCallingHandlers(...)
  #  23.                 └─rstan::stan(...)
  #  24.                   └─rstan::stan_model(...)
  #  25.                     └─rstan:::cxxfunctionplus(...)
  #  26.                       └─base::sink(type = 'output')

  skip_if_not_or_load_if_installed("bridgesampling")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  stan_brms_model_0 <- skip_if_fit_failed(brm(
    Sepal.Length ~ 1,
    data = iris,
    iter = 500,
    refresh = 0,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2
  ))

  stan_brms_model_1 <- skip_if_fit_failed(brm(
    Sepal.Length ~ Petal.Length,
    data = iris,
    iter = 500,
    refresh = 0,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2
  ))

  set.seed(444)
  suppressWarnings(suppressMessages(
    expect_message(
      {
        bfm <- bayesfactor_models(stan_brms_model_0, stan_brms_model_1)
      },
      regexp = "marginal"
    )
  ))

  set.seed(444)
  stan_brms_model_0wc <- brms::add_criterion(
    stan_brms_model_0,
    criterion = "marglik",
    repetitions = 5,
    silent = 2
  )

  stan_brms_model_1wc <- brms::add_criterion(
    stan_brms_model_1,
    criterion = "marglik",
    repetitions = 5,
    silent = 2
  )

  suppressWarnings(expect_message(
    {
      bfmwc <- bayesfactor_models(stan_brms_model_0wc, stan_brms_model_1wc)
    },
    regexp = NA
  ))
  expect_equal(bfmwc$log_BF, bfm$log_BF, tolerance = 0.01)
})


# bayesfactor_inclusion ---------------------------------------------------
test_that("bayesfactor_inclusion | BayesFactor", {
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(444)
  # BayesFactor
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  BF_ToothGrowth <- BayesFactor::anovaBF(len ~ dose * supp, ToothGrowth)
  expect_equal(
    bayesfactor_inclusion(BF_ToothGrowth),
    bayesfactor_inclusion(bayesfactor_models(BF_ToothGrowth)),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("bayesfactor_inclusion | LMM", {
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("BayesFactor")

  # with random effects in all models:
  expect_true(is.nan(bayesfactor_inclusion(BFM1)["1:Species", "log_BF"]))

  bfinc_all <- bayesfactor_inclusion(BFM4, match_models = FALSE)
  expect_equal(bfinc_all$p_prior, c(1, 0.8, 0.6, 0.4, 0.2), tolerance = 0.1)
  expect_equal(bfinc_all$p_posterior, c(1, 1, 0.12, 0.01, 0), tolerance = 0.1)
  expect_equal(bfinc_all$log_BF, c(NaN, 57.651, -2.352, -4.064, -4.788), tolerance = 0.1)

  # plus match_models
  bfinc_matched <- bayesfactor_inclusion(BFM4, match_models = TRUE)
  expect_equal(bfinc_matched$p_prior, c(1, 0.2, 0.6, 0.2, 0.2), tolerance = 0.1)
  expect_equal(
    bfinc_matched$p_posterior,
    c(1, 0.875, 0.125, 0.009, 0.002),
    tolerance = 0.1
  )
  expect_equal(
    bfinc_matched$log_BF,
    c(NaN, 58.904, -3.045, -3.573, -1.493),
    tolerance = 0.1
  )
})

test_that("bayesfactor() passes verbose to bayesfactor_models()", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(qsec ~ wt, data = mtcars)
  expect_warning(bayesfactor(m1, m2), "probably not all models were fit")
  expect_no_warning(bayesfactor(m1, m2, verbose = FALSE))
})
