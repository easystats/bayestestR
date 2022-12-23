# library(testthat)
if (requiet("lme4")) {
  # bayesfactor_models BIC --------------------------------------------------
  test_that("bayesfactor_models BIC", {
    set.seed(444)
    void <- suppressMessages(capture.output({
      mo1 <- lme4::lmer(Sepal.Length ~ (1 | Species), data = iris)
      mo2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
      mo3 <- lme4::lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
      mo4 <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris)
      mo5 <- lme4::lmer(Sepal.Length ~ Petal.Length * Petal.Width + (Petal.Length | Species), data = iris)
      mo4_e <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris[-1, ])
    }))

    # both uses of denominator
    BFM1 <<- bayesfactor_models(mo2, mo3, mo4, mo1, denominator = 4)
    BFM2 <- bayesfactor_models(mo2, mo3, mo4, denominator = mo1)
    BFM3 <- bayesfactor_models(mo2, mo3, mo4, mo1, denominator = mo1)
    BFM4 <<- bayesfactor_models(mo2, mo3, mo4, mo5, mo1, denominator = mo1)

    expect_equal(BFM1, BFM2)
    expect_equal(BFM1, BFM3)
    expect_equal(BFM1, bayesfactor_models(list(mo2 = mo2, mo3 = mo3, mo4 = mo4, mo1 = mo1), denominator = 4))

    # only on same data!
    expect_warning(bayesfactor_models(mo1, mo2, mo4_e))

    # update models
    expect_equal(update(BFM2, subset = c(1, 2))$log_BF, c(1, 57.3, 54.52), tolerance = 0.1)

    # update reference
    expect_equal(update(BFM2, reference = 1)$log_BF,
      c(0, -2.8, -6.2, -57.4),
      tolerance = 0.1
    )
  })


  test_that("bayesfactor_models BIC, transformed responses", {
    m1 <- lm(mpg ~ 1, mtcars)
    m2 <- lm(sqrt(mpg) ~ 1, mtcars)

    BF1 <- bayesfactor_models(m1, m2, check_response = TRUE)
    expect_equal(BF1$log_BF[2], 2.4404 / 2, tolerance = 0.01)

    BF2 <- bayesfactor_models(m1, m2, check_response = FALSE)
    expect_false(isTRUE(all.equal(BF1, BF2)))
  })

  test_that("bayesfactor_models BIC (unsupported / diff nobs)", {
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


  # bayesfactor_models STAN ---------------------------------------------
  if (requiet("rstanarm") && requiet("bridgesampling") && requiet("brms")) {
    test_that("bayesfactor_models STAN", {
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
        bridgesampling::bridge_sampler(stan_bf_1),
        bridgesampling::bridge_sampler(stan_bf_0)
      )

      set.seed(333)
      expect_warning(stan_models <- bayesfactor_models(stan_bf_0, stan_bf_1))
      expect_s3_class(stan_models, "bayesfactor_models")
      expect_equal(length(stan_models$log_BF), 2)
      expect_equal(stan_models$log_BF[2], log(bridge_BF$bf), tolerance = 0.1)
    })

    test_that("bayesfactor_models BRMS", {
      # Checks for brms models
      skip_on_cran()
      skip_on_ci()

      set.seed(333)
      stan_brms_model_0 <- brms::brm(
        Sepal.Length ~ 1,
        data = iris,
        iter = 500,
        refresh = 0,
        save_pars = brms::save_pars(all = TRUE)
      )

      stan_brms_model_1 <- brms::brm(
        Sepal.Length ~ Petal.Length,
        data = iris,
        iter = 500,
        refresh = 0,
        save_pars = brms::save_pars(all = TRUE)
      )

      set.seed(444)
      expect_message(bfm <- bayesfactor_models(stan_brms_model_0, stan_brms_model_1), regexp = "marginal")

      set.seed(444)
      stan_brms_model_0wc <- brms::add_criterion(
        stan_brms_model_0,
        criterion = "marglik",
        repetitions = 5,
        silent = TRUE
      )

      stan_brms_model_1wc <- brms::add_criterion(
        stan_brms_model_1,
        criterion = "marglik",
        repetitions = 5,
        silent = TRUE
      )

      expect_message(bfmwc <- bayesfactor_models(stan_brms_model_0wc, stan_brms_model_1wc), regexp = NA)
      expect_equal(bfmwc$log_BF, bfm$log_BF, tolerance = 0.01)
    })
  }


  # bayesfactor_inclusion ---------------------------------------------------
  if (requiet("BayesFactor")) {
    test_that("bayesfactor_inclusion | BayesFactor", {
      set.seed(444)
      # BayesFactor
      ToothGrowth$dose <- as.factor(ToothGrowth$dose)
      BF_ToothGrowth <- BayesFactor::anovaBF(len ~ dose * supp, ToothGrowth)
      expect_equal(
        bayesfactor_inclusion(BF_ToothGrowth),
        bayesfactor_inclusion(bayesfactor_models(BF_ToothGrowth))
      )
    })
  }

  test_that("bayesfactor_inclusion | LMM", {
    # with random effects in all models:
    expect_true(is.nan(bayesfactor_inclusion(BFM1)["1:Species", "log_BF"]))

    bfinc_all <- bayesfactor_inclusion(BFM4, match_models = FALSE)
    expect_equal(bfinc_all$p_prior, c(1, 0.8, 0.6, 0.4, 0.2), tolerance = 0.1)
    expect_equal(bfinc_all$p_posterior, c(1, 1, 0.12, 0.01, 0), tolerance = 0.1)
    expect_equal(bfinc_all$log_BF, c(NaN, 57.651, -2.352, -4.064, -4.788), tolerance = 0.1)

    # + match_models
    bfinc_matched <- bayesfactor_inclusion(BFM4, match_models = TRUE)
    expect_equal(bfinc_matched$p_prior, c(1, 0.2, 0.6, 0.2, 0.2), tolerance = 0.1)
    expect_equal(bfinc_matched$p_posterior, c(1, 0.875, 0.125, 0.009, 0.002), tolerance = 0.1)
    expect_equal(bfinc_matched$log_BF, c(NaN, 58.904, -3.045, -3.573, -1.493), tolerance = 0.1)
  })
}
