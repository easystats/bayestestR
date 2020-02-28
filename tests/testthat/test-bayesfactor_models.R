if (require("rstanarm") &&
    require("BayesFactor") &&
    require("testthat") &&
    require("brms")) {
  context("bayesfactor_models + bayesfactor_inclusion")

  # bayesfactor_models ------------------------------------------------------

  set.seed(444)
  mo1 <- lme4::lmer(Sepal.Length ~ (1 | Species), data = iris)
  mo2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
  mo3 <- lme4::lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
  mo4 <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris)
  mo5 <- lme4::lmer(Sepal.Length ~ Petal.Length * Petal.Width + (Petal.Length | Species), data = iris)
  mo4_e <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris[-1, ])

  # both uses of denominator
  BFM1 <- bayestestR::bayesfactor_models(mo2, mo3, mo4, mo1, denominator = 4)
  BFM2 <- bayestestR::bayesfactor_models(mo2, mo3, mo4, denominator = mo1)
  BFM3 <- bayestestR::bayesfactor_models(mo2, mo3, mo4, mo1, denominator = mo1)
  BFM4 <- bayestestR::bayesfactor_models(mo2, mo3, mo4, mo5, mo1, denominator = mo1)

  test_that("bayesfactor_models BIC", {
    set.seed(444)

    testthat::expect_equal(BFM1, BFM2)
    testthat::expect_equal(BFM1, BFM3)

    # only on same data!
    testthat::expect_error(bayestestR::bayesfactor_models(mo1, mo2, mo4_e))

    # update models
    testthat::expect_equal(log(update(BFM2, subset = c(1, 2))$BF), c(0, 57.3, 54.52), tolerance = 0.1)

    # update reference
    testthat::expect_equal(log(update(BFM2, reference = 1)$BF), c(0, -2.8, -6.2, -57.4), tolerance = 0.1)
  })

  test_that("bayesfactor_models BIC (unsupported / diff nobs)", {
    set.seed(444)

    fit1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, iris)
    fit2a <- lm(Sepal.Length ~ Sepal.Width, iris[-1, ]) # different number of objects
    fit2b <- lm(Sepal.Length ~ Sepal.Width, iris) # not supported
    class(fit2b) <- "NOTLM"
    logLik.NOTLM <- function(...){
      stats:::logLik.lm(...)
    }

    # Should fail
    testthat::expect_error(bayesfactor_models(fit1, fit2a))

    # Should warn, but still work
    testthat::expect_warning(res <- bayesfactor_models(fit1, fit2b))
    testthat::expect_equal(log(res$BF), c(0, -133.97), tolerance = 0.1)
  })

  test_that("bayesfactor_models RSTANARM", {
    testthat::skip("Skipping bayesfactor_models RSTANARM")
    library(rstanarm)
    set.seed(444)
    stan_bf_0 <- stan_glm(Sepal.Length ~ 1,
                          data = iris,
                          refresh = 0,
                          diagnostic_file = file.path(tempdir(), "df0.csv")
    )
    stan_bf_1 <- stan_glm(Sepal.Length ~ Species,
                          data = iris,
                          refresh = 0,
                          diagnostic_file = file.path(tempdir(), "df1.csv")
    )

    testthat::expect_warning(stan_models <- bayestestR::bayesfactor_models(stan_bf_0, stan_bf_1))
    testthat::expect_is(stan_models, "bayesfactor_models")
    testthat::expect_equal(log(stan_models$BF), c(0, 65.19), tolerance = 0.1)
  })

  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"
  if (.runThisTest || Sys.getenv("USER") == "travis") {
    test_that("bayesfactor_models BRMS", {
      testthat::skip("Skipping bayesfactor_models BRMS")
      set.seed(444)
      brms_4bf_1 <- insight::download_model("brms_4bf_1")
      brms_4bf_2 <- insight::download_model("brms_4bf_2")
      brms_4bf_3 <- insight::download_model("brms_4bf_3")
      brms_4bf_4 <- insight::download_model("brms_4bf_4")
      brms_4bf_5 <- insight::download_model("brms_4bf_5")

      library(brms)
      brms_models <- suppressWarnings(bayestestR::bayesfactor_models(brms_4bf_1, brms_4bf_2, brms_4bf_3, brms_4bf_4, brms_4bf_5))

      testthat::expect_warning(bayestestR::bayesfactor_models(brms_4bf_1, brms_4bf_2))
      testthat::expect_is(brms_models, "bayesfactor_models")
      testthat::expect_equal(log(brms_models$BF), c(0, 68.5, 102.5, 128.6, 128.8), tolerance = 0.1)
    })
  }

  # bayesfactor_inclusion ---------------------------------------------------

  test_that("bayesfactor_inclusion", {
    set.seed(444)
    # BayesFactor
    ToothGrowth$dose <- as.factor(ToothGrowth$dose)
    BF_ToothGrowth <- BayesFactor::anovaBF(len ~ dose * supp, ToothGrowth)
    testthat::expect_equal(
      bayestestR::bayesfactor_inclusion(BF_ToothGrowth),
      bayestestR::bayesfactor_inclusion(bayestestR::bayesfactor_models(BF_ToothGrowth))
    )

    # with random effects in all models:
    testthat::expect_true(is.nan(bayestestR::bayesfactor_inclusion(BFM1)[1, "BF"]))

    bfinc_all <- bayestestR::bayesfactor_inclusion(BFM4, match_models = FALSE)
    testthat::expect_equal(bfinc_all$p_prior, c(1, 0.8, 0.6, 0.4, 0.2), tolerance = 0.1)
    testthat::expect_equal(bfinc_all$p_posterior, c(1, 1, 0.06, 0.01, 0), tolerance = 0.1)
    testthat::expect_equal(log(bfinc_all$BF), c(NaN, 56.04, -3.22, -5.9, -8.21), tolerance = 0.1)

    # + match_models
    bfinc_matched <- bayestestR::bayesfactor_inclusion(BFM4, match_models = TRUE)
    testthat::expect_equal(bfinc_matched$p_prior, c(1, 0.2, 0.6, 0.2, 0.2), tolerance = 0.1)
    testthat::expect_equal(bfinc_matched$p_posterior, c(1, 0.94, 0.06, 0.01, 0), tolerance = 0.1)
    testthat::expect_equal(log(bfinc_matched$BF), c(NaN, 57.37, -3.92, -5.25, -3.25), tolerance = 0.1)
  })
}