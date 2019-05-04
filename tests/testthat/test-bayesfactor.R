context("bayesfactor_*")

test_that("bayesfactor_savagedickey", {
  set.seed(444)
  Xprior <- rnorm(1000)
  Xposterior <- rnorm(1000,0.7,0.2)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = 0)
  testthat::expect_equal(as.numeric(bfsd), 39.6, tolerance = 0.1)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 0, direction = 1)
  testthat::expect_equal(as.numeric(bfsd), 76.8, tolerance = 0.1)

  bfsd <- bayestestR::bayesfactor_savagedickey(Xposterior, prior = Xprior, hypothesis = 1, direction = 0)
  testthat::expect_equal(as.numeric(bfsd), 0.4, tolerance = 0.1)
})

set.seed(444)

mo1 <- lme4::lmer(Sepal.Length ~ (1 | Species), data = iris)
mo2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
mo3 <- lme4::lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
mo4 <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris)
mo4_e <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris[-1,])

# both uses of denominator
BFM1 <- bayestestR::bayesfactor_models(mo2, mo3, mo4, mo1, denominator = 4)
BFM2 <- bayestestR::bayesfactor_models(mo2, mo3, mo4, denominator = mo1)

test_that("bayesfactor_models", {
  ## CANT TEST BRMS / RSTANARM without running the whole thing
  ## loading data from circus looses the ll data


  ## BIC
  testthat::expect_equal(BFM1,BFM2)

  # only on same data!
  testthat::expect_error(bayestestR::bayesfactor_models(mo1, mo2, mo4_e))

  # update models
  testthat::expect_equal(
    update(BFM2,subset = c(1,2)),
    structure(list(Model = c("1 + (1 | Species)", "Petal.Length + (1 | Species)",
                             "Petal.Length + (Petal.Length | Species)"),
                   log.BF = c(0, 57.3710763968226, 54.5213320500375)),
              denominator = 1,
              BF_method = "BIC approximation",
              row.names = c(4L, 1L, 2L),
              class = c("bayesfactor_models", "data.frame"))
  )

  # update reference
  testthat::expect_equal(
    update(BFM2,reference = 1),
    structure(list(Model = c("Petal.Length + (1 | Species)", "Petal.Length + (Petal.Length | Species)",
                             "Petal.Length + Petal.Width + (Petal.Length | Species)", "1 + (1 | Species)"),
                   log.BF = c(0, -2.84974434678507, -6.2922613780033, -57.3710763968226)),
              row.names = c(NA, -4L),
              denominator = 1,
              BF_method = "BIC approximation",
              class = c("bayesfactor_models", "data.frame"))
  )
})

test_that("bayesfactor_inclusion", {
  # BayesFactor
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  BF_ToothGrowth <- BayesFactor::anovaBF(len ~ dose*supp, ToothGrowth)
  testthat::expect_equal(
    bayestestR::bayesfactor_inclusion(BF_ToothGrowth),
    bayestestR::bayesfactor_inclusion(bayesfactor_models(BF_ToothGrowth))
  )

  # with random effects in all models:
  testthat::expect_true(is.na(bayesfactor_inclusion(BFM1)[1,"log.BF.Inc"]))

  # + match_models
  testthat::expect_equal(
    bayesfactor_inclusion(BFM1, match_models = TRUE),
    structure(list(P.Inc.prior = c(1, 0.25, 0.5, 0.25),
                   P.Inc.posterior = c(1, 0.94365467987862, 0.0563453201213796, 0.00174629939236133),
                   log.BF.Inc = c(NaN, 57.3710763968226, -2.81826110716954, -5.24989624835758)),
              class = c("bayesfactor_inclusion", "data.frame"),
              row.names = c("1:Species", "Petal.Length", "Petal.Length:Species", "Petal.Width"),
              matched = TRUE)
  )
})