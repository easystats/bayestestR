skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")

test_that("rstanarm to freq", {
  skip_if_not_or_load_if_installed("rstanarm")

  set.seed(333)
  m <- insight::download_model("stanreg_glm_1")
  m1 <- glm(vs ~ wt, data = mtcars, family = "binomial")
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(coef(m1), coef(m2), tolerance = 1e-3)
})


test_that("rstanarm to freq", {
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("lme4")

  set.seed(333)
  m <- insight::download_model("stanreg_lmerMod_1")
  m1 <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(lme4::fixef(m1), lme4::fixef(m2), tolerance = 1e-3)
})


test_that("brms beta to freq", {
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("glmmTMB")
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("betareg")

  set.seed(333)
  m <- suppressWarnings(insight::download_model("brms_beta_1"))
  data(FoodExpenditure, package = "betareg")
  m1 <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    data = FoodExpenditure,
    family = glmmTMB::beta_family()
  )
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(lme4::fixef(m1)$cond[2], lme4::fixef(m2)$cond[2], tolerance = 1e-2)
})


test_that("ordbetareg to freq", {
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("ordbetareg")
  skip_if_not_or_load_if_installed("glmmTMB")
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("datawizard")

  set.seed(333)
  data(sleepstudy, package = "lme4")
  m <- suppressWarnings(insight::download_model("ordbetareg_1"))
  sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
  m1 <- glmmTMB::glmmTMB(
    y ~ Days + (Days | Subject),
    data = sleepstudy,
    family = glmmTMB::ordbeta()
  )
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(lme4::fixef(m1), lme4::fixef(m2), tolerance = 1e-1)
})


test_that("brms 0 + Intercept to freq", {
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  data(mtcars)
  m <- brms::brm(qsec ~ 0 + Intercept + mpg, data = mtcars, refresh = 0)
  m1 <- lm(qsec ~ mpg, data = mtcars)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(coef(m1), coef(m2), tolerance = 1e-2)
})


test_that("brms Interaction terms to freq", {
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  m <- brms::brm(qsec ~ mpg * as.factor(am), data = mtcars, refresh = 0)
  m1 <- lm(qsec ~ mpg * as.factor(am), data = mtcars)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(coef(m1), coef(m2), tolerance = 1e-2)
})


test_that("brms multivariate to freq, one response", {
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))
  d <- insight::get_data(m)

  m1 <- lm(gear ~ wt + hp, data = d)
  m2 <- convert_bayesian_as_frequentist(m, resp = "gear")
  expect_s3_class(m2, "lm")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)

  m1 <- lm(cyl ~ wt + hp, data = d)
  m2 <- bayesian_as_frequentist(m, resp = "cyl")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)

  # brms removes the dot from response names, "Sepal.Length" is "SepalLength"
  m <- insight::download_model("brms_mv_4")
  skip_if(is.null(m))
  d <- insight::get_data(m)
  m1 <- lm(Sepal.Length ~ Petal.Length + Sepal.Width + Species, data = d)
  m2 <- convert_bayesian_as_frequentist(m, resp = "SepalLength")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)
})


test_that("brms multivariate to freq, invalid resp", {
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))

  expect_error(
    convert_bayesian_as_frequentist(m),
    "(?s)resp.*cyl,\\s+gear,\\s+carb",
    perl = TRUE
  )
  expect_error(
    convert_bayesian_as_frequentist(m, resp = "mpg"),
    "(?s)resp.*cyl,\\s+gear,\\s+carb",
    perl = TRUE
  )
  expect_error(
    convert_bayesian_as_frequentist(m, resp = c("cyl", "gear")),
    "(?s)resp.*cyl,\\s+gear,\\s+carb",
    perl = TRUE
  )
})


test_that("brms univariate to freq, resp is ignored", {
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_1")
  skip_if(is.null(m))

  m1 <- convert_bayesian_as_frequentist(m)
  m2 <- convert_bayesian_as_frequentist(m, resp = "x")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)
})
