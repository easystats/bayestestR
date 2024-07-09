skip_on_cran()
skip_if_offline()
skip_if_not_or_load_if_installed("httr2")

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
  m <- insight::download_model("brms_beta_1")
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
  m <- insight::download_model("ordbetareg_1")
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
