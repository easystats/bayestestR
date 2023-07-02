test_that("rstanarm to freq", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("httr")

  set.seed(333)
  m <- insight::download_model("stanreg_glm_1")
  m1 <- glm(vs ~ wt, data = mtcars, family = "binomial")
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(coef(m1), coef(m2), tolerance = 1e-3)
})


test_that("rstanarm to freq", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("httr")

  set.seed(333)
  m <- insight::download_model("stanreg_lmerMod_1")
  m1 <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(lme4::fixef(m1), lme4::fixef(m2), tolerance = 1e-3)
})
