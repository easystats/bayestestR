test_that("BayesFactor mixed model to freq, random interaction", {
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")

  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$gear <- factor(d$gear)

  set.seed(333)
  m <- BayesFactor::lmBF(
    mpg ~ cyl + gear + cyl:gear,
    data = d,
    whichRandom = c("gear", "cyl:gear"),
    progress = FALSE
  )
  m1 <- suppressMessages(lme4::lmer(mpg ~ cyl + (1 | gear) + (1 | cyl:gear), data = d))
  m2 <- suppressMessages(convert_bayesian_as_frequentist(m))

  expect_s4_class(m2, "lmerMod")
  expect_equal(lme4::fixef(m2), lme4::fixef(m1), tolerance = 1e-6)
  expect_identical(sort(names(lme4::VarCorr(m2))), c("cyl:gear", "gear"))
})


test_that("BayesFactor mixed model to freq, random slope", {
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")

  data(sleepstudy, package = "lme4")
  sleepstudy$Subject <- factor(sleepstudy$Subject)

  set.seed(333)
  # BayesFactor warns that it removed NA samples for this model
  m <- suppressWarnings(BayesFactor::lmBF(
    Reaction ~ Days + Subject + Days:Subject,
    data = sleepstudy,
    whichRandom = "Subject",
    progress = FALSE
  ))
  m1 <- lme4::lmer(
    Reaction ~ Days + (1 | Subject) + (0 + Days | Subject),
    data = sleepstudy
  )
  m2 <- convert_bayesian_as_frequentist(m)

  expect_s4_class(m2, "lmerMod")
  expect_equal(lme4::fixef(m2), lme4::fixef(m1), tolerance = 1e-6)
  expect_equal(
    formula(m2, random.only = TRUE),
    formula(m1, random.only = TRUE),
    ignore_attr = TRUE
  )
})


test_that("BayesFactor mixed model to freq, REML", {
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")

  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$gear <- factor(d$gear)

  set.seed(333)
  m <- BayesFactor::lmBF(
    mpg ~ cyl + gear + cyl:gear,
    data = d,
    whichRandom = c("gear", "cyl:gear"),
    progress = FALSE
  )

  expect_true(lme4::isREML(suppressMessages(convert_bayesian_as_frequentist(m))))
  expect_false(lme4::isREML(suppressMessages(convert_bayesian_as_frequentist(m, REML = FALSE))))
})


test_that("BayesFactor model without random factors to freq", {
  skip_if_not_or_load_if_installed("BayesFactor")

  d <- mtcars
  d$cyl <- factor(d$cyl)

  set.seed(333)
  m <- BayesFactor::lmBF(mpg ~ cyl, data = d, progress = FALSE)
  m1 <- lm(mpg ~ cyl, data = d)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_s3_class(m2, "lm")
  expect_equal(coef(m2), coef(m1), tolerance = 1e-6)
})
