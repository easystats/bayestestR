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

  expect_equal(lme4::fixef(m2), lme4::fixef(m1), tolerance = 1e-6)
  expect_identical(sort(names(lme4::VarCorr(m2))), c("cyl:gear", "gear"))
  expect_true(lme4::isREML(m2))
  expect_false(lme4::isREML(suppressMessages(convert_bayesian_as_frequentist(
    m,
    REML = FALSE
  ))))

  expect_error(
    convert_bayesian_as_frequentist(m, data = d[c("mpg", "cyl")]),
    "could not be automatically converted.*gear"
  )
})


test_that("BayesFactor mixed model to freq, random factor only", {
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")

  d <- mtcars
  d$gear <- factor(d$gear)

  set.seed(333)
  m <- BayesFactor::lmBF(mpg ~ gear, data = d, whichRandom = "gear", progress = FALSE)
  m1 <- lme4::lmer(mpg ~ 1 + (1 | gear), data = d)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(formula(m2), formula(m1), ignore_attr = TRUE)
  expect_equal(lme4::fixef(m2), lme4::fixef(m1), tolerance = 1e-6)
})


test_that("BayesFactor mixed model to freq, random slope and non-syntactic names", {
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")

  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$`my gear` <- factor(d$gear)

  set.seed(333)
  # BayesFactor prints an internal error here, but returns the model
  utils::capture.output(
    m <- BayesFactor::lmBF(
      mpg ~ cyl + `my gear` + wt + wt:`my gear`,
      data = d,
      whichRandom = "my gear",
      progress = FALSE
    ),
    type = "message"
  )
  m1 <- suppressMessages(lme4::lmer(
    mpg ~ cyl + wt + (1 | `my gear`) + (0 + wt | `my gear`),
    data = d
  ))
  m2 <- suppressMessages(convert_bayesian_as_frequentist(m))

  expect_equal(formula(m2), formula(m1), ignore_attr = TRUE)
  expect_equal(lme4::fixef(m2), lme4::fixef(m1), tolerance = 1e-6)
})


test_that("BayesFactor mixed model to freq, several models", {
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")

  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$gear <- factor(d$gear)

  set.seed(333)
  m <- BayesFactor::generalTestBF(
    mpg ~ cyl + gear,
    data = d,
    whichRandom = "gear",
    progress = FALSE
  )
  m1 <- suppressMessages(lme4::lmer(mpg ~ cyl + (1 | gear), data = d))
  m2 <- suppressMessages(convert_bayesian_as_frequentist(m))

  # the last model, as in insight::find_formula()
  expect_equal(formula(m2), formula(m1), ignore_attr = TRUE)
})


test_that("BayesFactor models without random factors to freq keep the old path", {
  skip_if_not_or_load_if_installed("BayesFactor")

  d <- mtcars
  d$cyl <- factor(d$cyl)

  set.seed(333)
  expect_s3_class(convert_bayesian_as_frequentist(BayesFactor::ttestBF(d$mpg)), "lm")
  m <- BayesFactor::lmBF(mpg ~ cyl, data = d, progress = FALSE)
  expect_equal(
    coef(convert_bayesian_as_frequentist(m)),
    coef(lm(mpg ~ cyl, data = d)),
    tolerance = 1e-6
  )
})
