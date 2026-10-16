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
  m <- skip_if_fit_failed(brm(qsec ~ 0 + Intercept + mpg, data = mtcars, refresh = 0))
  m1 <- lm(qsec ~ mpg, data = mtcars)
  m2 <- convert_bayesian_as_frequentist(m)

  expect_equal(coef(m1), coef(m2), tolerance = 1e-2)
})


test_that("brms Interaction terms to freq", {
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  m <- skip_if_fit_failed(brm(qsec ~ mpg * as.factor(am), data = mtcars, refresh = 0))
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
  m2 <- convert_bayesian_as_frequentist(m, response = "gear")
  expect_s3_class(m2, "lm")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)

  m1 <- lm(cyl ~ wt + hp, data = d)
  m2 <- bayesian_as_frequentist(m, response = "cyl")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)

  # brms removes the dot from response names, "Sepal.Length" is "SepalLength"
  m <- insight::download_model("brms_mv_4")
  skip_if(is.null(m))
  d <- insight::get_data(m)
  m1 <- lm(Sepal.Length ~ Petal.Length + Sepal.Width + Species, data = d)
  m2 <- convert_bayesian_as_frequentist(m, response = "SepalLength")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)
})


test_that("brms multivariate to freq, `resp` partially matches `response`", {
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))

  withr::local_options(warnPartialMatchArgs = FALSE)
  m1 <- convert_bayesian_as_frequentist(m, response = "gear")
  m2 <- convert_bayesian_as_frequentist(m, resp = "gear")
  expect_identical(coef(m1), coef(m2))
})


test_that("brms multivariate to freq, invalid response", {
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))

  msg <- "(?s)`response`\\s+must\\s+name\\s+one\\s+response.*cyl,\\s+gear,\\s+carb"
  expect_error(convert_bayesian_as_frequentist(m), msg, perl = TRUE)
  expect_error(convert_bayesian_as_frequentist(m, response = "mpg"), msg, perl = TRUE)
  expect_error(
    convert_bayesian_as_frequentist(m, response = c("cyl", "gear")),
    msg,
    perl = TRUE
  )
  # a factor would select a response by its integer code, not by its label
  expect_error(
    convert_bayesian_as_frequentist(m, response = factor("gear")),
    msg,
    perl = TRUE
  )
})


test_that("brms univariate to freq, response is ignored", {
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_1")
  skip_if(is.null(m))

  m1 <- convert_bayesian_as_frequentist(m)
  m2 <- convert_bayesian_as_frequentist(m, response = "x")
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)
})


test_that("brms multivariate to freq, user data drops the rows brms dropped", {
  skip_if_not_or_load_if_installed("brms")

  # missing values in the other response (qsec), in a variable of the other
  # response (hp), and in a column that no formula uses (drat)
  d <- mtcars
  d$qsec[1:4] <- NA
  d$hp[5:6] <- NA
  d$drat[7] <- NA

  set.seed(333)
  m <- skip_if_fit_failed(brm(
    bf(mpg ~ wt) + bf(qsec ~ 0 + Intercept + hp) + set_rescor(FALSE),
    data = d,
    chains = 1,
    iter = 500,
    refresh = 0
  ))

  complete_rows <- d[stats::complete.cases(d[c("mpg", "wt", "qsec", "hp")]), ]
  m1 <- lm(mpg ~ wt, data = complete_rows)
  m2 <- convert_bayesian_as_frequentist(m, response = "mpg", data = d)
  expect_identical(nobs(m2), nrow(insight::get_data(m)))
  expect_identical(nobs(m2), nrow(complete_rows))
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)

  # `0 + Intercept` in one response of a multivariate model
  m1 <- lm(qsec ~ 1 + hp, data = insight::get_data(m))
  m2 <- convert_bayesian_as_frequentist(m, response = "qsec")
  expect_named(coef(m2), c("(Intercept)", "hp"))
  expect_equal(coef(m1), coef(m2), tolerance = 1e-8)
})


test_that("brms multivariate to freq, user data keeps rows with mi() terms", {
  skip_if_not_or_load_if_installed("brms")

  # brms imputes the missing values of qsec, so it keeps all rows
  d <- mtcars
  d$qsec[1:4] <- NA

  set.seed(333)
  m <- skip_if_fit_failed(brm(
    bf(mpg ~ wt) + bf(qsec | mi() ~ hp) + set_rescor(FALSE),
    data = d,
    chains = 1,
    iter = 500,
    refresh = 0
  ))

  m2 <- convert_bayesian_as_frequentist(m, response = "mpg", data = d)
  expect_identical(nobs(m2), nrow(d))
})


test_that("brms to freq, a failed refit gives one error with its cause", {
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("glmmTMB")

  m <- insight::download_model("brms_1")
  skip_if(is.null(m))

  msg <- "(?s)Model\\s+could\\s+not\\s+be\\s+automatically\\s+converted.*refit\\s+broke"

  # the refit can throw an error (lm(), glm()) or return one (other refits)
  refit_throws <- function(...) stop("refit broke", call. = FALSE)
  refit_returns <- function(...) simpleError("refit broke")

  # gaussian family
  local_mocked_bindings(.convert_bayesian_as_frequentist = refit_throws)
  expect_error(convert_bayesian_as_frequentist(m), msg, perl = TRUE)
  local_mocked_bindings(.convert_bayesian_as_frequentist = refit_returns)
  expect_error(convert_bayesian_as_frequentist(m), msg, perl = TRUE)

  # a brms family that is converted to a glmmTMB family
  local_mocked_bindings(get_family = function(...) brms::Beta(), .package = "insight")
  local_mocked_bindings(.convert_bayesian_as_frequentist = refit_throws)
  expect_error(convert_bayesian_as_frequentist(m), msg, perl = TRUE)
  local_mocked_bindings(.convert_bayesian_as_frequentist = refit_returns)
  expect_error(convert_bayesian_as_frequentist(m), msg, perl = TRUE)
})
