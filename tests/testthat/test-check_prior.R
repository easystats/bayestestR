skip_on_os(os = "mac")

test_that("check_prior - stanreg", {
  skip_on_cran()
  skip_on_os(os = c("windows", "mac"))
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("BH")
  skip_if_not_or_load_if_installed("RcppEigen")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  model1 <- insight::download_model("stanreg_lm_1")

  expect_identical(
    check_prior(model1)$Prior_Quality,
    c("informative", "uninformative")
  )

  expect_identical(
    check_prior(model1, method = "lakeland")$Prior_Quality,
    c("informative", "informative")
  )
})

test_that("check_prior - brms (linux)", {
  skip("TODO: check hard-coded values")

  skip_on_cran()
  skip_on_os(os = c("windows", "mac", "solaris"))
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("BH")
  skip_if_not_or_load_if_installed("RcppEigen")
  skip_if_not_or_load_if_installed("brms")

  # all `brms` examples in circus have uniform prior distribution, so
  # need to use a custom example here
  set.seed(333)
  suppressMessages({
    model2 <- brm(rating ~ period + carry + cs(treat),
      data = inhaler, family = sratio("logit"),
      prior = set_prior("normal(0,5)"),
      chains = 2, silent = TRUE, refresh = 0
    )
  })

  expect_warning(expect_identical(
    check_prior(model2)$Prior_Quality,
    c(
      "uninformative", "informative", "informative", "uninformative",
      "uninformative", "not determinable", "not determinable", "not determinable"
    )
  ))

  expect_warning(expect_identical(
    check_prior(model2, method = "lakeland")$Prior_Quality,
    c(
      "informative", "informative", "informative", "informative",
      "informative", "not determinable", "not determinable", "not determinable"
    )
  ))
})

test_that("check_prior - brms (linux)", {
  skip_on_cran()
  skip_on_os(os = c("windows", "mac", "solaris"))
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("BH")
  skip_if_not_or_load_if_installed("RcppEigen")
  skip_if_not_or_load_if_installed("brms")

  # all `brms` examples in circus have uniform prior distribution, so
  # need to use a custom example here
  set.seed(333)
  model2 <- brm(rating ~ period + carry + cs(treat),
    data = inhaler, family = sratio("logit"),
    prior = set_prior("normal(0,5)"),
    chains = 2, silent = TRUE, refresh = 0
  )

  # TODO: check hard-coded values
  expect_warning(expect_identical(
    check_prior(model2)$Prior_Quality,
    c(
      "uninformative", "informative", "informative", "uninformative",
      "uninformative", "not determinable", "not determinable", "not determinable"
    )
  ))

  expect_warning(expect_identical(
    check_prior(model2, method = "lakeland")$Prior_Quality,
    c(
      "informative", "misinformative", "informative", "informative",
      "informative", "not determinable", "not determinable", "not determinable"
    )
  ))
})

test_that("check_prior - brms (not linux or windows)", {
  skip_on_cran()
  skip_on_os(os = c("linux", "windows", "mac"))
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("BH")
  skip_if_not_or_load_if_installed("RcppEigen")
  skip_if_not_or_load_if_installed("brms")

  # all `brms` examples in circus have uniform prior distribution, so
  # need to use a custom example here
  set.seed(333)
  suppressMessages({
    model2 <- brm(rating ~ period + carry + cs(treat),
      data = inhaler, family = sratio("logit"),
      prior = set_prior("normal(0,5)"),
      chains = 2, silent = TRUE, refresh = 0
    )
  })

  expect_warning(expect_identical(
    check_prior(model2)$Prior_Quality,
    c(
      "uninformative", "uninformative", "informative", "uninformative",
      "uninformative", "not determinable", "not determinable", "not determinable"
    )
  ))

  expect_warning(expect_identical(
    check_prior(model2, method = "lakeland")$Prior_Quality,
    c(
      "informative", "informative", "informative", "informative",
      "informative", "not determinable", "not determinable", "not determinable"
    )
  ))
})
