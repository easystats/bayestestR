.runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"

if (.runThisTest &&
  requiet("rstanarm") &&
  requiet("testthat") &&
  requiet("bayestestR") &&
  requiet("brms")) {
  skip_on_cran()

  # stanreg --------------------------

  set.seed(333)
  model1 <- insight::download_model("stanreg_lm_1")

  expect_equal(
    check_prior(model1)$Prior_Quality,
    c("informative", "uninformative")
  )

  expect_equal(
    check_prior(model1, method = "lakeland")$Prior_Quality,
    c("informative", "informative")
  )

  # brms -----------------------------

  # all `brms` examples in circus have uniform prior distribution, so
  # need to use a custom example here
  set.seed(333)
  model2 <- brm(rating ~ period + carry + cs(treat),
    data = inhaler, family = sratio("logit"),
    prior = set_prior("normal(0,5)"),
    chains = 2, silent = TRUE, refresh = 0
  )

  linux <- tryCatch({
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Linux" || grepl("^linux", R.version$os)
    } else {
      FALSE
    }
  })

  if (isTRUE(linux)) {
    expect_warning(expect_equal(
      check_prior(model2)$Prior_Quality,
      c(
        "uninformative", "informative", "informative", "uninformative",
        "uninformative", "not determinable", "not determinable", "not determinable"
      )
    ))

    expect_warning(expect_equal(
      check_prior(model2, method = "lakeland")$Prior_Quality,
      c(
        "informative", "informative", "informative", "informative",
        "informative", "not determinable", "not determinable", "not determinable"
      )
    ))
  } else {
    expect_warning(expect_equal(
      check_prior(model2)$Prior_Quality,
      c(
        "uninformative", "uninformative", "informative", "uninformative",
        "uninformative", "not determinable", "not determinable", "not determinable"
      )
    ))

    expect_warning(expect_equal(
      check_prior(model2, method = "lakeland")$Prior_Quality,
      c(
        "informative", "informative", "informative", "informative",
        "informative", "not determinable", "not determinable", "not determinable"
      )
    ))
  }
}
