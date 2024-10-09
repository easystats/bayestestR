test_that("print.describe_posterior", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr2")

  m <- insight::download_model("brms_zi_3")
  expect_snapshot(describe_posterior(m, verbose = FALSE), variant = "windows")
  expect_snapshot(describe_posterior(m, effects = "all", component = "all", verbose = FALSE), variant = "windows")
})
