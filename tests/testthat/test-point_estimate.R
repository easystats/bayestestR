test_that("point_estimate: stanreg", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  expect_equal(
    point_estimate(m, effects = "all")$Median,
    point_estimate(p)$Median,
    tolerance = 1e-3
  )
})

test_that("point_estimate: brms", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    point_estimate(m, effects = "all", component = "all")$Median,
    point_estimate(p)$Median,
    tolerance = 1e-3
  )
})

