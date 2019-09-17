context("point_estimate")

if (require("insight")) {
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("point_estimate", {
    testthat::expect_equal(
      point_estimate(m, effects = "all")$Median,
      point_estimate(p)$Median,
      tolerance = 1e-3
    )
  })

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("point_estimate", {
    testthat::expect_equal(
      point_estimate(m, effects = "all", component = "all")$Median,
      point_estimate(p)$Median,
      tolerance = 1e-3
    )
  })
}
