skip_if_offline()

if (requiet("rstanarm") && requiet("brms") && requiet("httr")) {
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("point_estimate", {
    expect_equal(
      point_estimate(m, effects = "all")$Median,
      point_estimate(p)$Median,
      tolerance = 1e-3
    )
  })

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("point_estimate", {
    expect_equal(
      point_estimate(m, effects = "all", component = "all")$Median,
      point_estimate(p)$Median,
      tolerance = 1e-3
    )
  })
}
