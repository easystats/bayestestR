context("map_estimate")

test_that("map_estimate", {
  testthat::expect_equal(
    as.numeric(map_estimate(distribution_normal(1000))),
    0,
    tolerance = 0.01
  )
})




if (require("insight")) {
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("map_estimate", {
    testthat::expect_equal(
      # fix range to -.1/.1, to compare to data frame method
      rope(m, range = c(-.1, .1), effects = "all")$ROPE_Percentage,
      rope(p)$ROPE_Percentage,
      tolerance = 1e-3
    )
  })

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("map_estimate", {
    testthat::expect_equal(
      rope(m, range = c(-.1, .1), effects = "all", component = "all")$ROPE_Percentage,
      rope(p)$ROPE_Percentage,
      tolerance = 1e-3
    )
  })
}