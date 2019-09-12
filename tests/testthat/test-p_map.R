context("p_map")

test_that("p_map", {
  testthat::expect_equal(as.numeric(p_map(rnorm_perfect(1000))), 1, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_map(rnorm_perfect(1000, 1, 1))), 0.62, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_map(rnorm_perfect(1000, 2, 1))), 0.15, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_map(rnorm_perfect(1000, 3, 0.01))), 0, tolerance = 0.01)
})


if (require("insight")) {
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("p_map", {
    testthat::expect_equal(
      p_map(m, effects = "all")$p_MAP,
      p_map(p)$p_MAP,
      tolerance = 1e-3
    )
  })

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("p_map", {
    testthat::expect_equal(
      p_map(m, effects = "all", component = "all")$p_MAP,
      p_map(p)$p_MAP,
      tolerance = 1e-3
    )
  })
}
