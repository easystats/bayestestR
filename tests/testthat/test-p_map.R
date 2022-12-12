if (requireNamespace("rstanarm", quietly = TRUE)) {
  test_that("p_map", {
    expect_equal(as.numeric(p_map(distribution_normal(1000))), 1, tolerance = 0.1)
    expect_equal(as.numeric(p_map(distribution_normal(1000, 1, 1))), 0.62, tolerance = 0.1)
    expect_equal(as.numeric(p_map(distribution_normal(1000, 2, 1))), 0.15, tolerance = 0.1)
    expect_equal(as.numeric(p_map(distribution_normal(1000, 3, 0.01))), 0, tolerance = 0.1)
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest) {
    if (require("insight")) {
      m <- insight::download_model("stanreg_merMod_5")
      p <- insight::get_parameters(m, effects = "all")

      test_that("p_map", {
        expect_equal(
          p_map(m, effects = "all")$p_MAP,
          p_map(p)$p_MAP,
          tolerance = 0.1
        )
      })

      m <- insight::download_model("brms_zi_3")
      p <- insight::get_parameters(m, effects = "all", component = "all")

      test_that("p_map", {
        expect_equal(
          p_map(m, effects = "all", component = "all")$p_MAP,
          p_map(p)$p_MAP,
          tolerance = 0.1
        )
      })
    }
  }
}

test_that("p_map | null", {
  x <- distribution_normal(4000, mean = 1)
  expect_equal(p_map(x), 0.6194317, ignore_attr = TRUE, tolerance = 0.01)
  expect_equal(p_map(x, null = 1), 1, ignore_attr = TRUE, tolerance = 0.01)
})
