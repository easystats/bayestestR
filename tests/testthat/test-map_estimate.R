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

  test_that("map_estimate", {
    testthat::expect_equal(
      map_estimate(m, effects = "all")$Parameter,
      colnames(as.data.frame(m))[1:20]
    )
  })

  m <- insight::download_model("brms_zi_3")
  cn <- colnames(as.data.frame(m))
  cn <- cn[!grepl("^(sd_|lp_)", cn)]

  test_that("map_estimate", {
    testthat::expect_equal(map_estimate(m, effects = "all", component = "all")$Parameter, cn)
  })
}