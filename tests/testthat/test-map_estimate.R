context("map_estimate")

test_that("map_estimate", {
  testthat::expect_equal(
    as.numeric(map_estimate(distribution_normal(1000))),
    0,
    tolerance = 0.01
  )
})
