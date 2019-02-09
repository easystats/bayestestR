context("hdi")

test_that("hdi", {
  testthat::expect_equal(hdi(rnorm_perfect(1000), CI = 90)$CI_low[1], -1.64, tolerance = 0.02)
  testthat::expect_equal(nrow(hdi(rnorm_perfect(1000), CI = c(80, 90, 95))), 3, tolerance = 0.01)
  testthat::expect_equal(hdi(rnorm_perfect(1000), CI = 100)$CI_low[1], -3.09, tolerance = 0.02)

  testthat::expect_warning(hdi(c(2, 3, NA)))
  testthat::expect_warning(hdi(c(2, 3)))
  testthat::expect_warning(hdi(rnorm_perfect(1000), CI = 0.00001))
  testthat::expect_warning(hdi(rnorm_perfect(1000), CI = 950))
  testthat::expect_warning(hdi(c(rnorm_perfect(1000, 0, 1), rnorm_perfect(1000, 6, 1), rnorm_perfect(1000, 12, 1)), CI = 10))
})
