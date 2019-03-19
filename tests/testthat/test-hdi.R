context("hdi")

test_that("hdi", {
  testthat::expect_equal(hdi(rnorm_perfect(1000), ci = .90)$CI_low[1], -1.64, tolerance = 0.02)
  testthat::expect_equal(nrow(hdi(rnorm_perfect(1000), ci = c(.80, .90, .95))), 3, tolerance = 0.01)
  testthat::expect_equal(hdi(rnorm_perfect(1000), ci = 1)$CI_low[1], -3.09, tolerance = 0.02)

  testthat::expect_warning(hdi(c(2, 3, NA)))
  testthat::expect_warning(hdi(c(2, 3)))
  testthat::expect_warning(hdi(rnorm_perfect(1000), ci = 0.0000001))
  testthat::expect_warning(hdi(rnorm_perfect(1000), ci = 950))
  testthat::expect_warning(hdi(c(rnorm_perfect(1000, 0, 1), rnorm_perfect(1000, 6, 1), rnorm_perfect(1000, 12, 1)), ci = .10))

  # TODO add tests
  model <- insight::download_model("stanreg_lm_1")
  hdi(model)

  model <- insight::download_model("stanreg_lmerMod_1")
  hdi(model)
})
