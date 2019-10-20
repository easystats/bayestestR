context("distributions")

test_that("distributions", {
  testthat::expect_equal(mean(bayestestR::distribution_normal(10)), 0, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_normal(10, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_beta(10, 1, 1)), 0.5, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_normal(10, 1, 1, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_binomial(10, 0, 0.5)), 0, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_binomial(10, 0, 0.5, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_cauchy(10)), 0, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_cauchy(10, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_chisquared(10, 1)), 0.778, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_chisquared(10, 1, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_gamma(10, 1)), 0.874, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_gamma(10, 1, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_poisson(10)), 0.8, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_poisson(10, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_student(10, 1)), 0, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_student(10, 1, random = TRUE)), 10, tolerance = 0.01)

  testthat::expect_equal(mean(bayestestR::distribution_uniform(10)), 0.5, tolerance = 0.01)
  testthat::expect_equal(length(bayestestR::distribution_uniform(10, random = TRUE)), 10, tolerance = 0.01)
})
