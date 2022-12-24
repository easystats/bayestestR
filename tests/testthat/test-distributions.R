test_that("distributions", {
  tolerance <- 0.01

  expect_equal(mean(distribution_normal(10)), 0, tolerance = tolerance)
  expect_equal(length(distribution_normal(10, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_beta(10, 1, 1)), 0.5, tolerance = tolerance)
  expect_equal(length(distribution_normal(10, 1, 1, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_binomial(10, 0, 0.5)), 0, tolerance = tolerance)
  expect_equal(length(distribution_binomial(10, 0, 0.5, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_cauchy(10)), 0, tolerance = tolerance)
  expect_equal(length(distribution_cauchy(10, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_chisquared(10, 1)), 0.893, tolerance = tolerance)
  expect_equal(length(distribution_chisquared(10, 1, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_gamma(10, 1)), 0.9404, tolerance = tolerance)
  expect_equal(length(distribution_gamma(10, 1, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_poisson(10)), 1, tolerance = tolerance)
  expect_equal(length(distribution_poisson(10, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_student(10, 1)), 0, tolerance = tolerance)
  expect_equal(length(distribution_student(10, 1, random = TRUE)), 10, tolerance = tolerance)

  expect_equal(mean(distribution_uniform(10)), 0.5, tolerance = tolerance)
  expect_equal(length(distribution_uniform(10, random = TRUE)), 10, tolerance = tolerance)
})
