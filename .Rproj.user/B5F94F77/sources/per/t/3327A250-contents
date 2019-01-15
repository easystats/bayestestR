context("bayes_p")

test_that("bayes_p", {
  testthat::expect_equal(bayes_p(rnorm_perfect(1000)), 1, tolerance = 0.01)
  testthat::expect_equal(bayes_p(rnorm_perfect(1000, 1, 1)), 0.62, tolerance = 0.01)
  testthat::expect_equal(bayes_p(rnorm_perfect(1000, 2, 1)), 0.15, tolerance = 0.01)
  testthat::expect_equal(bayes_p(rnorm_perfect(1000, 3, 0.01)), 0, tolerance = 0.01)
})
