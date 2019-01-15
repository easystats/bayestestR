context("overlap")

test_that("overlap", {
  testthat::expect_equal(overlap(rnorm_perfect(100, 0, 1),
                                 rnorm_perfect(100, 0, 1)), 1, tolerance = 0.01)
  testthat::expect_equal(overlap(rnorm_perfect(100, 0, 1),
                                 rnorm_perfect(100, 1, 1)), 0.462, tolerance = 0.01)
  testthat::expect_equal(overlap(rnorm_perfect(100, 0, 1),
                                 rnorm_perfect(100, 10, 1)), 0, tolerance = 0.01)

  testthat::expect_equal(overlap(rnorm_perfect(100, 0, 1),
                                 rnorm_perfect(100, 0, 0.1),
                                 normalize = TRUE,
                                 onesided = TRUE), 0.10, tolerance = 0.01)
  testthat::expect_equal(overlap(rnorm_perfect(100, 0, 0.1),
                                 rnorm_perfect(100, 0, 1),
                                 normalize = TRUE,
                                 onesided = TRUE), 1, tolerance = 0.01)
})
