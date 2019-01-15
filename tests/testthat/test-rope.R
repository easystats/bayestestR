context("rope")

test_that("rope", {

  testthat::expect_equal(rope(rnorm_perfect(1000, 0, 1)), 0.08, tolerance = 0.01)
  testthat::expect_equal(rope_test(rnorm_perfect(1000, 0, 1)), "undecided")

  testthat::expect_equal(rope(rnorm_perfect(1000, 2, 0.01)), 0, tolerance = 0.01)
  testthat::expect_equal(rope_test(rnorm_perfect(1000, 2, 0.01)), "rejected")

  testthat::expect_equal(rope(rnorm_perfect(1000, 0, 0.001)), 1, tolerance = 0.01)
  testthat::expect_equal(rope_test(rnorm_perfect(1000, 0, 0.001)), "accepted")
})
