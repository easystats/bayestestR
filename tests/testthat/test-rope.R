context("rope")

test_that("rope", {
  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 1))), 8.88, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 1)), "undecided")

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 2, 0.01))), 0, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 2, 0.01)), "rejected")

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 0.001))), 100, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 0.001)), "accepted")

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 1, 1), CI = c(50, 99))$CI_99), 4.94, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 1, 1), CI = c(50, 99))$CI_99, "undecided")

  set.seed(333)
  testthat::expect_is(rope(rnorm_perfect(1000, 0, 1)), "rope")
  testthat::expect_equal(capture.output(print(rope(rnorm_perfect(1000, 0, 1)))), "8.88% of the 90% CI is in ROPE [-0.10, 0.10]")

  testthat::expect_error(rope(rnorm_perfect(1000, 0, 1), bounds = c("A", 0.1)))
  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 1), bounds = c(-0.1, 0.1))), 8.88, tolerance = 0.01)
})
