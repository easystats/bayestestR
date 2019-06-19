context("rope")

test_that("rope", {
  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 1))), 8.98, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 1))$ROPE_Equivalence, "Undecided")
  testthat::expect_equal(length(capture.output(print(equivalence_test(rnorm_perfect(1000))))), 7)
  testthat::expect_equal(length(capture.output(print(equivalence_test(rnorm_perfect(1000), ci = c(0.8, 0.9))))), 10)

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 2, 0.01))), 0, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 2, 0.01))$ROPE_Equivalence, "Rejected")

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 0.001))), 100, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 0.001))$ROPE_Equivalence, "Accepted")

  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 0.001), ci = 1)$ROPE_Equivalence, "Accepted")

  print(rope(rnorm(1000, mean = 0, sd = 3), ci = .5))
  testthat::expect_equal(rope(rnorm(1000, mean = 0, sd = 3), ci = c(.1, .5, .9))$CI, c(10, 50, 90))

  x <- equivalence_test(rnorm_perfect(1000, 1, 1), ci = c(.50, .99))
  testthat::expect_equal(x$ROPE_Percentage[2], 4.94, tolerance = 0.01)
  testthat::expect_equal(x$ROPE_Equivalence[2], "Undecided")

  testthat::expect_error(rope(rnorm_perfect(1000, 0, 1), range = c(0.0, 0.1, 0.2)))

  set.seed(333)
  testthat::expect_is(rope(rnorm_perfect(1000, 0, 1)), "rope")
  testthat::expect_error(rope(rnorm_perfect(1000, 0, 1), range = c("A", 0.1)))
  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 1), range = c(-0.1, 0.1))), 8.98, tolerance = 0.01)
})
