context("rope")

test_that("rope", {
  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 1))), 8.88, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 1))$ROPE_Equivalence, "undecided")

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 2, 0.01))), 0, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 2, 0.01))$ROPE_Equivalence, "rejected")

  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 0.001))), 100, tolerance = 0.01)
  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 0.001))$ROPE_Equivalence, "accepted")

  testthat::expect_equal(equivalence_test(rnorm_perfect(1000, 0, 0.001), ci = 1)$ROPE_Equivalence, "accepted")


  x <- equivalence_test(rnorm_perfect(1000, 1, 1), ci = c(.50, .99))
  testthat::expect_equal(x$ROPE_Percentage[2], 4.94, tolerance = 0.01)
  testthat::expect_equal(x$ROPE_Equivalence[2], "undecided")

  testthat::expect_error(rope(rnorm_perfect(1000, 0, 1), range = c(0.0, 0.1, 0.2)))

  set.seed(333)
  testthat::expect_is(rope(rnorm_perfect(1000, 0, 1)), "rope")
  testthat::expect_equal(
    capture.output(print(rope(rnorm_perfect(1000, 0, 1)))),
    c("# Proportions of samples inside the ROPE [-0.10, 0.10]", "", " % in ROPE", "      8.88")
  )
  testthat::expect_error(rope(rnorm_perfect(1000, 0, 1), range = c("A", 0.1)))
  testthat::expect_equal(as.numeric(rope(rnorm_perfect(1000, 0, 1), range = c(-0.1, 0.1))), 8.88, tolerance = 0.01)
})
