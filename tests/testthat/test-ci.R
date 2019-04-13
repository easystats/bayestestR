context("ci")

test_that("ci", {
  testthat::expect_equal(ci(rnorm_perfect(1000), ci = .90)$CI_low[1], -1.6361, tolerance = 0.02)
  testthat::expect_equal(nrow(ci(rnorm_perfect(1000), ci = c(.80, .90, .95))), 3, tolerance = 0.01)
  testthat::expect_equal(ci(rnorm_perfect(1000), ci = 1)$CI_low[1], -3.09, tolerance = 0.02)
  # testthat::expect_equal(length(capture.output(print(ci(rnorm_perfect(1000))))))
  # testthat::expect_equal(length(capture.output(print(ci(rnorm_perfect(1000), ci = c(.80, .90))))))

  testthat::expect_warning(ci(c(2, 3, NA)))
  testthat::expect_warning(ci(c(2, 3)))
  testthat::expect_warning(ci(rnorm_perfect(1000), ci = 950))
})
