context("p_rope")

test_that("p_rope", {

  testthat::expect_equal(p_rope(posterior=rnorm_perfect(1000, mean = 5, sd = 1), bounds = c(-0.1, 0.1)), 0, tolerance = 0.01)
  testthat::expect_equal(p_rope(posterior=rnorm_perfect(1000, mean = 1, sd = 1), bounds = c(-0.1, 0.1)), 0.016, tolerance = 0.01)
  testthat::expect_equal(p_rope(posterior=rnorm_perfect(1000, mean = -1, sd = 1), bounds = c(-0.1, 0.1)), 0.016, tolerance = 0.01)
  testthat::expect_equal(p_rope(posterior=rnorm_perfect(1000, mean = 0, sd = 1), bounds = c(-0.1, 0.1)), 1.126, tolerance = 0.01)
  testthat::expect_equal(p_rope(posterior=rnorm_perfect(1000, mean = 0, sd = 0.01), bounds = c(-0.1, 0.1)), 2, tolerance = 0.01)
})
