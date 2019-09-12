context("p_rope")

test_that("p_rope", {
  testthat::expect_equal(as.numeric(p_rope(x = distribution_normal(1000, mean = 5, sd = 1), range = c(-0.1, 0.1))), 1, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_rope(x = distribution_normal(1000, mean = 1, sd = 1), range = c(-0.1, 0.1))), 0.631, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_rope(x = distribution_normal(1000, mean = -1, sd = 1), range = c(-0.1, 0.1))), 0.631, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_rope(x = distribution_normal(1000, mean = 0, sd = 1), range = c(-0.1, 0.1))), -0.079, tolerance = 0.01)
  testthat::expect_equal(as.numeric(p_rope(x = distribution_normal(1000, mean = 0, sd = 0.01), range = c(-0.1, 0.1))), -1, tolerance = 0.01)
})

