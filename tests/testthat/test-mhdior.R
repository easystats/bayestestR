
test_that("mhdior", {
  expect_equal(as.numeric(mhdior(x = distribution_normal(1000, mean = 5, sd = 1), range = c(-0.1, 0.1))), 1, tolerance = 0.01)
  expect_equal(as.numeric(mhdior(x = distribution_normal(1000, mean = 1, sd = 1), range = c(-0.1, 0.1))), 0.631, tolerance = 0.01)
  expect_equal(as.numeric(mhdior(x = distribution_normal(1000, mean = -1, sd = 1), range = c(-0.1, 0.1))), 0.631, tolerance = 0.01)
  expect_equal(as.numeric(mhdior(x = distribution_normal(1000, mean = 0, sd = 1), range = c(-0.1, 0.1))), -0.079, tolerance = 0.01)
  expect_equal(as.numeric(mhdior(x = distribution_normal(1000, mean = 0, sd = 0.01), range = c(-0.1, 0.1))), -1, tolerance = 0.01)
})
