test_that("pd_to_p", {
  pds <- c(0.7, 0.95, 0.99, 0.5)
  expect_equal(pd_to_p(pds), c(0.6, 0.1, 0.02, 1))
  expect_equal(pd_to_p(pds, direction = 1), c(0.3, 0.05, 0.01, 0.5))

  expect_warning(p <- pd_to_p(0.3), "0.5")
  expect_equal(p, 1)
})
