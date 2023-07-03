test_that("overlap", {
  set.seed(333)
  x <- distribution_normal(1000, 2, 0.5)
  y <- distribution_normal(1000, 0, 1)

  expect_equal(as.numeric(overlap(x, y)), 0.185, tolerance = 0.01)
  out <- capture.output(print(overlap(x, y)))
  expect_identical(out, c("# Overlap", "", "18.6%"))
})
