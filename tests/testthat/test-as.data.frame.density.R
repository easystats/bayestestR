test_that("as.data.frame.density", {
  expect_s3_class(as.data.frame(density(distribution_normal(1000))), "data.frame")
})
