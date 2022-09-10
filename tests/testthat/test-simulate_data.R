test_that("simulate_correlation", {
  set.seed(333)
  data <- simulate_correlation(r = 0.5, n = 50)
  expect_equal(as.numeric(cor.test(data$V1, data$V2)$estimate), 0.5, tolerance = 0.001)

  data <- simulate_correlation(r = 0.5, n = 50, mean = c(0, 1), sd = c(0.7, 1.7))
  expect_equal(as.numeric(cor.test(data$V1, data$V2)$estimate), 0.5, tolerance = 0.001)
  expect_equal(c(mean(data$V1), sd(data$V1)), c(0, 0.7), tolerance = 0.001)
  expect_equal(c(mean(data$V2), sd(data$V2)), c(1, 1.7), tolerance = 0.001)

  cor_matrix <- matrix(
    c(
      1.0, 0.2, 0.4,
      0.2, 1.0, 0.3,
      0.4, 0.3, 1.0
    ),
    nrow = 3
  )

  data <- simulate_correlation(r = cor_matrix)

  expect_equal(matrix(cor(data), nrow = 3), cor_matrix, tolerance = 0.001)
})
