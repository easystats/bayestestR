test_that("simulate_correlation", {
  set.seed(333)
  data <- simulate_correlation(r = 0.5, n = 50)
  testthat::expect_equal(as.numeric(cor.test(data$V1, data$V2)$estimate), 0.5, tol = 0.001)

  data <- simulate_correlation(r = 0.5, n = 50, mean = c(0, 1), sd = c(0.7, 1.7))
  testthat::expect_equal(as.numeric(cor.test(data$V1, data$V2)$estimate), 0.5, tol = 0.001)
  testthat::expect_equal(c(mean(data$V1), sd(data$V1)), c(0, 0.7), tol = 0.001)
  testthat::expect_equal(c(mean(data$V2), sd(data$V2)), c(1, 1.7), tol = 0.001)

  cor_matrix <- matrix(c(
    1.0, 0.2, 0.4,
    0.2, 1.0, 0.3,
    0.4, 0.3, 1.0
  ),
  nrow = 3
  )

  data <- simulate_correlation(r = cor_matrix)

  testthat::expect_equal(matrix(cor(data), nrow = 3), cor_matrix, tol = 0.001)
})
