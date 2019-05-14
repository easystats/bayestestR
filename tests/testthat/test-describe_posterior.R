context("describe_posterior")

test_that("describe_posterior", {
  set.seed(333)
  x <- distribution_normal(1000)
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")
  testthat::expect_equal(dim(rez), c(1, 16))
  rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  testthat::expect_equal(dim(rez), c(2, 16))

  model <- insight::download_model("stanreg_lm_1")

  # x <- data.frame(replicate(4, rnorm(100)))
  # bayesfactor_savagedickey(x)
  # rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all")
  # testthat::expect_equal(dim(rez), c(4, 16))
  # rez <- describe_posterior(x, estimate = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
  # testthat::expect_equal(dim(rez), c(8, 16))

})
