test_that("mp-posterior-draws", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("posterior")
  skip_if_not_or_load_if_installed("brms")

  model <- insight::download_model("brms_1")
  x <- posterior::as_draws(model)
  mp <- describe_posterior(x)
  expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})

test_that("mp-posterior-draws_list", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("posterior")
  skip_if_not_or_load_if_installed("brms")

  model <- insight::download_model("brms_1")
  x <- posterior::as_draws_list(model)
  mp <- describe_posterior(x)
  expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})

test_that("mp-posterior-draws_df", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("posterior")
  skip_if_not_or_load_if_installed("brms")

  model <- insight::download_model("brms_1")
  x <- posterior::as_draws_df(model)
  mp <- describe_posterior(x)
  expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})

test_that("mp-posterior-draws_matrix", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("posterior")
  skip_if_not_or_load_if_installed("brms")

  model <- insight::download_model("brms_1")
  x <- posterior::as_draws_matrix(model)
  mp <- describe_posterior(x)
  expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})

test_that("mp-posterior-draws_array", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("posterior")
  skip_if_not_or_load_if_installed("brms")

  model <- insight::download_model("brms_1")
  x <- posterior::as_draws_array(model)
  mp <- describe_posterior(x)
  expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
  expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})


test_that("mp-posterior-draws_rvar", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("posterior")
  skip_if_not_or_load_if_installed("brms")

  model <- insight::download_model("brms_1")
  # Create random vectors by adding an additional dimension:
  n <- 4 # length of output vector
  set.seed(123)
  x <- rvar(array(rnorm(4000 * n, mean = rep(1:n, each = 4000), sd = 1), dim = c(4000, n)))
  mp <- describe_posterior(x)
  expect_equal(mp$Median, c(0.99503, 1.99242, 2.9899, 3.99362), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("x[1]", "x[2]", "x[3]", "x[4]"))
  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})
