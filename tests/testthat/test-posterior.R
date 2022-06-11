.runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"

if (.runThisTest && require("posterior") && require("brms")) {
  model <- insight::download_model("brms_1")

  test_that("mp-posterior-draws", {
    x <- posterior::as_draws(model)
    mp <- describe_posterior(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
        "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
  })

  test_that("mp-posterior-draws_list", {
    x <- posterior::as_draws_list(model)
    mp <- describe_posterior(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
        "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
  })

  test_that("mp-posterior-draws_df", {
    x <- posterior::as_draws_df(model)
    mp <- describe_posterior(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
        "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
  })

  test_that("mp-posterior-draws_matrix", {
    x <- posterior::as_draws_matrix(model)
    mp <- describe_posterior(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
        "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
  })

  test_that("mp-posterior-draws_array", {
    x <- posterior::as_draws_array(model)
    mp <- describe_posterior(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$pd, c(1, 0.9995, 0.9995, 1, 1), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
        "ROPE_low", "ROPE_high", "ROPE_Percentage"
      )
    )
  })
}
