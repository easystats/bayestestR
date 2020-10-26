if (require("brms") && require("testthat") && require("insight") && require("httr")) {
  test_that("brms", {
    # testthat::skip_on_travis()
    testthat::skip_on_cran()

    set.seed(333)
    model <- insight::download_model("brms_mixed_1")

    testthat::expect_is(hdi(model), "data.frame")
    testthat::expect_is(ci(model), "data.frame")
    testthat::expect_is(rope(model), "data.frame")
    # testthat::expect_true("equivalence_test" %in% class(equivalence_test(model)))
    testthat::expect_is(map_estimate(model), "data.frame")
    testthat::expect_is(p_map(model), "data.frame")
    testthat::expect_is(mhdior(model), "data.frame")
    testthat::expect_is(p_direction(model), "data.frame")

    testthat::expect_equal(colnames(hdi(model)), c("Parameter", "CI", "CI_low", "CI_high", "Effects", "Component"))
    testthat::expect_equal(colnames(hdi(model, effects = "all")), c("Parameter", "CI", "CI_low", "CI_high", "Effects", "Component"))
    # testthat::expect_equal(nrow(equivalence_test(model)), 2)

    out <- describe_posterior(model, effects = "all", components = "all", centrality = "mean")
    s <- summary(model)
    testthat::expect_identical(colnames(out), c(
      "Parameter", "Effects", "Mean", "CI", "CI_low", "CI_high",
      "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
      "Rhat", "ESS"
    ))
    testthat::expect_equal(s$fixed[, 1, drop = TRUE], out$Mean[1:2], check.attributes = FALSE, tolerance = 1e-3)
    testthat::expect_equal(s$fixed[, 5, drop = TRUE], out$Rhat[1:2], check.attributes = FALSE, tolerance = 1e-1)
    testthat::expect_equal(s$random$cyl[, 1, drop = TRUE], out$Mean[12], check.attributes = FALSE, tolerance = 1e-3)
    testthat::expect_equal(s$random$gear[, 1, drop = TRUE], out$Mean[13:15], check.attributes = FALSE, tolerance = 1e-3)
  })

  test_that("brms", {
    # testthat::skip_on_travis()
    testthat::skip_on_cran()

    set.seed(333)
    model <- insight::download_model("brms_1")

    out <- describe_posterior(model, effects = "all", components = "all", centrality = "mean")
    s <- summary(model)
    testthat::expect_identical(colnames(out), c(
      "Parameter", "Mean", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage", "Rhat", "ESS"
    ))
    testthat::expect_equal(s$fixed[, 1, drop = TRUE], out$Mean[1:3], check.attributes = FALSE, tolerance = 1e-3)
    testthat::expect_equal(s$fixed[, 5, drop = TRUE], out$Rhat[1:3], check.attributes = FALSE, tolerance = 1e-1)
  })

  test_that("brms", {
    # testthat::skip_on_travis()
    testthat::skip_on_cran()

    set.seed(333)
    model <- insight::download_model("brms_mv_2")

    out <- describe_posterior(model, effects = "all", components = "all", centrality = "mean", test = NULL)
    s <- summary(model)
    testthat::expect_identical(colnames(out), c(
      "Parameter", "Effects", "Mean", "CI", "CI_low", "CI_high",
      "Rhat", "ESS"
    ))
    testthat::expect_equal(s$fixed[, 1, drop = TRUE], out$Mean[c(1, 11, 2:5, 12:14)], check.attributes = FALSE, tolerance = 1e-3)
    testthat::expect_equal(s$fixed[, 5, drop = TRUE], out$Rhat[c(1, 11, 2:5, 12:14)], check.attributes = FALSE, tolerance = 1e-1)
  })

  test_that("brms", {
    # testthat::skip_on_travis()
    testthat::skip_on_cran()

    set.seed(333)
    model <- insight::download_model("brms_2")

    out <- describe_posterior(model, effects = "all", components = "all", centrality = "mean", test = NULL)
    s <- summary(model)
    testthat::expect_equal(s$fixed[, 1, drop = TRUE], out$Mean, check.attributes = FALSE, tolerance = 1e-3)
    testthat::expect_equal(s$fixed[, 5, drop = TRUE], out$Rhat, check.attributes = FALSE, tolerance = 1e-1)
  })
}
