test_that("brms", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  model <- insight::download_model("brms_mixed_1")
  skip_if(is.null(model))

  expect_s3_class(hdi(model), "data.frame")
  expect_s3_class(ci(model), "data.frame")
  expect_s3_class(rope(model, verbose = FALSE), "data.frame")
  expect_s3_class(equivalence_test(model), "equivalence_test")
  expect_s3_class(map_estimate(model), "data.frame")
  expect_s3_class(p_map(model), "data.frame")
  expect_s3_class(p_direction(model), "data.frame")

  expect_named(hdi(model), c("Parameter", "CI", "CI_low", "CI_high", "Effects", "Component"))
  expect_named(hdi(model, effects = "all"), c("Parameter", "CI", "CI_low", "CI_high", "Effects", "Component"))
  expect_identical(nrow(equivalence_test(model)), 2L)

  out <- describe_posterior(model, effects = "all", component = "all", centrality = "mean")
  suppressWarnings({
    s <- summary(model)
  })
  expect_identical(colnames(out), c(
    "Parameter", "Effects", "Component", "Mean", "CI", "CI_low", "CI_high",
    "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
    "Rhat", "ESS"
  ))
  expect_equal(as.vector(s$fixed[, 1, drop = TRUE]), out$Mean[1:2], tolerance = 1e-3)
  expect_equal(as.vector(s$fixed[, 5, drop = TRUE]), out$Rhat[1:2], tolerance = 1e-1)
  expect_equal(as.vector(s$random$cyl[, 1, drop = TRUE]), out$Mean[3], tolerance = 1e-3)
  expect_equal(as.vector(s$random$gear[, 1, drop = TRUE]), out$Mean[4:6], tolerance = 1e-3)
})

test_that("brms", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  model <- insight::download_model("brms_1")
  skip_if(is.null(model))

  out <- describe_posterior(model, effects = "all", component = "all", centrality = "mean")
  s <- summary(model)
  expect_identical(colnames(out), c(
    "Parameter", "Component", "Mean", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
    "ROPE_low", "ROPE_high", "ROPE_Percentage", "Rhat", "ESS"
  ))
  expect_equal(as.vector(s$fixed[, 1, drop = TRUE]), out$Mean[1:3], tolerance = 1e-3)
  expect_equal(as.vector(s$fixed[, 5, drop = TRUE]), out$Rhat[1:3], tolerance = 1e-1)
})

test_that("brms", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  model <- insight::download_model("brms_mv_2")
  skip_if(is.null(model))

  out <- describe_posterior(model, effects = "all", component = "all", centrality = "mean", test = NULL)
  s <- suppressWarnings(summary(model))
  expect_identical(colnames(out), c(
    "Parameter", "Effects", "Component", "Mean", "CI", "CI_low", "CI_high",
    "Rhat", "ESS"
  ))

  known <- s$fixed
  unknown <- out[out$Effects == "fixed" & out$Component == "conditional", ]
  idx <- match(row.names(known), gsub("b_", "", unknown$Parameter, fixed = TRUE))
  unknown <- unknown[idx, ]
  expect_equal(unknown$Mean, known$Estimate, ignore_attr = TRUE)
  expect_equal(unknown$Rhat, known$Rhat, tolerance = 1e-2, ignore_attr = TRUE)
})

test_that("brms", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  model <- insight::download_model("brms_2")
  skip_if(is.null(model))

  out <- describe_posterior(model, effects = "all", component = "all", centrality = "mean", test = NULL)
  s <- summary(model)
  expect_equal(as.vector(s$fixed[, 1, drop = TRUE]), out$Mean, tolerance = 1e-3)
  expect_equal(as.vector(s$fixed[, 5, drop = TRUE]), out$Rhat, tolerance = 1e-1)
})
