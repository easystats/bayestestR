.runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
if (.runThisTest) {
  if (requireNamespace("rstanarm", quietly = TRUE) && require("bayestestR") && require("insight")) {
    test_that("rstanarm", {
      testthat::skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanreg_lm_1")
      testthat::expect_equal(rope_range(model)[1], -0.602, tol = 0.1)

      model <- insight::download_model("stanreg_meanfield_lm_1")
      testthat::expect_equal(rope_range(model)[1], -0.602, tol = 0.1)

      model <- insight::download_model("stanreg_fullrank_lm_1")
      testthat::expect_equal(rope_range(model)[1], -0.602, tol = 0.1)

      model <- insight::download_model("stanreg_lmerMod_1")
      testthat::expect_equal(rope_range(model)[1], -0.097, tol = 0.1)

      model <- insight::download_model("stanreg_glm_1")
      testthat::expect_equal(rope_range(model)[1], -0.18, tol = 0.1)

      model <- insight::download_model("stanreg_merMod_1")
      testthat::expect_equal(rope_range(model)[1], -0.18, tol = 0.1)

      model <- insight::download_model("stanreg_gamm4_1")
      testthat::expect_equal(rope_range(model)[1], -0.043, tol = 0.1)

      model <- insight::download_model("stanreg_gam_1")
      params <- describe_posterior(model, centrality = "all", test = "all", dispersion = TRUE)
      testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 22))

      testthat::expect_is(hdi(model), "data.frame")
      testthat::expect_is(ci(model), "data.frame")
      testthat::expect_is(rope(model), "data.frame")
      # testthat::expect_true("equivalence_test" %in% class(equivalence_test(model)))
      testthat::expect_is(map_estimate(model), "data.frame")
      testthat::expect_is(p_map(model), "data.frame")
      testthat::expect_is(mhdior(model), "data.frame")
      testthat::expect_is(p_direction(model), "data.frame")

      # testthat::expect_error(equivalence_test(model, range = c(.1, .3, .5)))
      # print(equivalence_test(model, ci = c(.1, .3, .5)))
    })

    test_that("rstanarm", {
      testthat::skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanreg_glm_3")

      out <- describe_posterior(model, effects = "all", components = "all", centrality = "mean")
      s <- summary(model)
      testthat::expect_equal(s[1:4, 1, drop = TRUE], out$Mean, check.attributes = FALSE, tolerance = 1e-3)
      testthat::expect_equal(s[1:4, 8, drop = TRUE], out$Rhat, check.attributes = FALSE, tolerance = 1e-1)
    })

    test_that("rstanarm", {
      testthat::skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanmvreg_1")

      out <- describe_posterior(model, effects = "fixed", components = "all", centrality = "mean", test = NULL)
      s <- summary(model)
      testthat::expect_equal(s[c(1:2, 5:7), 1, drop = TRUE], out$Mean, check.attributes = FALSE, tolerance = 1e-3)
      testthat::expect_equal(s[c(1:2, 5:7), 10, drop = TRUE], out$Rhat, check.attributes = FALSE, tolerance = 1e-1)
    })
  }
}
