.runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
if (.runThisTest) {
  if (requiet("rstanarm") && requiet("bayestestR") && requiet("httr") && requiet("insight")) {
    test_that("rstanarm", {
      skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanreg_lm_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.602, tolerance = 0.1)

      model <- insight::download_model("stanreg_meanfield_lm_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.602, tolerance = 0.1)

      model <- insight::download_model("stanreg_fullrank_lm_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.602, tolerance = 0.1)

      model <- insight::download_model("stanreg_lmerMod_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.097, tolerance = 0.1)

      model <- insight::download_model("stanreg_glm_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.18, tolerance = 0.1)

      model <- insight::download_model("stanreg_merMod_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.18, tolerance = 0.1)

      model <- insight::download_model("stanreg_gamm4_1")
      expect_equal(rope_range(model, verbose = FALSE)[1], -0.043, tolerance = 0.1)

      model <- insight::download_model("stanreg_gam_1")
      expect_warning(params <- describe_posterior(model, centrality = "all", test = "all", dispersion = TRUE))
      expect_equal(c(nrow(params), ncol(params)), c(4, 22))

      expect_s3_class(hdi(model), "data.frame")
      expect_s3_class(ci(model), "data.frame")
      expect_s3_class(rope(model, verbose = FALSE), "data.frame")
      # expect_true("equivalence_test" %in% class(equivalence_test(model)))
      expect_s3_class(map_estimate(model), "data.frame")
      expect_s3_class(p_map(model), "data.frame")
      expect_s3_class(p_direction(model), "data.frame")

      # expect_error(equivalence_test(model, range = c(.1, .3, .5)))
      # print(equivalence_test(model, ci = c(.1, .3, .5)))
    })

    test_that("rstanarm", {
      skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanreg_glm_3")

      out <- describe_posterior(model, effects = "all", component = "all", centrality = "mean")
      s <- summary(model)
      expect_identical(colnames(out), c(
        "Parameter", "Mean", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
        "ROPE_low", "ROPE_high", "ROPE_Percentage", "Rhat", "ESS"
      ))
      expect_equal(as.vector(s[1:4, 1, drop = TRUE]), out$Mean, tolerance = 1e-3)
      expect_equal(as.vector(s[1:4, 8, drop = TRUE]), out$Rhat, tolerance = 1e-1)
    })

    test_that("rstanarm", {
      skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanreg_merMod_3")

      out <- describe_posterior(model, effects = "all", component = "all", centrality = "mean")
      s <- summary(model)
      expect_identical(colnames(out), c(
        "Parameter", "Effects", "Mean", "CI", "CI_low", "CI_high",
        "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
        "Rhat", "ESS"
      ))
      expect_equal(as.vector(s[1:8, 1, drop = TRUE]), out$Mean, tolerance = 1e-3)
      expect_equal(as.vector(s[1:8, 8, drop = TRUE]), out$Rhat, tolerance = 1e-1)
    })

    test_that("rstanarm", {
      skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanmvreg_1")

      out <- describe_posterior(model, effects = "fixed", component = "all", centrality = "mean", test = NULL)
      s <- summary(model)
      expect_identical(colnames(out), c(
        "Parameter", "Response", "Mean", "CI", "CI_low", "CI_high",
        "Rhat", "ESS"
      ))
      expect_equal(as.vector(s[c(1:2, 5:7), 1, drop = TRUE]), out$Mean, tolerance = 1e-3)
      expect_equal(as.vector(s[c(1:2, 5:7), 10, drop = TRUE]), out$Rhat, tolerance = 1e-1)
    })


    test_that("rstanarm", {
      skip_on_cran()

      set.seed(333)
      model <- insight::download_model("stanmvreg_1")

      out <- describe_posterior(model, effects = "fixed", component = "all", centrality = "mean", test = NULL, priors = TRUE)
      expect_identical(colnames(out), c(
        "Parameter", "Response", "Mean", "CI", "CI_low", "CI_high",
        "Rhat", "ESS", "Prior_Distribution", "Prior_Location",
        "Prior_Scale"
      ))
      expect_equal(nrow(out), 5)
    })
  }
}
