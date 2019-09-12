context("rstanarm")

test_that("rstanarm", {
  testthat::skip_on_cran()

  set.seed(333)

  library(rstanarm)
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
  testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 20))

  testthat::expect_is(hdi(model), "data.frame")
  testthat::expect_is(ci(model), "data.frame")
  testthat::expect_is(rope(model), "data.frame")
  # testthat::expect_true("equivalence_test" %in% class(equivalence_test(model)))
  testthat::expect_is(map_estimate(model), "data.frame")
  testthat::expect_is(p_map(model), "data.frame")
  testthat::expect_is(p_rope(model), "data.frame")
  testthat::expect_is(p_direction(model), "data.frame")

  # testthat::expect_error(equivalence_test(model, range = c(.1, .3, .5)))
  # print(equivalence_test(model, ci = c(.1, .3, .5)))
})
