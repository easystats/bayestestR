context("rstanarm")

test_that("rstanarm", {
  set.seed(333)

  library(rstanarm)
  model <- circus::download_model("stanreg_lm_1")
  testthat::expect_equal(rope_range(model)[1], -0.602, tol = 0.1)

  model <- circus::download_model("stanreg_meanfield_lm_1")
  testthat::expect_equal(rope_range(model)[1], -0.602, tol = 0.1)

  model <- circus::download_model("stanreg_fullrank_lm_1")
  testthat::expect_equal(rope_range(model)[1], -0.602, tol = 0.1)

  model <- circus::download_model("stanreg_lmerMod_1")
  testthat::expect_equal(rope_range(model)[1], -0.097, tol = 0.1)

  model <- circus::download_model("stanreg_glm_1")
  testthat::expect_equal(rope_range(model)[1], -0.2912962, tol = 0.1)

  model <- circus::download_model("stanreg_merMod_1")
  testthat::expect_equal(rope_range(model)[1], -0.2912962, tol = 0.1)

  model <- circus::download_model("stanreg_gamm4_1")
  testthat::expect_equal(rope_range(model)[1], -0.043, tol = 0.1)

  testthat::expect_is(hdi(model), "data.frame")
  testthat::expect_is(rope(model), "data.frame")
  testthat::expect_is(equivalence_test(model), "data.frame")
  testthat::expect_is(map_estimate(model), "data.frame")
  testthat::expect_is(p_map(model), "data.frame")
  testthat::expect_is(p_rope(model), "data.frame")
  testthat::expect_is(p_direction(model), "data.frame")
})
