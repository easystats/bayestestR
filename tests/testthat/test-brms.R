context("brms")

test_that("brms", {
  testthat::skip_on_travis()

  set.seed(333)

  library(brms)
  model <- brm(mpg ~ wt + cyl + (1 | gear), data = mtcars, sample_prior = T)

  testthat::expect_is(hdi(model), "data.frame")
  testthat::expect_is(rope(model), "data.frame")
  testthat::expect_is(equivalence_test(model), "data.frame")
  testthat::expect_is(map_estimate(model), "data.frame")
  testthat::expect_is(p_map(model), "data.frame")
  testthat::expect_is(p_rope(model), "data.frame")
  testthat::expect_is(p_direction(model), "data.frame")

  testthat::expect_equal(colnames(hdi(model)), c("Parameter", "CI", "CI_low", "CI_high"))
  testthat::expect_equal(colnames(hdi(model, effects = "all")), c("Parameter", "CI", "CI_low", "CI_high", "Group"))
  testthat::expect_equal(nrow(equivalence_test(model)), 3)
})
