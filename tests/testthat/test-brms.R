context("brms")

test_that("brms", {
  testthat::skip_on_travis()

  set.seed(333)

  library(brms)
  model <- brms::brm(mpg ~ wt + cyl, data = mtcars)

  testthat::expect_is(hdi(model), "data.frame")
  testthat::expect_is(rope(model), "data.frame")
  testthat::expect_is(equivalence_test(model), "data.frame")
  testthat::expect_is(map_estimate(model), "data.frame")
  testthat::expect_is(p_map(model), "data.frame")
  testthat::expect_is(p_rope(model), "data.frame")
  testthat::expect_is(p_direction(model), "data.frame")
})
