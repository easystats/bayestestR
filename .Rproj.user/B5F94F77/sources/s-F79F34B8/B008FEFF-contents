context("brms")

test_that("brms", {

  set.seed(333)

  library(brms)
  model <- brms::brm(mpg ~ wt + cyl, data = mtcars)

  testthat::expect_is(hdi(model), "list")
  testthat::expect_is(rope(model), "list")
  testthat::expect_is(equivalence_test(model), "list")
  testthat::expect_is(map_estimate(model), "list")
  testthat::expect_is(p_map(model), "list")
  testthat::expect_is(p_rope(model), "list")
  testthat::expect_is(p_direction(model), "list")

})
