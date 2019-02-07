context("rstanarm")

test_that("rstanarm", {

  set.seed(333)

  library(rstanarm)
  model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)

  testthat::expect_is(hdi(model), "list")
  testthat::expect_is(rope(model), "list")
  testthat::expect_is(equivalence_test(model), "list")
  testthat::expect_is(map_estimate(model), "list")
  testthat::expect_is(p_map(model), "list")
  testthat::expect_is(p_rope(model), "list")
  testthat::expect_is(p_direction(model), "list")

})
