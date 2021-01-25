if (require("brms", quietly = TRUE)) {
  test_that("rope_range", {
    model <- brm(mpg ~ wt + gear, data = mtcars, iter = 300)

    testthat::expect_equal(rope_range(model), c(-0.6026948, 0.6026948), tolerance = 0.01)
  })

  test_that("rope_range (multivariate)", {
    model <- brm(mvbind(mpg, disp) ~ wt + gear, data = mtcars, iter = 300)

    testthat::expect_equal(
      rope_range(model), 
      matrix(
        c(-0.602694, 0.602694, -12.393869, 12.393869), 
        nrow = 2L, ncol = 2L,
        dimnames = list(NULL, c("mpg", "disp"))
      ),
      tolerance = 0.01
    )
  })
}
