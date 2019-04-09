context("map_estimate")

test_that("map_estimate", {
  testthat::expect_equal(
    map_estimate(rnorm_perfect(1000)),
    structure(-0.00368006135440124, MAP_density = 0.389791625903652, class = c("numeric", "MAP")),
    tolerance = 0.01
  )
})
