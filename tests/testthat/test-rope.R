test_that("rope", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")

  expect_equal(as.numeric(rope(distribution_normal(1000, 0, 1), verbose = FALSE)), 0.084, tolerance = 0.01)
  expect_equal(equivalence_test(distribution_normal(1000, 0, 1))$ROPE_Equivalence, "Undecided")
  expect_equal(length(capture.output(print(equivalence_test(distribution_normal(1000))))), 9)
  expect_equal(length(capture.output(print(equivalence_test(distribution_normal(1000),
    ci = c(0.8, 0.9)
  )))), 14)

  expect_equal(as.numeric(rope(distribution_normal(1000, 2, 0.01), verbose = FALSE)), 0, tolerance = 0.01)
  expect_equal(equivalence_test(distribution_normal(1000, 2, 0.01))$ROPE_Equivalence, "Rejected")

  expect_equal(as.numeric(rope(distribution_normal(1000, 0, 0.001), verbose = FALSE)), 1, tolerance = 0.01)
  expect_equal(equivalence_test(distribution_normal(1000, 0, 0.001))$ROPE_Equivalence, "Accepted")

  expect_equal(equivalence_test(distribution_normal(1000, 0, 0.001), ci = 1)$ROPE_Equivalence, "Accepted")

  expect_equal(rope(rnorm(1000, mean = 0, sd = 3), ci = c(0.1, 0.5, 0.9), verbose = FALSE)$CI, c(.1, .5, .9))

  x <- equivalence_test(distribution_normal(1000, 1, 1), ci = c(0.50, 0.99))
  expect_equal(x$ROPE_Percentage[2], 0.0484, tolerance = 0.01)
  expect_identical(x$ROPE_Equivalence[2], "Undecided")

  expect_error(rope(distribution_normal(1000, 0, 1), range = c(0.0, 0.1, 0.2)))

  set.seed(333)
  expect_s3_class(rope(distribution_normal(1000, 0, 1), verbose = FALSE), "rope")
  expect_error(rope(distribution_normal(1000, 0, 1), range = c("A", 0.1)))
  expect_equal(
    as.numeric(rope(distribution_normal(1000, 0, 1),
      range = c(-0.1, 0.1)
    )), 0.084,
    tolerance = 0.01
  )
})


test_that("rope", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")
  expect_equal(
    # fix range to -.1/.1, to compare to data frame method
    rope(m, range = c(-0.1, 0.1), effects = "all", verbose = FALSE)$ROPE_Percentage,
    rope(p, verbose = FALSE)$ROPE_Percentage,
    tolerance = 1e-3
  )
})



test_that("rope", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")
  expect_equal(
    rope(m, effects = "all", component = "all", verbose = FALSE)$ROPE_Percentage,
    rope(p, verbose = FALSE)$ROPE_Percentage,
    tolerance = 1e-3
  )
})


skip_if_not_or_load_if_installed("brms")
set.seed(123)
model <- brm(mpg ~ wt + gear, data = mtcars, iter = 500)
rope <- rope(model, verbose = FALSE)

test_that("rope (brms)", {
  expect_equal(rope$ROPE_high, -rope$ROPE_low, tolerance = 0.01)
  expect_equal(rope$ROPE_high[1], 0.6026948)
  expect_equal(rope$ROPE_Percentage, c(0.00, 0.00, 0.50), tolerance = 0.1)
})

model <- brm(bf(mvbind(mpg, disp) ~ wt + gear) + set_rescor(TRUE), data = mtcars, iter = 500, refresh = 0)
rope <- rope(model, verbose = FALSE)

test_that("rope (brms, multivariate)", {
  expect_equal(rope$ROPE_high, -rope$ROPE_low, tolerance = 0.01)
  expect_equal(rope$ROPE_high[1], 0.6026948, tolerance = 0.01)
  expect_equal(rope$ROPE_high[4], 12.3938694, tolerance = 0.01)
  expect_equal(
    rope$ROPE_Percentage,
    c(0, 0, 0.493457, 0.072897, 0, 0.508411),
    tolerance = 0.1
  )
})


test_that("BayesFactor", {
  skip_if_not_or_load_if_installed("BayesFactor")

  mods <- regressionBF(mpg ~ am + cyl, mtcars, progress = FALSE)
  rx <- suppressMessages(rope(mods, verbose = FALSE))
  expect_equal(rx$ROPE_high, -rx$ROPE_low, tolerance = 0.01)
  expect_equal(rx$ROPE_high[1], 0.6026948, tolerance = 0.01)
})
