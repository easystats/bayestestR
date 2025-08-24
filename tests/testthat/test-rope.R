test_that("rope, vector", {
  expect_equal(as.numeric(rope(distribution_normal(1000, 0, 1), verbose = FALSE)), 0.084, tolerance = 0.01)
  expect_identical(equivalence_test(distribution_normal(1000, 0, 1))$ROPE_Equivalence, "Undecided")
  expect_length(capture.output(print(equivalence_test(distribution_normal(1000)))), 9)
  expect_length(capture.output(print(equivalence_test(distribution_normal(1000), ci = c(0.8, 0.9)))), 14)

  expect_equal(as.numeric(rope(distribution_normal(1000, 2, 0.01), verbose = FALSE)), 0, tolerance = 0.01)
  expect_identical(equivalence_test(distribution_normal(1000, 2, 0.01))$ROPE_Equivalence, "Rejected")

  expect_equal(as.numeric(rope(distribution_normal(1000, 0, 0.001), verbose = FALSE)), 1, tolerance = 0.01)
  expect_identical(equivalence_test(distribution_normal(1000, 0, 0.001))$ROPE_Equivalence, "Accepted")

  expect_identical(equivalence_test(distribution_normal(1000, 0, 0.001), ci = 1)$ROPE_Equivalence, "Accepted")

  expect_equal(rope(rnorm(1000, mean = 0, sd = 3), ci = c(0.1, 0.5, 0.9), verbose = FALSE)$CI, c(0.1, 0.5, 0.9))

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

  set.seed(1234)
  x <- rnorm(4000, sd = 5)
  out <- rope(x, complement = TRUE)
  expect_named(
    out,
    c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Superiority_Percentage",  "Inferiority_Percentage")
  )
  expect_snapshot(print(out))

  out <- p_rope(x, complement = TRUE)
  expect_named(
    out,
    c("ROPE_low", "ROPE_high", "p_ROPE", "p_Superiority", "p_Inferiority")
  )
  expect_equal(out$p_Superiority, 0.497, tolerance = 1e-3)
  expect_equal(out$p_Inferiority, 0.4885, tolerance = 1e-3)
})


test_that("rope, bayes", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
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

  # list range
  expect_equal(
    rope(m, range = list(c(-1, 0.1), "default", "default", c(-1, 1), c(-1.5, -1)))$ROPE_Percentage,
    c(0.15823, 1, 0, 0.3903, 0.38186),
    tolerance = 1e-3
  )

  # named elements, chooses "default" for unnamed
  expect_equal(
    rope(m, range = list(c(-1, 0.1), "default", "default", c(-1, 1), c(-1.5, -1)))$ROPE_Percentage,
    rope(m, range = list("(Intercept)" = c(-1, 0.1), period4 = c(-1.5, -1), period3 = c(-1, 1)))$ROPE_Percentage,
    tolerance = 1e-3
  )

  expect_error(
    rope(m, range = list(c(-0.1, 0.1), c(2, 2))),
    regex = "Length of"
  )
  expect_error(
    rope(m, range = list(c(-0.1, 0.1), c(2, 2), "default", "a", c(1, 3))),
    regex = "should be 'default'"
  )
  expect_error(
    rope(m, range = list("(Intercept)" = c(-1, 0.1), pointout = c(-1.5, -1), period3 = c(-1, 1))),
    regex = "Not all elements"
  )
})


test_that("rope, get_parameters", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
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


test_that("rope BayesFactor", {
  skip_on_cran()
  skip_on_os(c("linux", "mac"))
  skip_if_not_or_load_if_installed("BayesFactor")

  mods <- regressionBF(mpg ~ am + cyl, mtcars, progress = FALSE)
  rx <- suppressMessages(rope(mods, verbose = FALSE))
  expect_equal(rx$ROPE_high, -rx$ROPE_low, tolerance = 0.01)
  expect_equal(rx$ROPE_high[1], 0.6026948, tolerance = 0.01)
})


test_that("rope (brms)", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")
  skip_on_os(c("windows", "mac"))

  set.seed(123)
  model <- suppressWarnings(brms::brm(mpg ~ wt + gear, data = mtcars, iter = 500))
  rope <- rope(model, verbose = FALSE)

  expect_equal(rope$ROPE_high, -rope$ROPE_low, tolerance = 0.01)
  expect_equal(rope$ROPE_high[1], 0.6026948)
  expect_equal(rope$ROPE_Percentage, c(0.00, 0.00, 0.50), tolerance = 0.1)

  out <- describe_posterior(model, complement = TRUE)
  expect_equal(out$Superiority_Percentage, c(1, 0, 0.137895), tolerance = 0.01)
  expect_named(
    out,
    c(
      "Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI",
      "ROPE_low", "ROPE_high", "ROPE_Percentage", "Superiority_Percentage",
      "Inferiority_Percentage", "Rhat", "ESS"
    )
  )
})


test_that("rope (brms, multivariate)", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")
  skip_on_os(c("windows", "mac"))

  model <- suppressWarnings(brm(bf(mvbind(mpg, disp) ~ wt + gear) + set_rescor(TRUE), data = mtcars, iter = 500, refresh = 0))
  rope <- rope(model, verbose = FALSE)

  expect_equal(rope$ROPE_high, -rope$ROPE_low, tolerance = 0.01)
  expect_equal(rope$ROPE_high[1], 0.6026948, tolerance = 0.01)
  expect_equal(rope$ROPE_high[4], 12.3938694, tolerance = 0.01)
  expect_equal(
    rope$ROPE_Percentage,
    c(0, 0, 0.493457, 0.072897, 0, 0.508411),
    tolerance = 0.1
  )
})
