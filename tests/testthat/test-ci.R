test_that("ci", {
  set.seed(123)
  x <- rnorm(1000, 3, 2)
  expect_error(ci(x, method = "FDI"), regex = "`method` should be 'ETI'")
  out <- capture.output(print(ci(x, method = "SPI")))
  expect_identical(out, "95% SPI: [-1.16, 6.76]")
  out <- capture.output(print(ci(x, method = "BCI")))
  expect_identical(out, "95% ETI: [-0.88, 7.08]")
})


test_that("ci", {
  expect_equal(ci(distribution_normal(1000), ci = 0.90)$CI_low[1], -1.6361, tolerance = 0.02)
  expect_equal(nrow(ci(distribution_normal(1000), ci = c(0.80, 0.90, 0.95))), 3, tolerance = 0.01)
  expect_equal(ci(distribution_normal(1000), ci = 1)$CI_low[1], -3.29, tolerance = 0.02)
  # expect_equal(length(capture.output(print(ci(distribution_normal(1000))))))
  # expect_equal(length(capture.output(print(ci(distribution_normal(1000), ci = c(.80, .90))))))

  expect_equal(ci(c(2, 3, NA))$CI_low, 2.02, tolerance = 1e-2)
  expect_warning(ci(c(2, 3)))
  expect_warning(ci(distribution_normal(1000), ci = 950))

  x <- data.frame(replicate(4, rnorm(100)))
  x <- ci(x, ci = c(0.68, 0.89, 0.95))
  a <- datawizard::reshape_ci(x)
  expect_identical(c(nrow(x), ncol(x)), c(12L, 4L))
  expect_true(all(datawizard::reshape_ci(a) == x))
})


test_that("ci", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("httr")
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  expect_equal(
    ci(m, ci = c(0.5, 0.8), effects = "all")$CI_low,
    ci(p, ci = c(0.5, 0.8))$CI_low,
    tolerance = 1e-3
  )
})


test_that("rope", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("httr")
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    ci(m, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
    ci(p, ci = c(0.5, 0.8))$CI_low,
    tolerance = 1e-3
  )
})
