# numeric -------------------------------
test_that("hdi method density", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("BayesFactor")

  expect_equal(
    hdi(distribution_normal(1000), ci = 0.90, method = "density")$CI_low[1],
    -1.65,
    tolerance = 0.02
  )
  expect_equal(
    nrow(hdi(distribution_normal(1000), ci = c(0.80, 0.90, 0.95), method = "density")),
    3,
    tolerance = 0.01
  )
  expect_equal(
    hdi(distribution_normal(1000), ci = 1, method = "density")$CI_low[1],
    -3.29,
    tolerance = 0.02
  )
  expect_equal(
    nchar(capture.output(print(hdi(distribution_normal(1000), method = "density")))),
    22
  )
  expect_equal(
    length(capture.output(print(
      hdi(distribution_normal(1000), ci = c(0.80, 0.90), method = "density")
    ))),
    5
  )

  expect_error(hdi(c(2, 3, NA), method = "density"))
  expect_warning(hdi(c(2, 3), method = "density"), "too short")
  expect_message(hdi(distribution_normal(1000), ci = 0.0000001, method = "density"))
  expect_warning(hdi(distribution_normal(1000), ci = 950, method = "density"))
  expect_message(hdi(c(0, 0, 0), method = "density"))
})


test_that("ci", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("BayesFactor")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  out <- hdi(m, ci = c(0.5, 0.8), effects = "all", method = "density")
  expect_equal(
    out$CI_low,
    hdi(p, ci = c(0.5, 0.8), method = "density")$CI_low,
    tolerance = 1e-3
  )
  expect_s3_class(out, "bayestestR_spi")
})

test_that("hdi method density brms", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("BayesFactor")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    hdi(
      m,
      ci = c(0.5, 0.8),
      effects = "all",
      component = "all",
      method = "density"
    )$CI_low,
    hdi(p, ci = c(0.5, 0.8), method = "density")$CI_low,
    tolerance = 1e-3
  )
})


test_that("ci - BayesFactor", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("BayesFactor")

  mod_bf <- proportionBF(y = 15, N = 25, p = 0.5)
  p_bf <- insight::get_parameters(mod_bf)

  expect_equal(
    hdi(
      mod_bf,
      ci = c(0.5, 0.8),
      effects = "all",
      component = "all",
      method = "density"
    )$CI_low,
    hdi(p_bf, ci = c(0.5, 0.8), method = "density")$CI_low,
    tolerance = 0.1
  )
})


# deprecated spi() ----------------------

test_that("spi() is deprecated and returns hdi(method = 'density')", {
  skip_if_not_installed("quadprog")
  set.seed(123)
  x <- rnorm(1000)
  expect_warning(
    out <- spi(x, ci = c(0.8, 0.95)),
    "hdi(method = \"density\")",
    fixed = TRUE,
    class = "deprecatedWarning"
  )
  expect_identical(out, hdi(x, ci = c(0.8, 0.95), method = "density"))
})
