# numeric -------------------------------
test_that("hdi", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr")
  skip_if_not_or_load_if_installed("BayesFactor")

  expect_equal(hdi(distribution_normal(1000), ci = 0.90)$CI_low[1], -1.64, tolerance = 0.02)
  expect_equal(nrow(hdi(distribution_normal(1000), ci = c(0.80, 0.90, 0.95))), 3, tolerance = 0.01)
  expect_equal(hdi(distribution_normal(1000), ci = 1)$CI_low[1], -3.29, tolerance = 0.02)
  expect_identical(nchar(capture.output(print(hdi(distribution_normal(1000))))), 22L)
  expect_length(capture.output(print(hdi(distribution_normal(1000), ci = c(0.80, 0.90)))), 5)

  expect_message(hdi(c(2, 3, NA)))
  expect_message(hdi(c(2, 3)))
  expect_message(hdi(distribution_normal(1000), ci = 0.0000001))
  expect_message(hdi(distribution_normal(1000), ci = 950))
  expect_message(hdi(c(0, 0, 0)))
})


# stanreg ---------------------------
test_that("ci", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr")
  skip_if_not_or_load_if_installed("BayesFactor")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  expect_equal(
    hdi(m, ci = c(0.5, 0.8), effects = "all")$CI_low,
    hdi(p, ci = c(0.5, 0.8))$CI_low,
    tolerance = 1e-3
  )
})

# brms ---------------------------
test_that("rope", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr")
  skip_if_not_or_load_if_installed("BayesFactor")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    hdi(m, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
    hdi(p, ci = c(0.5, 0.8))$CI_low,
    tolerance = 1e-3
  )
})

# BayesFactor ---------------------------
test_that("ci - BayesFactor", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr")
  skip_if_not_or_load_if_installed("BayesFactor")

  mod_bf <- proportionBF(y = 15, N = 25, p = 0.5)
  p_bf <- insight::get_parameters(mod_bf)

  expect_equal(
    hdi(mod_bf, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
    hdi(p_bf, ci = c(0.5, 0.8))$CI_low,
    tolerance = 0.1
  )
})
