skip_if_offline()

if (requiet("rstanarm") && requiet("brms") && requiet("httr") && requiet("BayesFactor")) {
  # numeric -------------------------------
  test_that("hdi", {
    expect_equal(hdi(distribution_normal(1000), ci = .90)$CI_low[1], -1.64, tolerance = 0.02)
    expect_equal(nrow(hdi(distribution_normal(1000), ci = c(.80, .90, .95))), 3, tolerance = 0.01)
    expect_equal(hdi(distribution_normal(1000), ci = 1)$CI_low[1], -3.29, tolerance = 0.02)
    expect_equal(nchar(capture.output(print(hdi(distribution_normal(1000))))), 22)
    expect_equal(length(capture.output(print(hdi(distribution_normal(1000), ci = c(.80, .90))))), 5)


    expect_warning(hdi(c(2, 3, NA)))
    expect_warning(hdi(c(2, 3)))
    expect_warning(hdi(distribution_normal(1000), ci = 0.0000001))
    expect_warning(hdi(distribution_normal(1000), ci = 950))
    expect_warning(hdi(c(0, 0, 0)))
  })





  # stanreg ---------------------------
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("ci", {
    expect_equal(
      hdi(m, ci = c(0.5, 0.8), effects = "all")$CI_low,
      hdi(p, ci = c(0.5, 0.8))$CI_low,
      tolerance = 1e-3
    )
  })

  # brms ---------------------------
  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("rope", {
    expect_equal(
      hdi(m, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
      hdi(p, ci = c(0.5, 0.8))$CI_low,
      tolerance = 1e-3
    )
  })

  # BayesFactor ---------------------------
  mod_bf <- proportionBF(y = 15, N = 25, p = .5)
  p_bf <- insight::get_parameters(mod_bf)

  test_that("ci - BayesFactor", {
    expect_equal(
      hdi(mod_bf, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
      hdi(p_bf, ci = c(0.5, 0.8))$CI_low,
      tolerance = 0.1
    )
  })
}
