skip_if_offline()

if (requiet("rstanarm") && requiet("brms") && requiet("httr") && requiet("BayesFactor")) {
  # numeric -------------------------------
  test_that("spi", {
    expect_equal(spi(distribution_normal(1000), ci = .90)$CI_low[1], -1.65, tolerance = 0.02)
    expect_equal(nrow(spi(distribution_normal(1000), ci = c(.80, .90, .95))), 3, tolerance = 0.01)
    expect_equal(spi(distribution_normal(1000), ci = 1)$CI_low[1], -3.29, tolerance = 0.02)
    expect_equal(nchar(capture.output(print(spi(distribution_normal(1000))))), 22)
    expect_equal(length(capture.output(print(spi(distribution_normal(1000), ci = c(.80, .90))))), 5)


    expect_error(spi(c(2, 3, NA)))
    expect_warning(spi(c(2, 3)))
    expect_message(spi(distribution_normal(1000), ci = 0.0000001))
    expect_warning(spi(distribution_normal(1000), ci = 950))
    expect_message(spi(c(0, 0, 0)))
  })


  # stanreg ---------------------------
  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  test_that("ci", {
    expect_equal(
      spi(m, ci = c(0.5, 0.8), effects = "all")$CI_low,
      spi(p, ci = c(0.5, 0.8))$CI_low,
      tolerance = 1e-3
    )
  })

  # brms ---------------------------
  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  test_that("spi brms", {
    expect_equal(
      spi(m, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
      spi(p, ci = c(0.5, 0.8))$CI_low,
      tolerance = 1e-3
    )
  })

  # BayesFactor ---------------------------
  mod_bf <- proportionBF(y = 15, N = 25, p = .5)
  p_bf <- insight::get_parameters(mod_bf)

  test_that("ci - BayesFactor", {
    expect_equal(
      spi(mod_bf, ci = c(0.5, 0.8), effects = "all", component = "all")$CI_low,
      spi(p_bf, ci = c(0.5, 0.8))$CI_low,
      tolerance = 0.1
    )
  })
}
