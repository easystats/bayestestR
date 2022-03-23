if (require("rstanarm") && require("httr") && require("brms") && require("insight") && require("testthat")) {
  test_that("ci", {
    expect_equal(ci(distribution_normal(1000), ci = .90)$CI_low[1], -1.6361, tolerance = 0.02)
    expect_equal(nrow(ci(distribution_normal(1000), ci = c(.80, .90, .95))), 3, tolerance = 0.01)
    expect_equal(ci(distribution_normal(1000), ci = 1)$CI_low[1], -3.29, tolerance = 0.02)
    # expect_equal(length(capture.output(print(ci(distribution_normal(1000))))))
    # expect_equal(length(capture.output(print(ci(distribution_normal(1000), ci = c(.80, .90))))))

    expect_equal(ci(c(2, 3, NA))$CI_low, 2.02, tolerance = 1e-2)
    expect_warning(ci(c(2, 3)))
    expect_warning(ci(distribution_normal(1000), ci = 950))

    x <- data.frame(replicate(4, rnorm(100)))
    x <- ci(x, ci = c(0.68, 0.89, 0.95))
    a <- datawizard::reshape_ci(x)
    expect_equal(c(nrow(x), ncol(x)), c(12, 4))
    expect_true(all(datawizard::reshape_ci(a) == x))
  })



  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest) {
    m <- insight::download_model("stanreg_merMod_5")
    p <- insight::get_parameters(m, effects = "all")

    test_that("ci", {
      expect_equal(
        ci(m, ci = c(.5, .8), effects = "all")$CI_low,
        ci(p, ci = c(.5, .8))$CI_low,
        tolerance = 1e-3
      )
    })

    m <- insight::download_model("brms_zi_3")
    p <- insight::get_parameters(m, effects = "all", component = "all")

    test_that("rope", {
      expect_equal(
        ci(m, ci = c(.5, .8), effects = "all", component = "all")$CI_low,
        ci(p, ci = c(.5, .8))$CI_low,
        tolerance = 1e-3
      )
    })
  }
}
