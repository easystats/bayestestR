if (requireNamespace("rstanarm", quietly = TRUE)) {
  context("p_significance")

  test_that("p_significance", {
    set.seed(333)
    x <- bayestestR::distribution_normal(10000, 1, 1)
    ps <- bayestestR::p_significance(x)
    testthat::expect_equal(as.numeric(ps), 0.816, tolerance = 0.1)
    testthat::expect_equal(nrow(p_significance(data.frame(replicate(4, rnorm(100))))), 4)
    testthat::expect_is(ps, "p_significance")
    testthat::expect_equal(tail(capture.output(print(ps)), 1), "ps [0.10] = 81.60%")
  })


  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"
  if (.runThisTest || Sys.getenv("USER") == "travis") {
    if (require("insight")) {
      m <- insight::download_model("stanreg_merMod_5")
      p <- insight::get_parameters(m, effects = "all")

      testthat::expect_equal(
        p_significance(m, effects = "all")$ps[1],
        0.988,
        tolerance = 1e-3
      )
    }
  }
}
