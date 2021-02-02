if (require("rstanarm", quietly = TRUE)) {
  test_that("p_significance", {
    set.seed(333)
    x <- bayestestR::distribution_normal(10000, 1, 1)
    ps <- bayestestR::p_significance(x)
    expect_equal(as.numeric(ps), 0.816, tolerance = 0.1)
    expect_equal(nrow(p_significance(data.frame(replicate(4, rnorm(100))))), 4)
    expect_s3_class(ps, "p_significance")
    expect_equal(tail(capture.output(print(ps)), 1), "ps [0.10] = 81.60%")
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest) {
    if (require("insight")) {
      m <- insight::download_model("stanreg_merMod_5")
      p <- insight::get_parameters(m, effects = "all")

      expect_equal(
        p_significance(m, effects = "all")$ps[1],
        0.99,
        tolerance = 1e-2
      )
    }
  }
}
