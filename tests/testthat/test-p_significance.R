if (require("bayestestR", quietly = TRUE) && require("rstanarm", quietly = TRUE) && require("testthat", quietly = TRUE)) {
  test_that("p_significance", {
    # numeric
    set.seed(333)
    x <- bayestestR::distribution_normal(10000, 1, 1)
    ps <- bayestestR::p_significance(x)
    expect_equal(as.numeric(ps), 0.816, tolerance = 0.1)
    expect_equal(nrow(p_significance(data.frame(replicate(4, rnorm(100))))), 4)
    expect_s3_class(ps, "p_significance")
    expect_equal(tail(capture.output(print(ps)), 1), "Practical Significance (threshold: 0.10): 0.82")
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest) {
    if (require("insight")) {
      # stanreg
      m <- insight::download_model("stanreg_merMod_5")

      expect_equal(
        p_significance(m, effects = "all")$ps[1],
        0.99,
        tolerance = 1e-2
      )

      # brms
      m2 <- insight::download_model("brms_1")

      expect_equal(
        p_significance(m2, effects = "all")$ps,
        c(1.0000, 0.9985, 0.9785),
        tolerance = 0.01
      )

    }
  }
}
