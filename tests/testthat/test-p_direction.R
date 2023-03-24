skip_if_offline()

if (requiet("rstanarm") && requiet("brms")) {
  test_that("p_direction", {
    set.seed(333)
    x <- distribution_normal(10000, 1, 1)
    pd <- p_direction(x)
    expect_equal(as.numeric(pd), 0.842, tolerance = 0.1)
    expect_equal(as.numeric(p_direction(x, method = "kernel")), 0.842, tolerance = 0.1)
    expect_equal(nrow(p_direction(data.frame(replicate(4, rnorm(100))))), 4)
    expect_s3_class(pd, "p_direction")
    expect_equal(tail(capture.output(print(pd)), 1), "Probability of Direction: 0.84")
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest) {
    m <- insight::download_model("stanreg_merMod_5")
    p <- insight::get_parameters(m, effects = "all")

    test_that("p_direction", {
      expect_equal(
        p_direction(m, effects = "all")$pd,
        p_direction(p)$pd,
        tolerance = 1e-3
      )
    })

    m <- insight::download_model("brms_zi_3")
    p <- insight::get_parameters(m, effects = "all", component = "all")

    test_that("p_direction", {
      expect_equal(
        p_direction(m, effects = "all", component = "all")$pd,
        p_direction(p)$pd,
        tolerance = 1e-3
      )
    })
  }
}
