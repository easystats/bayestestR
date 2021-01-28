if (require("rstanarm", quietly = TRUE) && require("brms", quietly = TRUE)) {
  test_that("rope", {
    expect_equal(as.numeric(rope(distribution_normal(1000, 0, 1))), 0.0898, tolerance = 0.01)
    expect_equal(equivalence_test(distribution_normal(1000, 0, 1))$ROPE_Equivalence, "Undecided")
    expect_equal(length(capture.output(print(equivalence_test(distribution_normal(1000))))), 9)
    expect_equal(length(capture.output(print(equivalence_test(distribution_normal(1000), ci = c(0.8, 0.9))))), 14)

    expect_equal(as.numeric(rope(distribution_normal(1000, 2, 0.01))), 0, tolerance = 0.01)
    expect_equal(equivalence_test(distribution_normal(1000, 2, 0.01))$ROPE_Equivalence, "Rejected")

    expect_equal(as.numeric(rope(distribution_normal(1000, 0, 0.001))), 1, tolerance = 0.01)
    expect_equal(equivalence_test(distribution_normal(1000, 0, 0.001))$ROPE_Equivalence, "Accepted")

    expect_equal(equivalence_test(distribution_normal(1000, 0, 0.001), ci = 1)$ROPE_Equivalence, "Accepted")

    # print(rope(rnorm(1000, mean = 0, sd = 3), ci = .5))
    expect_equal(rope(rnorm(1000, mean = 0, sd = 3), ci = c(.1, .5, .9))$CI, c(10, 50, 90))

    x <- equivalence_test(distribution_normal(1000, 1, 1), ci = c(.50, .99))
    expect_equal(x$ROPE_Percentage[2], 0.0494, tolerance = 0.01)
    expect_equal(x$ROPE_Equivalence[2], "Undecided")

    expect_error(rope(distribution_normal(1000, 0, 1), range = c(0.0, 0.1, 0.2)))

    set.seed(333)
    expect_s3_class(rope(distribution_normal(1000, 0, 1)), "rope")
    expect_error(rope(distribution_normal(1000, 0, 1), range = c("A", 0.1)))
    expect_equal(as.numeric(rope(distribution_normal(1000, 0, 1), range = c(-0.1, 0.1))), 0.0898, tolerance = 0.01)
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest) {
    if (require("insight")) {
      m <- insight::download_model("stanreg_merMod_5")
      p <- insight::get_parameters(m, effects = "all")

      test_that("rope", {
        expect_equal(
          # fix range to -.1/.1, to compare to data frame method
          rope(m, range = c(-.1, .1), effects = "all")$ROPE_Percentage,
          rope(p)$ROPE_Percentage,
          tolerance = 1e-3
        )
      })

      m <- insight::download_model("brms_zi_3")
      p <- insight::get_parameters(m, effects = "all", component = "all")

      test_that("rope", {
        expect_equal(
          rope(m, effects = "all", component = "all")$ROPE_Percentage,
          rope(p)$ROPE_Percentage,
          tolerance = 1e-3
        )
      })
    }
  }
}


if (require("BayesFactor", quietly = TRUE)) {
  mods <- regressionBF(mpg ~ am + cyl, mtcars, progress = FALSE)
  rx <- suppressMessages(rope(mods))
  expect_equal(rx$ROPE_high, -rx$ROPE_low)
  expect_equal(rx$ROPE_high[1], 0.6026948)
}
