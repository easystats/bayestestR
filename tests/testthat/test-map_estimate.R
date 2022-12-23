skip_if_offline()

if (requireNamespace("rstanarm", quietly = TRUE)) {
  # numeric ----------------------
  test_that("map_estimate", {
    expect_equal(
      as.numeric(map_estimate(distribution_normal(1000))),
      0,
      tolerance = 0.01
    )
  })

  if (require("insight") && require("BayesFactor")) {
    # stanreg ----------------------
    m <- insight::download_model("stanreg_merMod_5")

    test_that("map_estimate", {
      expect_equal(
        map_estimate(m, effects = "all")$Parameter,
        colnames(as.data.frame(m))[1:21]
      )
    })

    # brms ----------------------
    m <- insight::download_model("brms_zi_3")

    test_that("map_estimate", {
      expect_equal(
        map_estimate(m, effects = "all", component = "all")$Parameter,
        c(
          "b_Intercept", "b_child", "b_camper", "r_persons[1,Intercept]",
          "r_persons[2,Intercept]", "r_persons[3,Intercept]", "r_persons[4,Intercept]",
          "sd_persons__Intercept", "b_zi_Intercept", "b_zi_child", "b_zi_camper",
          "r_persons__zi[1,Intercept]", "r_persons__zi[2,Intercept]", "r_persons__zi[3,Intercept]",
          "r_persons__zi[4,Intercept]", "sd_persons__zi_Intercept"
        )
      )
    })

    # BayesFactor -------------
    m <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
    expect_error(map_estimate(m))
  }
}
