skip_if_offline()

if (requiet("rstanarm") && requiet("brms") && requiet("rstan")) {
  test_that("effective_sample", {
    brms_1 <- insight::download_model("brms_1")
    res <- effective_sample(brms_1)
    expect_equal(
      res,
      data.frame(
        Parameter = c("b_Intercept", "b_wt", "b_cyl"),
        ESS = c(5242, 2071, 1951),
        stringsAsFactors = F
      )
    )

    brms_null_1 <- insight::download_model("brms_null_1")
    res <- effective_sample(brms_null_1)
    expect_equal(
      res,
      data.frame(
        Parameter = c("b_Intercept"),
        ESS = c(2888),
        stringsAsFactors = F
      )
    )

    brms_null_2 <- insight::download_model("brms_null_2")
    res <- effective_sample(brms_null_2)
    expect_equal(
      res,
      data.frame(
        Parameter = c("b_Intercept"),
        ESS = c(1059),
        stringsAsFactors = F
      )
    )
  })
}
