skip_on_cran()

test_that("equivalence test, rstanarm", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  m <- insight::download_model("stanreg_merMod_5")

  out <- equivalence_test(m, verbose = FALSE)
  expect_snapshot(print(out))

  out <- equivalence_test(
    m,
    range = list(c(-1, 1), "default", c(0, 2), c(-2, 0), "default"),
    verbose = FALSE
  )
  expect_snapshot(print(out))

  expect_error(
    equivalence_test(
      m,
      range = list(c(-1, 1), "default", c(0, 2), c(-2, 0)),
      verbose = FALSE
    ),
    regex = "Length of"
  )
  expect_error(
    equivalence_test(
      m,
      range = list(c(-1, 1), "default", c(0, 2), c(-2, 0), "a"),
      verbose = FALSE
    ),
    regex = "should be 'default'"
  )
})


test_that("equivalence test, df", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  m <- insight::download_model("stanreg_merMod_5")
  params <- as.data.frame(m)[1:5]

  out <- equivalence_test(params, verbose = FALSE)
  expect_snapshot(print(out))

  out <- equivalence_test(
    params,
    range = list(c(-1, 1), "default", c(0, 2), c(-2, 0), "default"),
    verbose = FALSE
  )
  expect_snapshot(print(out))

  expect_error(
    equivalence_test(
      params,
      range = list(c(-1, 1), "default", c(0, 2), c(-2, 0)),
      verbose = FALSE
    ),
    regex = "Length of"
  )
  expect_error(
    equivalence_test(
      params,
      range = list(c(-1, 1), "default", c(0, 2), c(-2, 0), "a"),
      verbose = FALSE
    ),
    regex = "should be 'default'"
  )
})


test_that("equivalence test passes ci_method to rope", {
  set.seed(2026)
  dat <- data.frame(x = rgamma(4000, shape = 2, rate = 1) - 2)

  out_eq <- equivalence_test(dat, ci_method = "HDI", verbose = FALSE)
  out_rope <- rope(dat, ci_method = "HDI", verbose = FALSE)

  hdi_area <- attr(out_rope, "HDI_area")$x
  expect_identical(out_eq$ROPE_Percentage, out_rope$ROPE_Percentage)
  expect_identical(out_eq$HDI_low, hdi_area$CI_low)
  expect_identical(out_eq$HDI_high, hdi_area$CI_high)
})
