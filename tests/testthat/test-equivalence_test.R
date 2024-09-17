test_that("equivalence test", {
  skip_if_offline()
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
