test_that("p_direction", {
  set.seed(333)
  x <- distribution_normal(10000, 1, 1)
  pd <- p_direction(x)
  expect_equal(as.numeric(pd), 0.842, tolerance = 0.1)
  # converstion into frequentist p-value works
  p <- p_direction(x, as_p = TRUE)
  expect_equal(as.numeric(p), pd_to_p(pd$pd), tolerance = 0.1)
  expect_equal(as.vector(p), pd_to_p(pd$pd), tolerance = 0.1)
  # return NA
  expect_true(is.na(as.numeric(p_direction(c(x, NA), remove_na = FALSE))))
  # works
  expect_equal(as.numeric(p_direction(c(x, NA))), 0.8413, tolerance = 0.1)
  expect_equal(as.vector(p_direction(c(x, NA))), 0.8413, tolerance = 0.1)
  # error if only NA
  expect_error(p_direction(c(NA_real_, NA_real_)), regex = "No valid values found")
  expect_equal(as.numeric(p_direction(x, method = "kernel")), 0.842, tolerance = 0.1)
  expect_s3_class(pd, "p_direction")
  expect_s3_class(pd, "data.frame")
  expect_identical(dim(pd), c(1L, 2L))
  expect_identical(
    capture.output(print(pd)),
    c(
      "Probability of Direction", "", "Parameter |     pd", "------------------",
      "Posterior | 84.13%"
    )
  )

  df <- data.frame(replicate(4, rnorm(100)))
  pd <- p_direction(df)
  expect_s3_class(pd, "p_direction")
  expect_s3_class(pd, "data.frame")
  expect_identical(dim(pd), c(4L, 2L))
})


test_that("p_direction", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  expect_equal(
    p_direction(m, effects = "all")$pd,
    p_direction(p)$pd,
    tolerance = 1e-3
  )
  # converstion into frequentist p-value works
  expect_equal(
    p_direction(m, effects = "all", as_p = TRUE)$p,
    pd_to_p(p_direction(m, effects = "all")$pd),
    tolerance = 1e-3
  )
  expect_equal(
    p_direction(m, effects = "all", as_p = TRUE)$p,
    as.numeric(p_direction(m, effects = "all", as_p = TRUE)),
    tolerance = 1e-3
  )
  expect_equal(
    p_direction(m, effects = "all", as_p = TRUE)$p,
    as.vector(p_direction(m, effects = "all", as_p = TRUE)),
    tolerance = 1e-3
  )
})


test_that("p_direction", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    p_direction(m, effects = "all", component = "all")$pd,
    p_direction(p)$pd,
    tolerance = 1e-3
  )
})
