test_that("p_direction", {
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")

  set.seed(333)
  x <- distribution_normal(10000, 1, 1)
  pd <- p_direction(x)
  expect_equal(as.numeric(pd), 0.842, tolerance = 0.1)
  expect_equal(as.numeric(p_direction(x, method = "kernel")), 0.842, tolerance = 0.1)
  expect_equal(nrow(p_direction(data.frame(replicate(4, rnorm(100))))), 4)
  expect_s3_class(pd, "p_direction")
  expect_equal(tail(capture.output(print(pd)), 1), "Probability of Direction: 0.84")
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

