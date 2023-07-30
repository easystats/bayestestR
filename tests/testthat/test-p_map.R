test_that("p_map", {
  x <- distribution_normal(1000, 0.4)
  pmap <- p_map(x)
  expect_equal(as.numeric(pmap), 0.9285376, tolerance = 0.001)
  expect_s3_class(pmap, "p_map")
  expect_s3_class(pmap, "data.frame")
  expect_equal(dim(pmap), c(1, 1))
  expect_output(print(pmap), "MAP-based p-value: 0.929")

  expect_equal(as.numeric(p_map(distribution_normal(1000))), 1, tolerance = 0.1)
  expect_equal(as.numeric(p_map(distribution_normal(1000, 1, 1))), 0.62, tolerance = 0.1)
  expect_equal(as.numeric(p_map(distribution_normal(1000, 2, 1))), 0.15, tolerance = 0.1)
  expect_equal(as.numeric(p_map(distribution_normal(1000, 3, 0.01))), 0, tolerance = 0.1)
})


test_that("p_map", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  expect_equal(
    p_map(m, effects = "all")$p_MAP,
    p_map(p)$p_MAP,
    tolerance = 0.1
  )
})


test_that("p_map", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    p_map(m, effects = "all", component = "all")$p_MAP,
    p_map(p)$p_MAP,
    tolerance = 0.1
  )
})



test_that("p_map | null", {
  x <- distribution_normal(4000, mean = 1)
  expect_equal(p_map(x), 0.6194317, ignore_attr = TRUE, tolerance = 0.01)
  expect_equal(p_map(x, null = 1), 1, ignore_attr = TRUE, tolerance = 0.01)
})
