test_that("point_estimate: stanreg", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr2")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")

  expect_equal(
    point_estimate(m, effects = "all")$Median,
    point_estimate(p)$Median,
    tolerance = 1e-3
  )
})

test_that("point_estimate: brms", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr2")

  m <- insight::download_model("brms_zi_3")
  p <- insight::get_parameters(m, effects = "all", component = "all")

  expect_equal(
    point_estimate(m, effects = "all", component = "all")$Median,
    point_estimate(p)$Median,
    tolerance = 1e-3
  )
})

# edge cases
test_that("point_estimate, constant vectors or sparse samples", {
  x <- c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.2, 2.2, 2.2, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5)
  out <- point_estimate(x, centrality = "MAP", verbose = FALSE)
  expect_true(is.na(out$MAP))
  out <- point_estimate(c(3, 3, 3), centrality = "MAP", verbose = FALSE)
  expect_identical(out$MAP, 3)
  expect_message(
    point_estimate(x, centrality = "MAP", verbose = TRUE),
    regex = "Could not calculate MAP estimate"
  )
  expect_message(
    point_estimate(c(3, 3, 3), centrality = "MAP", verbose = TRUE),
    regex = "Data is singular"
  )
})
