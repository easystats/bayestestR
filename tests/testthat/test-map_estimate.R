# numeric ----------------------
test_that("map_estimate", {
  x <- distribution_normal(1000, 1)
  MAP <- map_estimate(x)
  expect_equal(as.numeric(MAP), 0.997, tolerance = 0.001, ignore_attr = TRUE)
  expect_s3_class(MAP, "map_estimate")
  expect_s3_class(MAP, "data.frame")
  expect_identical(dim(MAP), c(1L, 2L))
  expect_identical(
    capture.output(print(MAP)),
    c(
      "MAP Estimate",
      "",
      "Parameter | MAP_Estimate",
      "------------------------",
      "x         |         1.00"
    )
  )
})

# stanreg ----------------------
test_that("map_estimate", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("BayesFactor")

  m <- insight::download_model("stanreg_merMod_5")
  skip_if(is.null(m))
  expect_identical(
    map_estimate(m, effects = "all")$Parameter,
    colnames(as.data.frame(m))[c(1:5, 21)]
  )
  expect_identical(
    map_estimate(m, effects = "full")$Parameter,
    colnames(as.data.frame(m))[1:21]
  )
})

# brms ----------------------
test_that("map_estimate", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("BayesFactor")

  m <- insight::download_model("brms_zi_3")
  skip_if(is.null(m))
  expect_identical(
    map_estimate(m, effects = "all", component = "all")$Parameter,
    c(
      "b_Intercept", "b_child", "b_camper", "sd_persons__Intercept",
      "b_zi_Intercept", "b_zi_child", "b_zi_camper", "sd_persons__zi_Intercept"
    )
  )
  expect_identical(
    map_estimate(m, effects = "full", component = "all")$Parameter,
    c(
      "b_Intercept", "b_child", "b_camper", "r_persons[1,Intercept]",
      "r_persons[2,Intercept]", "r_persons[3,Intercept]", "r_persons[4,Intercept]",
      "sd_persons__Intercept", "b_zi_Intercept", "b_zi_child", "b_zi_camper",
      "r_persons__zi[1,Intercept]", "r_persons__zi[2,Intercept]", "r_persons__zi[3,Intercept]",
      "r_persons__zi[4,Intercept]", "sd_persons__zi_Intercept"
    )
  )
  m <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
  expect_error(map_estimate(m))
})

# edge cases
test_that("map_estimate, constant vectors or sparse samples", {
  x <- c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.2, 2.2, 2.2, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5)
  out <- map_estimate(x, verbose = FALSE)
  expect_true(is.na(out$MAP_Estimate))
  out <- map_estimate(c(3, 3, 3), verbose = FALSE)
  expect_identical(out$MAP_Estimate, 3)
  expect_message(
    map_estimate(x, verbose = TRUE),
    regex = "Could not calculate MAP estimate"
  )
  expect_message(
    map_estimate(c(3, 3, 3), verbose = TRUE),
    regex = "Data is singular"
  )
})
