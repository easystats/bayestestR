test_that("effective_sample, brms", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("rstan")

  brms_1 <- insight::download_model("brms_1")
  skip_if(is.null(brms_1))

  res <- effective_sample(brms_1)
  expect_equal(
    res,
    data.frame(
      Parameter = c("b_Intercept", "b_wt", "b_cyl"),
      ESS = c(5283, 2120, 2001),
      ESS_tail = c(3255, 2003, 2227),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-1
  )

  brms_null_1 <- insight::download_model("brms_null_1")
  skip_if(is.null(brms_null_1))

  res <- effective_sample(brms_null_1)
  expect_equal(
    res,
    data.frame(
      Parameter = "b_Intercept",
      ESS = 2912,
      ESS_tail = 2388,
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-1
  )

  brms_null_2 <- insight::download_model("brms_null_2")
  skip_if(is.null(brms_null_2))

  res <- effective_sample(brms_null_2)
  expect_equal(
    res,
    data.frame(
      Parameter = "b_Intercept",
      ESS = 1098,
      ESS_tail = 954,
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-1
  )
})


test_that("effective_sample, stanreg", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("rstan")

  m1 <- insight::download_model("stanreg_glm_2")
  skip_if(is.null(m1))

  res <- effective_sample(m1)
  expect_equal(
    res,
    data.frame(
      PParameter = c("(Intercept)", "wt", "cyl"),
      ESS_bulk = c(3265, 1628, 1616),
      ESS_tail = c(2228, 2035, 1870),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-1
  )

  m2 <- insight::download_model("stanmvreg_1")
  skip_if(is.null(m2))

  res <- effective_sample(m2)
  expect_equal(
    res,
    data.frame(
      Parameter = c("y1|(Intercept)", "y2|(Intercept)", "y1|year", "y2|sexf", "y2|year"),
      ESS_bulk = c(40, 177, 762, 186, 70),
      ESS_tail = c(85, 65, 406, 94, 155),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-1
  )
})
