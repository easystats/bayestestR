test_that("describe_prior", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("brms")
  skip_if_not_or_load_if_installed("httr2")
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_on_os("linux")

  # Bayes Factor ----------------------------------------

  expect_equal(
    describe_prior(correlationBF(mtcars$wt, mtcars$mpg, rscale = 0.5)),
    structure(list(
      Parameter = "rho", Prior_Distribution = "beta",
      Prior_Location = 2, Prior_Scale = 2
    ), class = "data.frame", row.names = c(
      NA,
      -1L
    ))
  )

  expect_equal(
    describe_prior(ttestBF(mtcars$wt, mu = 3)),
    structure(list(
      Parameter = "Difference", Prior_Distribution = "cauchy",
      Prior_Location = 0, Prior_Scale = 0.707106781186548
    ), class = "data.frame", row.names = c(
      NA,
      -1L
    ))
  )

  expect_equal(
    describe_prior(contingencyTableBF(
      x = table(mtcars$am, mtcars$cyl),
      sampleType = "poisson"
    )),
    structure(list(
      Parameter = "Ratio", Prior_Distribution = "poisson",
      Prior_Location = 0, Prior_Scale = 1
    ), class = "data.frame", row.names = c(
      NA,
      -1L
    ))
  )

  expect_equal(
    describe_prior(contingencyTableBF(
      x = table(mtcars$am, mtcars$cyl),
      sampleType = "indepMulti",
      fixedMargin = "cols",
      priorConcentration = 1.6
    )),
    structure(list(
      Parameter = "Ratio", Prior_Distribution = "independent multinomial",
      Prior_Location = 0, Prior_Scale = 1.6
    ), class = "data.frame", row.names = c(
      NA,
      -1L
    ))
  )

  expect_equal(
    describe_prior(anovaBF(extra ~ group, data = sleep, progress = FALSE)),
    structure(list(Parameter = c(
      "group-1", "group-2", "mu", "sig2",
      "g_group"
    ), Prior_Distribution = c(
      "cauchy", "cauchy", NA, NA,
      NA
    ), Prior_Location = c(0, 0, NA, NA, NA), Prior_Scale = c(
      0.5,
      0.5, NA, NA, NA
    )), row.names = c(NA, -5L), class = "data.frame")
  )

  # brms ----------------------------------------

  mod_brms <- insight::download_model("brms_1")

  expect_equal(
    describe_prior(mod_brms),
    structure(
      list(
        Parameter = c("b_Intercept", "b_wt", "b_cyl", "sigma"),
        Prior_Distribution = c("student_t", "uniform", "uniform", "student_t"),
        Prior_Location = c(19.2, NA, NA, 0),
        Prior_Scale = c(5.4, NA, NA, 5.4),
        Prior_df = c(3, NA, NA, 3)
      ),
      row.names = c(NA, -4L),
      class = "data.frame",
      priors = structure(
        list(
          prior = c(
            "(flat)", "(flat)", "(flat)", "student_t(3, 19.2, 5.4)",
            "student_t(3, 0, 5.4)"
          ),
          class = c("b", "b", "b", "Intercept", "sigma"),
          coef = c("", "cyl", "wt", "", ""),
          group = c("", "", "", "", ""),
          resp = c("", "", "", "", ""),
          dpar = c("", "", "", "", ""),
          nlpar = c("", "", "", "", ""),
          bound = c("", "", "", "", ""),
          source = c(
            "(unknown)", "(vectorized)", "(vectorized)", "(unknown)",
            "(unknown)"
          ),
          Parameter = c("b_", "b_cyl", "b_wt", "b_Intercept", "sigma")
        ),
        special = list(mu = list()),
        row.names = c(NA, -5L),
        sample_prior = "no",
        class = "data.frame"
      )
    ),
    ignore_attr = TRUE,
    tolerance = 1e-2
  )

  # stanreg ----------------------------------------

  mod_stanreg1 <- insight::download_model("stanreg_gamm4_1")
  mod_stanreg2 <- insight::download_model("stanreg_merMod_1")

  expect_equal(
    describe_prior(mod_stanreg1),
    structure(list(
      Parameter = "(Intercept)", Prior_Distribution = "normal",
      Prior_Location = 3.05733333333333, Prior_Scale = 1.08966571234175
    ), row.names = c(
      NA,
      -1L
    ), class = "data.frame")
  )

  expect_equal(
    describe_prior(mod_stanreg2),
    structure(
      list(
        Parameter = c("(Intercept)", "cyl"),
        Prior_Distribution = c(
          "normal",
          "normal"
        ), Prior_Location = c(0, 0),
        Prior_Scale = c(2.5, 1.39983744766986)
      ),
      row.names = c(NA, -2L), class = "data.frame"
    )
  )
})
