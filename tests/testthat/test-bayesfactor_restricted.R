# bayesfactor_restricted data.frame ---------------------------------------

test_that("bayesfactor_restricted df", {
  prior <- data.frame(
    X = distribution_normal(100),
    X1 = c(distribution_normal(50), distribution_normal(50)),
    X3 = c(distribution_normal(80), distribution_normal(20))
  )

  posterior <- data.frame(
    X = distribution_normal(100, 0.4, 0.2),
    X1 = distribution_normal(100, -0.2, 0.2),
    X3 = distribution_normal(100, 0.2)
  )

  hyps <- c(
    "X > X1 & X1 > X3",
    "X > X1"
  )

  bfr <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior)

  expect_equal(bfr$p_prior, c(0.2, 0.5), tolerance = 0.1)
  expect_equal(bfr$p_posterior, c(0.31, 1), tolerance = 0.1)
  expect_equal(bfr$log_BF, c(0.43, 0.69), tolerance = 0.1)
  expect_equal(exp(bfr$log_BF), bfr$p_posterior / bfr$p_prior, tolerance = 0.1)

  expect_error(bayesfactor_restricted(posterior, prior, hypothesis = "Y < 0"))
})


# bayesfactor_restricted RSTANARM -----------------------------------------


test_that("bayesfactor_restricted RSTANARM", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  suppressWarnings(
    fit_stan <- rstanarm::stan_glm(mpg ~ wt + cyl + am, data = mtcars, refresh = 0, iter = 200)
  )

  hyps <- c(
    "am > 0 & cyl < 0",
    "cyl < 0",
    "wt - cyl > 0"
  )

  set.seed(444)
  fit_p <- suppressMessages(unupdate(fit_stan))
  bfr1 <- bayesfactor_restricted(fit_stan, prior = fit_p, hypothesis = hyps)

  set.seed(444)
  bfr2 <- bayesfactor_restricted(fit_stan, hypothesis = hyps)

  expect_equal(bfr1, bfr2)
})
