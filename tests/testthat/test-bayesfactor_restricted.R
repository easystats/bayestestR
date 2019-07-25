context("bayesfactor_restricted")

test_that("bayesfactor_restricted df", {
  set.seed(444)
  prior <- data.frame(
    X = rnorm(100),
    X1 = rnorm(100),
    X3 = rnorm(100)
  )

  posterior <- data.frame(
    X = rnorm(100, .4, .2),
    X1 = rnorm(100, -.2, .2),
    X3 = rnorm(100, .2)
  )

  hyps <- c(
    "X > X1 & X1 > X3",
    "X > X1"
  )

  bfr <- bayestestR::bayesfactor_restricted(posterior, prior, hypothesis = hyps)

  testthat::expect_equal(bfr$Prior_prob, c(1 / 6, 1 / 2), tolerance = 0.1)
  testthat::expect_equal(bfr$Posterior_prob, c(0.32, 0.99), tolerance = 0.1)
  testthat::expect_equal(log(bfr$BF), c(0.52, 0.76), tolerance = 0.1)
  testthat::expect_equal(bfr$BF, bfr$Posterior_prob / bfr$Prior_prob, tolerance = 0.1)

  testthat::expect_error(bayestestR::bayesfactor_restricted(posterior, prior, hypothesis = "Y < 0"))
})


test_that("bayesfactor_restricted RSTANARM", {
  set.seed(444)
  library(rstanarm)
  junk <- capture.output(fit_stan <- stan_glm(mpg ~ wt + cyl + am,
    data = mtcars
  ))

  hyps <- c(
    "am > 0 & cyl < 0",
    "cyl < 0",
    "wt - cyl > 0"
  )
  bfr <- bayestestR::bayesfactor_restricted(fit_stan, hypothesis = hyps)

  testthat::expect_equal(bfr$Prior_prob, c(1 / 4, 1 / 2, 1 / 2), tolerance = 0.1)
  testthat::expect_equal(bfr$Posterior_prob, c(.57, 1, .11), tolerance = 0.1)
  testthat::expect_equal(log(bfr$BF), c(.85, .68, -1.46), tolerance = 0.1)
})
