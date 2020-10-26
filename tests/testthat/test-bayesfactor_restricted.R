if (require("rstanarm") &&
  require("BayesFactor") &&
  require("testthat")) {

  # bayesfactor_restricted data.frame ---------------------------------------

  test_that("bayesfactor_restricted df", {
    prior <- data.frame(
      X = distribution_normal(100),
      X1 = c(distribution_normal(50), distribution_normal(50)),
      X3 = c(distribution_normal(80), distribution_normal(20))
    )

    posterior <- data.frame(
      X = distribution_normal(100, .4, .2),
      X1 = distribution_normal(100, -.2, .2),
      X3 = distribution_normal(100, .2)
    )

    hyps <- c(
      "X > X1 & X1 > X3",
      "X > X1"
    )

    bfr <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior)

    testthat::expect_equal(bfr$Prior_prob, c(0.2, 0.5), tolerance = 0.1)
    testthat::expect_equal(bfr$Posterior_prob, c(0.31, 1), tolerance = 0.1)
    testthat::expect_equal(log(bfr$BF), c(0.43, 0.69), tolerance = 0.1)
    testthat::expect_equal(bfr$BF, bfr$Posterior_prob / bfr$Prior_prob, tolerance = 0.1)

    testthat::expect_error(bayesfactor_restricted(posterior, prior, hypothesis = "Y < 0"))
  })


  # bayesfactor_restricted RSTANARM -----------------------------------------


  test_that("bayesfactor_restricted RSTANARM", {
    testthat::skip_on_cran()
    library(rstanarm)
    fit_stan <- stan_glm(mpg ~ wt + cyl + am, data = mtcars, refresh = 0)

    hyps <- c(
      "am > 0 & cyl < 0",
      "cyl < 0",
      "wt - cyl > 0"
    )

    set.seed(444)
    fit_p <- unupdate(fit_stan)
    bfr1 <- bayesfactor_restricted(fit_stan, prior = fit_p, hypothesis = hyps)

    set.seed(444)
    bfr2 <- bayesfactor_restricted(fit_stan, hypothesis = hyps)

    testthat::expect_equal(bfr1, bfr2)
  })
}
