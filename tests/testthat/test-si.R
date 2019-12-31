context("si")

test_that("si.numeric", {
  set.seed(333)
  prior <- distribution_normal(1000, mean = 0, sd = 1)
  posterior <- distribution_normal(1000, mean = .5, sd = .3)

  res <- si(posterior, prior)
  testthat::expect_equal(res$CI_low, 0.039, tolerance = 0.02)
  testthat::expect_equal(res$CI_high, 1.053, tolerance = 0.02)
  testthat::expect_is(res,c("bayestestR_si"))

  res <- si(posterior, prior, BF = 3)
  testthat::expect_equal(res$CI_low, 0.333, tolerance = 0.02)
  testthat::expect_equal(res$CI_high, 0.759, tolerance = 0.02)

  res <- si(posterior, prior, BF = 100)
  testthat::expect_true(all(is.na(res$CI_low)))
  testthat::expect_true(all(is.na(res$CI_high)))
})

test_that("si.rstanarm", {
  testthat::skip_on_cran()
  set.seed(333)
  library(rstanarm)
  contrasts(sleep$group) <- contr.bayes # see vingette
  stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep, refresh = 0)

  res <- si(stan_model, verbose = FALSE)
  testthat::expect_equal(res$CI_low, c(-0.057,0.417), tolerance = 0.02)
  testthat::expect_equal(res$CI_high, c(3.086,1.819), tolerance = 0.02)
  testthat::expect_is(res,c("bayestestR_si"))
})




