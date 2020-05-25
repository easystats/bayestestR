if (requireNamespace("rstanarm", quietly = TRUE)) {
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

    res <- si(posterior, prior, BF = c(1/3, 1, 3))
    testthat::expect_equal(res$CI, c(1/3, 1, 3), tolerance = 0.02)
    testthat::expect_equal(res$CI_low, c(-0.119, 0.039, 0.333), tolerance = 0.02)
    testthat::expect_equal(res$CI_high, c(1.213, 1.053, 0.759), tolerance = 0.02)
  })

  test_that("si.rstanarm", {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    testthat::skip_on_ci()

    set.seed(333)
    library(rstanarm)
    contrasts(sleep$group) <- contr.bayes # see vignette
    stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep, refresh = 0)

    set.seed(333)
    res <- si(stan_model, verbose = FALSE)
    testthat::expect_equal(res$CI_low, c(-0.013, 0.452), tolerance = 0.02)
    testthat::expect_equal(res$CI_high, c(3.168,1.818), tolerance = 0.02)
    testthat::expect_is(res,c("bayestestR_si"))
  })
}