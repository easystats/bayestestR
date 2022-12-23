if (suppressPackageStartupMessages(require("bayestestR", quietly = TRUE))) {
  test_that("si.numeric", {
    skip_if_not_installed("logspline")

    set.seed(333)
    prior <- distribution_normal(1000, mean = 0, sd = 1)
    posterior <- distribution_normal(1000, mean = .5, sd = .3)

    expect_warning(res <- si(posterior, prior), regexp = "40")
    expect_equal(res$CI_low, 0.043, tolerance = 0.02)
    expect_equal(res$CI_high, 1.053103, tolerance = 0.02)
    expect_s3_class(res, c("bayestestR_si"))

    res <- si(posterior, prior, BF = 3, verbose = FALSE)
    expect_equal(res$CI_low, 0.35, tolerance = 0.02)
    expect_equal(res$CI_high, 0.759, tolerance = 0.02)

    res <- si(posterior, prior, BF = 100, verbose = FALSE)
    expect_true(all(is.na(res$CI_low)))
    expect_true(all(is.na(res$CI_high)))

    res <- si(posterior, prior, BF = c(1 / 3, 1, 3), verbose = FALSE)
    expect_equal(res$CI, c(1 / 3, 1, 3), tolerance = 0.02)
    expect_equal(res$CI_low, c(-0.1277, 0.0426, 0.3549), tolerance = 0.02)
    expect_equal(res$CI_high, c(1.213, 1.053, 0.759), tolerance = 0.02)
  })

  test_that("si.rstanarm", {
    skip_on_cran()
    skip_if_not_installed("rstanarm")

    data(sleep)
    contrasts(sleep$group) <- contr.equalprior_pairs # See vignette
    stan_model <- rstanarm::stan_glmer(extra ~ group + (1 | ID), data = sleep, refresh = 0)

    set.seed(333)
    stan_model_p <- update(stan_model, prior_PD = TRUE)
    res1 <- si(stan_model, stan_model_p, verbose = FALSE)

    set.seed(333)
    res2 <- si(stan_model, verbose = FALSE)

    expect_s3_class(res1, c("bayestestR_si"))
    expect_equal(res1, res2)

    skip_if_not_installed("emmeans")
    set.seed(123)
    group_diff <- pairs(emmeans::emmeans(stan_model, ~group))
    res3 <- si(group_diff, prior = stan_model, verbose = FALSE)

    expect_equal(res3$CI_low, -2.746, tolerance = 0.3)
    expect_equal(res3$CI_high, -0.4, tolerance = 0.3)
  })
}
