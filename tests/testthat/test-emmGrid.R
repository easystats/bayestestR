if (requireNamespace("rstanarm", quietly = TRUE) &&
    requireNamespace("emmeans", quietly = TRUE)) {
  context("emmGrid_*")

  library(rstanarm)
  library(emmeans)
  set.seed(300)
  model <- stan_glm(extra ~ group, data = sleep, refresh = 0)

  em_ <- emmeans(model, ~group)
  c_ <- pairs(em_)
  all_ <- rbind(em_, c_)
  all_summ <- summary(all_)



  test_that("emmGrid ci", {
    testthat::skip_on_travis()
    xci <- ci(all_, ci = 0.9)
    testthat::expect_equal(xci$CI_low, c(-0.236749774206338, 1.23103419307697, -2.99025704276072), tolerance = 0.2)
    testthat::expect_equal(xci$CI_high, c(1.83, 3.35, -0.02), tolerance = 0.2)
  })

  test_that("emmGrid equivalence_test", {
    testthat::skip_on_travis()
    xeqtest <- equivalence_test(all_, ci = 0.9, range = c(-0.1, 0.1))
    testthat::expect_equal(xeqtest$ROPE_Percentage, c(0.0553, 0, 0.0183), tolerance = 0.2)
    testthat::expect_equal(xeqtest$ROPE_Equivalence, c("Undecided", "Rejected", "Undecided"))
  })

  test_that("emmGrid estimate_density", {
    testthat::skip_on_travis()
    xestden <- estimate_density(c_, method = "logspline", precision = 5)
    testthat::expect_equal(xestden$x, c(-4.67, -2.91, -1.16, 0.60, 2.35), tolerance = 0.2)
    testthat::expect_equal(log(xestden$y), c(-6.18, -2.12, -0.86, -3.62, -7.90), tolerance = 0.2)
  })

  test_that("emmGrid hdi", {
    testthat::skip_on_travis()
    xhdi <- hdi(all_, ci = 0.95)
    testthat::expect_equal(xhdi$CI_low, c(-0.41, 0.99, -3.23), tolerance = 0.2)
    testthat::expect_equal(xhdi$CI_high, c(2.06, 3.56, 0.28), tolerance = 0.2)
    testthat::expect_equal(xhdi$CI_low, all_summ$lower.HPD, tolerance = 0.2)
    testthat::expect_equal(xhdi$CI_high, all_summ$upper.HPD, tolerance = 0.2)
  })

  test_that("emmGrid p_direction", {
    testthat::skip_on_travis()
    xpd <- p_direction(all_, method = "direct")
    testthat::expect_equal(xpd$pd, c(0.9025, 0.999, 0.952), tolerance = 0.01)
  })

  test_that("emmGrid p_map", {
    testthat::skip_on_travis()
    xpmap <- p_map(all_, precision = 2^9)
    testthat::expect_equal(xpmap$p_MAP, c(0.42, 0, 0.26), tolerance = 0.1)
  })

  test_that("emmGrid mhdior", {
    testthat::skip_on_travis()
    xprope <- mhdior(all_, range = c(-0.1, 0.1), precision = 0.5)
    testthat::expect_equal(xprope$mhdior, c(0.695, 1, 0.87), tolerance = 0.1)
  })

  test_that("emmGrid point_estimate", {
    testthat::skip_on_travis()
    xpest <- point_estimate(all_, centrality = "median", dispersion = TRUE)
    testthat::expect_equal(xpest$Median, c(0.78, 2.29, -1.52), tolerance = 0.1)
    testthat::expect_equal(xpest$MAD, c(0.60, 0.61, 0.88), tolerance = 0.1)
    testthat::expect_equal(xpest$Median, all_summ$emmean, tolerance = 0.1)
  })

  test_that("emmGrid rope", {
    testthat::skip_on_travis()
    xrope <- rope(all_, range = "default", ci = .9)
    testthat::expect_equal(xrope$ROPE_Percentage, c(0.0553, 0, 0.0183), tolerance = 0.1)
  })

  test_that("emmGrid bayesfactor_parameters", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    set.seed(4)
    xsdbf <- bayesfactor_parameters(all_, prior = model)
    testthat::expect_equal(log(xsdbf$BF), c(-2.5756125848835, 1.69713280431204, -0.212277519930343), tolerance = .1)
    testthat::expect_warning(bayesfactor_parameters(all_))
  })

  test_that("emmGrid bayesfactor_restricted", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    set.seed(4)
    hyps <- c("`1` < `2`", "`1` < 0")
    xrbf <- bayesfactor_restricted(em_, prior = model, hypothesis = hyps)
    testthat::expect_equal(log(xrbf$BF), c(0.667225563308528, -1.62521030757486), tolerance = .1)
    testthat::expect_equal(xrbf$Prior_prob, c(0.49775, 0.504), tolerance = 1e-2)
    testthat::expect_equal(xrbf$Posterior_prob, c(0.952, 0.0975), tolerance = 1e-2)
    testthat::expect_warning(bayesfactor_restricted(em_, hypothesis = hyps))
  })

  test_that("emmGrid si", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    set.seed(4)
    xrsi <- si(em_, prior = model)
    testthat::expect_equal(xrsi$CI_low, c(-0.8479125,  0.5738828), tolerance = .1)
    testthat::expect_equal(xrsi$CI_high, c(2.387275, 4.004303), tolerance = .1)
  })

  test_that("emmGrid describe_posterior", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    set.seed(4)
    xpost <- describe_posterior(
      all_,
      centrality = "median", dispersion = TRUE,
      ci = 0.95, ci_method = "hdi",
      test = c("pd", "rope", "bf"),
      rope_range = "default", rope_ci = 0.89,
      bf_prior = model
    )
    testthat::expect_equal(log(xpost$BF), c(-2.58, 2.00, -0.25), tolerance = 0.1)
    testthat::expect_warning(describe_posterior(all_, test = "bf"))
  })

  ## For non linear models
  set.seed(333)
  df <- data.frame(
    G = rep(letters[1:3], each = 2),
    Y = rexp(6)
  )

  fit_bayes <- stan_glm(Y ~ G,
                        data = df,
                        family = Gamma(link = "identity"),
                        refresh = 0
  )
  fit_bayes_prior <- update(fit_bayes, prior_PD = TRUE)

  bayes_sum <- emmeans(fit_bayes, ~G)
  bayes_sum_prior <- emmeans(fit_bayes_prior, ~G)

  test_that("emmGrid bayesfactor_restricted2", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()

    hyps <- c("a < b", "b < c")
    xrbf1 <- bayesfactor_restricted(bayes_sum, fit_bayes, hypothesis = hyps)
    xrbf2 <- bayesfactor_restricted(bayes_sum, bayes_sum_prior, hypothesis = hyps)

    testthat::expect_equal(xrbf1, xrbf2, tolerance = 0.1)
  })


  test_that("emmGrid bayesfactor_parameters", {
    set.seed(333)
    testthat::skip_on_travis()
    testthat::skip_on_cran()

    xsdbf1 <- bayesfactor_parameters(bayes_sum, prior = fit_bayes)
    xsdbf2 <- bayesfactor_parameters(bayes_sum, prior = bayes_sum_prior)

    # testthat::expect_equal(log(xsdbf1$BF), log(xsdbf2$BF), tolerance = 0.1)
  })
}