if (require("rstanarm") && require("emmeans")) {
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
    testthat::expect_equal(length(xci$CI_low), 3, tolerance = 0.2)
    testthat::expect_equal(length(xci$CI_high), 3, tolerance = 0.2)
  })

  test_that("emmGrid equivalence_test", {
    testthat::skip_on_travis()
    xeqtest <- equivalence_test(all_, ci = 0.9, range = c(-0.1, 0.1))
    testthat::expect_equal(length(xeqtest$ROPE_Percentage), 3)
    testthat::expect_equal(length(xeqtest$ROPE_Equivalence), 3)
  })

  test_that("emmGrid estimate_density", {
    testthat::skip_on_travis()
    xestden <- estimate_density(c_, method = "logspline", precision = 5)
    testthat::expect_equal(length(xestden$x), 5)
    testthat::expect_equal(length(log(xestden$y)), 5)
  })

  test_that("emmGrid hdi", {
    testthat::skip_on_travis()
    xhdi <- hdi(all_, ci = 0.95)
    testthat::expect_equal(length(xhdi$CI_low), 3)
    testthat::expect_equal(length(xhdi$CI_high), 3)
    testthat::expect_equal(xhdi$CI_low, all_summ$lower.HPD, tolerance = 0.2)
    testthat::expect_equal(xhdi$CI_high, all_summ$upper.HPD, tolerance = 0.2)
  })

  test_that("emmGrid p_direction", {
    testthat::skip_on_travis()
    xpd <- p_direction(all_, method = "direct")
    testthat::expect_equal(length(xpd$pd), 3)
  })

  test_that("emmGrid p_map", {
    testthat::skip_on_travis()
    xpmap <- p_map(all_, precision = 2^9)
    testthat::expect_equal(length(xpmap$p_MAP), 3)
  })

  test_that("emmGrid mhdior", {
    testthat::skip_on_travis()
    xprope <- mhdior(all_, range = c(-0.1, 0.1), precision = 0.5)
    testthat::expect_equal(length(xprope$mhdior), 3)
  })

  test_that("emmGrid point_estimate", {
    testthat::skip_on_travis()
    xpest <- point_estimate(all_, centrality = "median", dispersion = TRUE)
    testthat::expect_equal(length(xpest$Median), 3)
    testthat::expect_equal(length(xpest$MAD), 3)
    testthat::expect_equal(xpest$Median, all_summ$emmean, tolerance = 0.1)
  })

  test_that("emmGrid rope", {
    testthat::skip_on_travis()
    xrope <- rope(all_, range = "default", ci = .9)
    testthat::expect_equal(length(xrope$ROPE_Percentage), 3)
  })

  test_that("emmGrid bayesfactor_parameters", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(4)
    xsdbf <- bayesfactor_parameters(all_, prior = model)
    testthat::expect_equal(length(log(xsdbf$BF)), 3)
    testthat::expect_warning(bayesfactor_parameters(all_))

    # error - cannot deal with regrid / transform
    testthat::expect_error(bayesfactor_parameters(regrid(all_), prior = model))
  })

  test_that("emmGrid bayesfactor_restricted", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(4)
    hyps <- c("`1` < `2`", "`1` < 0")
    xrbf <- bayesfactor_restricted(em_, prior = model, hypothesis = hyps)
    testthat::expect_equal(length(log(xrbf$BF)), 2)
    testthat::expect_equal(length(xrbf$Prior_prob), 2)
    testthat::expect_equal(length(xrbf$Posterior_prob), 2)
    testthat::expect_warning(bayesfactor_restricted(em_, hypothesis = hyps))
  })

  test_that("emmGrid si", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(4)
    xrsi <- si(em_, prior = model)
    testthat::expect_equal(length(xrsi$CI_low), 2)
    testthat::expect_equal(length(xrsi$CI_high), 2)
  })

  test_that("emmGrid describe_posterior", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(4)
    xpost <- describe_posterior(
      all_,
      centrality = "median", dispersion = TRUE,
      ci = 0.95, ci_method = "hdi",
      test = c("pd", "rope", "bf"),
      rope_range = "default", rope_ci = 0.89,
      bf_prior = model
    )
    testthat::expect_equal(length(log(xpost$BF)), 3)
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
    testthat::skip_on_ci()

    hyps <- c("a < b", "b < c")
    xrbf1 <- bayesfactor_restricted(bayes_sum, fit_bayes, hypothesis = hyps)
    xrbf2 <- bayesfactor_restricted(bayes_sum, bayes_sum_prior, hypothesis = hyps)

    testthat::expect_equal(xrbf1, xrbf2, tolerance = 0.1)
  })


  test_that("emmGrid bayesfactor_parameters", {
    set.seed(333)
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    testthat::skip_on_ci()

    xsdbf1 <- bayesfactor_parameters(bayes_sum, prior = fit_bayes)
    xsdbf2 <- bayesfactor_parameters(bayes_sum, prior = bayes_sum_prior)

    testthat::expect_equal(log(xsdbf1$BF), log(xsdbf2$BF), tolerance = 0.1)
  })

  test_that("emmGrid bayesfactor_parameters / describe w/ nonlinear models", {
    testthat::skip_on_travis()
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(333)

    model <- stan_glm(vs ~ mpg,
                      data = mtcars,
                      family = "binomial",
                      refresh = 0)

    probs <- emmeans(model, "mpg", type = "resp")
    link <- emmeans(model, "mpg")

    bfp1 <- bayesfactor_parameters(probs, prior = model, null = 0.5)
    bfp2 <- bayesfactor_parameters(link, prior = model, null = 0)

    testthat::expect_equal(length(bfp1$BF), 1)
    testthat::expect_equal(length(bfp2$BF), 1)
    testthat::expect_error(bayesfactor_parameters(regrid(link), prior = model))

    hdip1 <- hdi(probs, ci = 0.9)
    hdip2 <- hdi(link, ci = 0.9)
    testthat::expect_equal(length(hdip1$CI_low), 1)
    testthat::expect_equal(length(hdip1$CI_high), 1)
    testthat::expect_equal(length(hdip2$CI_low), 1)
    testthat::expect_equal(length(hdip2$CI_high), 1)
  })
}