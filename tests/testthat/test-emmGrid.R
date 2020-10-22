if (require("rstanarm") && require("emmeans")) {
  context("emmGrid_*")

  library(rstanarm)
  library(emmeans)
  set.seed(300)
  model <- stan_glm(extra ~ group, data = sleep, refresh = 0)

  em_ <- emmeans(model, ~group)
  c_ <- pairs(em_)
  emc_ <- emmeans(model, pairwise~group)
  all_ <- rbind(em_, c_)
  all_summ <- summary(all_)

  set.seed(4)
  model_p <- unupdate(model, verbose = FALSE)
  set.seed(300)

# estimate + hdi ----------------------------------------------------------

  test_that("emmGrid hdi", {
    xhdi <- hdi(all_, ci = 0.95)
    testthat::expect_equal(xhdi$CI_low, all_summ$lower.HPD, tolerance = 0.1)
    testthat::expect_equal(xhdi$CI_high, all_summ$upper.HPD, tolerance = 0.1)

    xhdi2 <- hdi(emc_, ci = 0.95)
    testthat::expect_equal(xhdi$CI_low, xhdi2$CI_low)
  })

  test_that("emmGrid point_estimate", {
    xpest <- point_estimate(all_, centrality = "all", dispersion = TRUE)
    testthat::expect_equal(xpest$Median, all_summ$emmean, tolerance = 0.1)

    xpest2 <- point_estimate(emc_, centrality = "all", dispersion = TRUE)
    testthat::expect_equal(xpest$Median, xpest2$Median)
  })



# Basics ------------------------------------------------------------------

  test_that("emmGrid ci", {
    xci <- ci(all_, ci = 0.9)
    testthat::expect_equal(length(xci$CI_low), 3)
    testthat::expect_equal(length(xci$CI_high), 3)
  })

  # test_that("emmGrid eti", {
  #   xeti <- eti(all_, ci = 0.9)
  #   testthat::expect_equal(length(xeti$CI_low), 3)
  #   testthat::expect_equal(length(xeti$CI_high), 3)
  # })

  test_that("emmGrid equivalence_test", {
    xeqtest <- equivalence_test(all_, ci = 0.9, range = c(-0.1, 0.1))
    testthat::expect_equal(length(xeqtest$ROPE_Percentage), 3)
    testthat::expect_equal(length(xeqtest$ROPE_Equivalence), 3)
  })

  test_that("emmGrid estimate_density", {
    xestden <- estimate_density(c_, method = "logspline", precision = 5)
    testthat::expect_equal(length(xestden$x), 5)
  })

  test_that("emmGrid map_estimate", {
    xmapest <- map_estimate(all_, method = "kernel")
    testthat::expect_equal(length(xmapest$MAP_Estimate), 3)
  })

  test_that("emmGrid mhdior", {
    xprope <- mhdior(all_, range = c(-0.1, 0.1), precision = 0.5)
    testthat::expect_equal(length(xprope$mhdior), 3)
  })

  test_that("emmGrid p_direction", {
    xpd <- p_direction(all_, method = "direct")
    testthat::expect_equal(length(xpd$pd), 3)
  })

  test_that("emmGrid p_map", {
    xpmap <- p_map(all_, precision = 2^9)
    testthat::expect_equal(length(xpmap$p_MAP), 3)
  })

  test_that("emmGrid p_rope", {
    xprope <- p_rope(all_, range = c(-0.1,0.1))
    testthat::expect_equal(length(xprope$p_ROPE), 3)
  })

  test_that("emmGrid p_significance", {
    xsig <- p_significance(all_, threshold = c(-0.1,0.1))
    testthat::expect_equal(length(xsig$ps), 3)
  })

  test_that("emmGrid rope", {
    xrope <- rope(all_, range = "default", ci = .9)
    testthat::expect_equal(length(xrope$ROPE_Percentage), 3)
  })


# describe_posterior ------------------------------------------------------

  test_that("emmGrid describe_posterior", {
    testthat::expect_equal(describe_posterior(all_)$median,
                           describe_posterior(emc_)$median)

    testthat::expect_equal(
      describe_posterior(all_, bf_prior = model_p, test = "bf")$BF,
      describe_posterior(emc_, bf_prior = model_p, test = "bf")$BF
    )
  })

# BFs ---------------------------------------------------------------------

  test_that("emmGrid bayesfactor_parameters", {
    testthat::skip_on_cran()
    set.seed(4)
    testthat::expect_equal(
      bayesfactor_parameters(all_, prior = model, verbose = FALSE),
      bayesfactor_parameters(all_, prior = model_p)
    )

    emc_p <- emmeans(model_p, pairwise ~ group)
    xbfp <- bayesfactor_parameters(all_, prior = model_p)
    xbfp2 <- bayesfactor_parameters(emc_, prior = model_p)
    xbfp3 <- bayesfactor_parameters(emc_, prior = emc_p)
    testthat::expect_equal(xbfp$BF, xbfp2$BF)
    testthat::expect_equal(xbfp$BF, xbfp3$BF)


    testthat::expect_warning(bayesfactor_parameters(all_))

    # error - cannot deal with regrid / transform
    testthat::expect_error(bayesfactor_parameters(regrid(all_), prior = model))
  })

  test_that("emmGrid bayesfactor_restricted", {
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(4)
    hyps <- c("`1` < `2`", "`1` < 0")
    xrbf <- bayesfactor_restricted(em_, prior = model_p, hypothesis = hyps)
    testthat::expect_equal(length(log(xrbf$BF)), 2)
    testthat::expect_equal(length(xrbf$Prior_prob), 2)
    testthat::expect_equal(length(xrbf$Posterior_prob), 2)
    testthat::expect_warning(bayesfactor_restricted(em_, hypothesis = hyps))

    xrbf2 <- bayesfactor_restricted(emc_, prior = model_p, hypothesis = hyps)
    testthat::expect_equal(xrbf, xrbf2)
  })

  test_that("emmGrid si", {
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    set.seed(4)

    xrsi <- si(all_, prior = model_p)
    testthat::expect_equal(length(xrsi$CI_low), 3)
    testthat::expect_equal(length(xrsi$CI_high), 3)

    xrsi2 <- si(emc_, prior = model_p)
    testthat::expect_equal(xrsi$CI_low, xrsi2$CI_low)
    testthat::expect_equal(xrsi$CI_high, xrsi2$CI_high)
  })


# For non linear models ---------------------------------------------------


  set.seed(333)
  df <- data.frame(
    G = rep(letters[1:3], each = 2),
    Y = rexp(6)
  )

  fit_bayes <- stan_glm(Y ~ G, data = df,
                        family = Gamma(link = "identity"),
                        refresh = 0)

  fit_bayes_prior <- unupdate(fit_bayes, verbose = FALSE)

  bayes_sum <- emmeans(fit_bayes, ~ G)
  bayes_sum_prior <- emmeans(fit_bayes_prior, ~ G)

  # test_that("emmGrid bayesfactor_restricted2", {
  #   testthat::skip_on_travis()
  #   testthat::skip_on_cran()
  #   testthat::skip_on_ci()
  #
  #   hyps <- c("a < b", "b < c")
  #   xrbf1 <- bayesfactor_restricted(bayes_sum, fit_bayes, hypothesis = hyps)
  #   xrbf2 <- bayesfactor_restricted(bayes_sum, bayes_sum_prior, hypothesis = hyps)
  #
  #   testthat::expect_equal(xrbf1, xrbf2, tolerance = 0.1)
  # })


  test_that("emmGrid bayesfactor_parameters", {
    set.seed(333)

    xsdbf1 <- bayesfactor_parameters(bayes_sum, prior = fit_bayes)
    xsdbf2 <- bayesfactor_parameters(bayes_sum, prior = bayes_sum_prior)

    testthat::expect_equal(log(xsdbf1$BF), log(xsdbf2$BF), tolerance = 0.1)
  })

  # link vs response
  test_that("emmGrid bayesfactor_parameters / describe w/ nonlinear models", {
    testthat::skip_on_cran()

    model <- stan_glm(vs ~ mpg,
                      data = mtcars,
                      family = "binomial",
                      refresh = 0)

    probs <- emmeans(model, "mpg", type = "resp")
    link <- emmeans(model, "mpg")

    probs_summ <- summary(probs)
    link_summ <- summary(link)

    xhdi <- hdi(probs, ci = 0.95)
    xpest <- point_estimate(probs, centrality = "median", dispersion = TRUE)
    testthat::expect_equal(xhdi$CI_low, probs_summ$lower.HPD, tolerance = 0.1)
    testthat::expect_equal(xhdi$CI_high, probs_summ$upper.HPD, tolerance = 0.1)
    testthat::expect_equal(xpest$Median, probs_summ$prob, tolerance = 0.1)


    xhdi <- hdi(link, ci = 0.95)
    xpest <- point_estimate(link, centrality = "median", dispersion = TRUE)
    testthat::expect_equal(xhdi$CI_low, link_summ$lower.HPD, tolerance = 0.1)
    testthat::expect_equal(xhdi$CI_high, link_summ$upper.HPD, tolerance = 0.1)
    testthat::expect_equal(xpest$Median, link_summ$emmean, tolerance = 0.1)
  })
}

