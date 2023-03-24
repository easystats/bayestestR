if (requiet("rstanarm") && requiet("emmeans")) {
  set.seed(300)
  model <- stan_glm(extra ~ group,
    data = sleep,
    refresh = 0,
    chains = 6, iter = 7000, warmup = 200
  )

  em_ <- emmeans(model, ~group)
  c_ <- pairs(em_)
  emc_ <- emmeans(model, pairwise ~ group)
  all_ <- rbind(em_, c_)
  all_summ <- summary(all_)

  set.seed(4)
  model_p <- unupdate(model, verbose = FALSE)
  set.seed(300)

  # estimate + hdi ----------------------------------------------------------

  test_that("emmGrid hdi", {
    xhdi <- hdi(all_, ci = 0.95)
    expect_equal(xhdi$CI_low, all_summ$lower.HPD, tolerance = 0.1)
    expect_equal(xhdi$CI_high, all_summ$upper.HPD, tolerance = 0.1)

    xhdi2 <- hdi(emc_, ci = 0.95)
    expect_equal(xhdi$CI_low, xhdi2$CI_low)
  })

  test_that("emmGrid point_estimate", {
    xpest <- point_estimate(all_, centrality = "all", dispersion = TRUE)
    expect_equal(xpest$Median, all_summ$emmean, tolerance = 0.1)

    xpest2 <- point_estimate(emc_, centrality = "all", dispersion = TRUE)
    expect_equal(xpest$Median, xpest2$Median)
  })



  # Basics ------------------------------------------------------------------

  test_that("emmGrid ci", {
    xci <- ci(all_, ci = 0.9)
    expect_equal(length(xci$CI_low), 3)
    expect_equal(length(xci$CI_high), 3)
  })

  # test_that("emmGrid eti", {
  #   xeti <- eti(all_, ci = 0.9)
  #   expect_equal(length(xeti$CI_low), 3)
  #   expect_equal(length(xeti$CI_high), 3)
  # })

  test_that("emmGrid equivalence_test", {
    xeqtest <- equivalence_test(all_, ci = 0.9, range = c(-0.1, 0.1))
    expect_equal(length(xeqtest$ROPE_Percentage), 3)
    expect_equal(length(xeqtest$ROPE_Equivalence), 3)
  })

  test_that("emmGrid estimate_density", {
    xestden <- estimate_density(c_, method = "logspline", precision = 5)
    expect_equal(length(xestden$x), 5)
  })

  test_that("emmGrid map_estimate", {
    xmapest <- map_estimate(all_, method = "kernel")
    expect_equal(length(xmapest$MAP_Estimate), 3)
  })


  test_that("emmGrid p_direction", {
    xpd <- p_direction(all_, method = "direct")
    expect_equal(length(xpd$pd), 3)
  })

  test_that("emmGrid p_map", {
    xpmap <- p_map(all_, precision = 2^9)
    expect_equal(length(xpmap$p_MAP), 3)
  })

  test_that("emmGrid p_rope", {
    xprope <- p_rope(all_, range = c(-0.1, 0.1))
    expect_equal(length(xprope$p_ROPE), 3)
  })

  test_that("emmGrid p_significance", {
    xsig <- p_significance(all_, threshold = c(-0.1, 0.1))
    expect_equal(length(xsig$ps), 3)
  })

  test_that("emmGrid rope", {
    xrope <- rope(all_, range = "default", ci = .9)
    expect_equal(length(xrope$ROPE_Percentage), 3)
  })


  # describe_posterior ------------------------------------------------------

  test_that("emmGrid describe_posterior", {
    expect_equal(
      describe_posterior(all_)$median,
      describe_posterior(emc_)$median
    )

    skip_on_cran()
    expect_equal(
      describe_posterior(all_, bf_prior = model_p, test = "bf")$log_BF,
      describe_posterior(emc_, bf_prior = model_p, test = "bf")$log_BF
    )
  })

  # BFs ---------------------------------------------------------------------

  test_that("emmGrid bayesfactor_parameters", {
    skip_on_cran()
    set.seed(4)
    expect_equal(
      bayesfactor_parameters(all_, prior = model, verbose = FALSE),
      bayesfactor_parameters(all_, prior = model_p, verbose = FALSE),
      tolerance = 0.001
    )

    emc_p <- emmeans(model_p, pairwise ~ group)
    xbfp <- bayesfactor_parameters(all_, prior = model_p, verbose = FALSE)
    xbfp2 <- bayesfactor_parameters(emc_, prior = model_p, verbose = FALSE)
    xbfp3 <- bayesfactor_parameters(emc_, prior = emc_p, verbose = FALSE)
    expect_equal(xbfp$log_BF, xbfp2$log_BF)
    expect_equal(xbfp$log_BF, xbfp3$log_BF)

    expect_warning(
      suppressMessages(
        bayesfactor_parameters(all_)
      ),
      regexp = "Prior not specified"
    )

    # error - cannot deal with regrid / transform

    e <- capture_error(suppressMessages(bayesfactor_parameters(regrid(all_), prior = model)))
    expect_match(as.character(e), "Unable to reconstruct prior estimates")
  })

  test_that("emmGrid bayesfactor_restricted", {
    skip_on_cran()
    skip_on_ci()
    set.seed(4)
    hyps <- c("`1` < `2`", "`1` < 0")
    xrbf <- bayesfactor_restricted(em_, prior = model_p, hypothesis = hyps)
    expect_equal(length(xrbf$log_BF), 2)
    expect_equal(length(xrbf$p_prior), 2)
    expect_equal(length(xrbf$p_posterior), 2)
    expect_warning(bayesfactor_restricted(em_, hypothesis = hyps))

    xrbf2 <- bayesfactor_restricted(emc_, prior = model_p, hypothesis = hyps)
    expect_equal(xrbf, xrbf2)
  })

  test_that("emmGrid si", {
    skip_on_cran()
    set.seed(4)

    xrsi <- si(all_, prior = model_p, verbose = FALSE)
    expect_equal(length(xrsi$CI_low), 3)
    expect_equal(length(xrsi$CI_high), 3)

    xrsi2 <- si(emc_, prior = model_p, verbose = FALSE)
    expect_equal(xrsi$CI_low, xrsi2$CI_low)
    expect_equal(xrsi$CI_high, xrsi2$CI_high)
  })


  # For non linear models ---------------------------------------------------


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

  fit_bayes_prior <- unupdate(fit_bayes, verbose = FALSE)

  bayes_sum <- emmeans(fit_bayes, ~G)
  bayes_sum_prior <- emmeans(fit_bayes_prior, ~G)

  # test_that("emmGrid bayesfactor_restricted2", {
  #   skip_on_cran()
  #   skip_on_ci()
  #
  #   hyps <- c("a < b", "b < c")
  #   xrbf1 <- bayesfactor_restricted(bayes_sum, fit_bayes, hypothesis = hyps, verbose = FALSE)
  #   xrbf2 <- bayesfactor_restricted(bayes_sum, bayes_sum_prior, hypothesis = hyps, verbose = FALSE)
  #
  #   expect_equal(xrbf1, xrbf2, tolerance = 0.1)
  # })


  test_that("emmGrid bayesfactor_parameters", {
    set.seed(333)

    xsdbf1 <- bayesfactor_parameters(bayes_sum, prior = fit_bayes, verbose = FALSE)
    xsdbf2 <- bayesfactor_parameters(bayes_sum, prior = bayes_sum_prior, verbose = FALSE)

    expect_equal(xsdbf1$log_BF, xsdbf2$log_BF, tolerance = 0.01)
  })

  # link vs response
  test_that("emmGrid bayesfactor_parameters / describe w/ nonlinear models", {
    skip_on_cran()

    model <- stan_glm(vs ~ mpg,
      data = mtcars,
      family = "binomial",
      refresh = 0
    )

    probs <- emmeans(model, "mpg", type = "resp")
    link <- emmeans(model, "mpg")

    probs_summ <- summary(probs)
    link_summ <- summary(link)

    xhdi <- hdi(probs, ci = 0.95)
    xpest <- point_estimate(probs, centrality = "median", dispersion = TRUE)
    expect_equal(xhdi$CI_low, probs_summ$lower.HPD, tolerance = 0.1)
    expect_equal(xhdi$CI_high, probs_summ$upper.HPD, tolerance = 0.1)
    expect_equal(xpest$Median, probs_summ$prob, tolerance = 0.1)


    xhdi <- hdi(link, ci = 0.95)
    xpest <- point_estimate(link, centrality = "median", dispersion = TRUE)
    expect_equal(xhdi$CI_low, link_summ$lower.HPD, tolerance = 0.1)
    expect_equal(xhdi$CI_high, link_summ$upper.HPD, tolerance = 0.1)
    expect_equal(xpest$Median, link_summ$emmean, tolerance = 0.1)
  })
}
