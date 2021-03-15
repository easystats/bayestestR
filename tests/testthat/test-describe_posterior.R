if (require("testthat") && suppressPackageStartupMessages(require("bayestestR", quietly = TRUE)) && require("rstanarm") && require("brms") && require("httr") && require("insight") && require("BayesFactor", quietly = TRUE)) {
  test_that("describe_posterior", {
    set.seed(333)

    # numeric -------------------------------------------------

    x <- distribution_normal(1000)

    expect_warning(describe_posterior(
      x,
      centrality = "all",
      dispersion = TRUE,
      test = "all",
      ci = 0.89
    ))

    rez <- as.data.frame(suppressWarnings(describe_posterior(
      x,
      centrality = "all",
      dispersion = TRUE,
      test = "all",
      ci = 0.89
    )))

    expect_equal(dim(rez), c(1, 19))
    expect_equal(colnames(rez), c(
      "Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
      "CI_high", "p_map", "pd", "p_ROPE", "ps", "ROPE_CI", "ROPE_low",
      "ROPE_high", "ROPE_Percentage", "ROPE_Equivalence", "BF"
    ))

    rez <- expect_warning(describe_posterior(
      x,
      centrality = "all",
      dispersion = TRUE,
      test = "all",
      ci = c(0.8, 0.9)
    ))
    expect_equal(dim(rez), c(2, 19))

    rez <- describe_posterior(
      x,
      centrality = NULL,
      dispersion = TRUE,
      test = NULL,
      ci_method = "quantile"
    )
    expect_equal(dim(rez), c(1, 4))

    # dataframes -------------------------------------------------

    x <- data.frame(replicate(4, rnorm(100)))
    rez <- expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
    expect_equal(dim(rez), c(4, 19))
    rez <- expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9)))
    expect_equal(dim(rez), c(8, 19))
    rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
    expect_equal(dim(rez), c(4, 4))
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest && Sys.info()["sysname"] != "Darwin") {
    test_that("describe_posterior", {
      set.seed(333)
      # Rstanarm
      x <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, refresh = 0, iter = 500)
      rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")
      expect_equal(dim(rez), c(2, 21))
      expect_equal(colnames(rez), c(
        "Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
        "CI_high", "p_MAP", "pd", "p_ROPE", "ps", "ROPE_CI", "ROPE_low",
        "ROPE_high", "ROPE_Percentage", "ROPE_Equivalence", "BF", "Rhat",
        "ESS"
      ))
      rez <- describe_posterior(
        x,
        centrality = "all",
        dispersion = TRUE,
        test = "all",
        ci = c(0.8, 0.9)
      )
      expect_equal(dim(rez), c(4, 21))

      rez <- describe_posterior(
        x,
        centrality = NULL,
        dispersion = TRUE,
        test = NULL,
        ci_method = "quantile",
        diagnostic = NULL,
        priors = FALSE
      )
      expect_equal(dim(rez), c(2, 4))

      # brms -------------------------------------------------

      x <- brms::brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars, refresh = 0)
      rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, ci = c(0.8, 0.9))

      expect_equal(dim(rez), c(4, 16))
      expect_equal(colnames(rez), c(
        "Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
        "CI_high", "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
        "Rhat", "ESS"
      ))

      rez <- describe_posterior(
        x,
        centrality = NULL,
        dispersion = TRUE,
        test = NULL,
        ci_method = "quantile",
        diagnostic = NULL
      )

      expect_equal(dim(rez), c(2, 4))

      model <- brms::brm(
        mpg ~ drat,
        data = mtcars,
        chains = 2,
        algorithm = "meanfield",
        refresh = 0
      )

      expect_equal(nrow(describe_posterior(model)), 2)

      # rstanarm -------------------------------------------------

      model <- rstanarm::stan_glm(mpg ~ drat,
        data = mtcars,
        algorithm = "meanfield",
        refresh = 0
      )

      expect_equal(nrow(describe_posterior(model)), 2)

      model <- rstanarm::stan_glm(mpg ~ drat,
        data = mtcars,
        algorithm = "optimizing",
        refresh = 0
      )

      expect_equal(nrow(describe_posterior(model)), 2)

      model <- rstanarm::stan_glm(mpg ~ drat,
        data = mtcars,
        algorithm = "fullrank",
        refresh = 0
      )

      expect_equal(nrow(describe_posterior(model)), 2)
      # model <- brms::brm(mpg ~ drat, data = mtcars, chains=2, algorithm="fullrank", refresh=0)
      # expect_equal(nrow(describe_posterior(model)), 2)

      # BayesFactor
      # library(BayesFactor)
      # x <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
      # rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")
      # expect_equal(dim(rez), c(4, 16))
      # rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))
      # expect_equal(dim(rez), c(8, 16))
      # rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method="quantile")
      # expect_equal(dim(rez), c(4, 4))
    })

    if (require("insight")) {
      m <- insight::download_model("stanreg_merMod_5")
      p <- insight::get_parameters(m, effects = "all")

      test_that("describe_posterior", {
        expect_equal(
          describe_posterior(m, effects = "all")$Median,
          describe_posterior(p)$Median,
          tolerance = 1e-3
        )
      })

      m <- insight::download_model("brms_zi_3")
      p <- insight::get_parameters(m, effects = "all", component = "all")

      test_that("describe_posterior", {
        expect_equal(
          describe_posterior(m, effects = "all", component = "all")$Median,
          describe_posterior(p)$Median,
          tolerance = 1e-3
        )
      })
    }


    test_that("describe_posterior w/ BF+SI", {
      skip_on_cran()

      x <- insight::download_model("stanreg_lm_1")
      set.seed(555)
      rez <- describe_posterior(x, ci_method = "SI", test = "bf")


      # test si
      set.seed(555)
      rez_si <- si(x)
      expect_equal(rez$CI_low, rez_si$CI_low, tolerance = 0.1)
      expect_equal(rez$CI_high, rez_si$CI_high, tolerance = 0.1)

      # test BF
      set.seed(555)
      rez_bf <- bayesfactor_parameters(x)
      expect_equal(rez$BF, rez_bf$BF, tolerance = 0.1)
    })

    # BayesFactor -------------------------------------------------

    if (getRversion() >= "4.0") {
      set.seed(123)
      expect_equal(
        describe_posterior(correlationBF(mtcars$wt, mtcars$mpg, rscale = 0.5)),
        structure(list(Parameter = "rho", Median = -0.832958463649399,
                       CI = 0.89, CI_low = -0.903528140372971, CI_high = -0.734146316854132,
                       pd = 1, ROPE_CI = 0.89, ROPE_low = -0.1, ROPE_high = 0.1,
                       ROPE_Percentage = 0, BF = 33555274.5519413, Prior_Distribution = "beta",
                       Prior_Location = 2, Prior_Scale = 2), row.names = 1L, class = c("describe_posterior",
                                                                                       "see_describe_posterior", "data.frame"), ci_method = "hdi"),
        tolerance = 0.1,
        ignore_attr = TRUE
      )

      set.seed(123)
      expect_equal(
        describe_posterior(ttestBF(mtcars$wt, mu = 3)),
        structure(
          list(
            Parameter = "Difference",
            Median = -0.203832944917419,
            CI = 0.89,
            CI_low = -0.475212995537289,
            CI_high = 0.0972425949727671,
            pd = 0.87575,
            ROPE_CI = 0.89,
            ROPE_low = -0.0978457442989697,
            ROPE_high = 0.0978457442989697,
            ROPE_Percentage = 0.243751755124965,
            BF = 0.386851835160946,
            Prior_Distribution = "cauchy",
            Prior_Location = 0,
            Prior_Scale = 0.707106781186548
          ),
          row.names = 1L,
          class = c("describe_posterior",
                    "see_describe_posterior", "data.frame"),
          ci_method = "hdi"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )

      set.seed(123)
      expect_equal(
        describe_posterior(contingencyTableBF(
          x = table(mtcars$am, mtcars$cyl),
          sampleType = "poisson"
        )),
        structure(
          list(
            Parameter = c(
              "cell[1,1]",
              "cell[2,1]",
              "cell[1,2]",
              "cell[2,2]",
              "cell[1,3]",
              "cell[2,3]",
              "Ratio"
            ),
            Median = c(
              3.04620767622137,
              7.32989568199659,
              3.97000903399153,
              3.07366183177967,
              10.6950851081453,
              2.26260241420764,
              NA
            ),
            CI = c(0.89, 0.89, 0.89, 0.89, 0.89, 0.89, NA),
            CI_low = c(
              0.752315108749041,
              3.76209619996333,
              1.5072105253063,
              0.767717951722201,
              6.26621101563977,
              0.356954226142079,
              NA
            ),
            CI_high = c(
              5.7803113975366,
              11.3813683854843,
              7.05717637515788,
              5.58765747166004,
              15.4698463090165,
              4.41250020663008,
              NA
            ),
            pd = c(1, 1, 1, 1, 1, 1, NA),
            ROPE_CI = c(0.89, 0.89, 0.89, 0.89, 0.89, 0.89, NA),
            ROPE_low = c(-0.1, -0.1, -0.1, -0.1, -0.1, -0.1, NA),
            ROPE_high = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, NA),
            ROPE_Percentage = c(0, 0, 0, 0, 0, 0, NA),
            BF = c(
              46.6128745808996,
              46.6128745808996,
              46.6128745808996,
              46.6128745808996,
              46.6128745808996,
              46.6128745808996,
              NA
            ),
            Prior_Distribution = c(NA, NA, NA, NA, NA, NA, "poisson"),
            Prior_Location = c(NA, NA, NA, NA, NA, NA, 0),
            Prior_Scale = c(NA, NA, NA, NA, NA, NA, 1)
          ),
          row.names = c(1L, 4L, 2L, 5L, 3L, 6L, 7L),
          class = c("describe_posterior", "see_describe_posterior", "data.frame"),
          ci_method = "hdi"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )

      set.seed(123)
      expect_equal(
        describe_posterior(contingencyTableBF(
          x = table(mtcars$am, mtcars$cyl),
          sampleType = "indepMulti",
          fixedMargin = "cols",
          priorConcentration = 1.6
        )),
        structure(
          list(
            Parameter = c(
              "cell[1,1]",
              "cell[2,1]",
              "cell[1,2]",
              "cell[2,2]",
              "cell[1,3]",
              "cell[2,3]",
              "Ratio"
            ),
            Median = c(
              3.34864716345133,
              7.2237058088546,
              4.16212411751866,
              3.32882909739155,
              10.3556344952798,
              2.59014390786925,
              NA
            ),
            CI = c(0.89, 0.89, 0.89, 0.89, 0.89, 0.89, NA),
            CI_low = c(
              1.16057250089558,
              4.15782470170975,
              1.72018584831039,
              1.25037293586929,
              6.46786088619769,
              0.62213974797028,
              NA
            ),
            CI_high = c(
              5.78171219806325,
              10.74432390807,
              6.76891913078361,
              5.85323883334191,
              13.8652885466891,
              4.69936080072413,
              NA
            ),
            pd = c(1, 1, 1, 1, 1, 1, NA),
            ROPE_CI = c(0.89, 0.89, 0.89, 0.89, 0.89, 0.89, NA),
            ROPE_low = c(-0.1, -0.1, -0.1,
                         -0.1, -0.1, -0.1, NA),
            ROPE_high = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, NA),
            ROPE_Percentage = c(0, 0, 0, 0, 0, 0, NA),
            BF = c(
              12.1022066941064,
              12.1022066941064,
              12.1022066941064,
              12.1022066941064,
              12.1022066941064,
              12.1022066941064,
              NA
            ),
            Prior_Distribution = c(NA, NA, NA, NA, NA, NA, "independent multinomial"),
            Prior_Location = c(NA, NA, NA, NA, NA, NA, 0),
            Prior_Scale = c(NA, NA, NA, NA, NA, NA, 1.6)
          ),
          row.names = c(1L, 4L, 2L, 5L, 3L, 6L, 7L),
          class = c("describe_posterior", "see_describe_posterior", "data.frame"),
          ci_method = "hdi"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )

      set.seed(123)
      expect_equal(
        describe_posterior(anovaBF(extra ~ group, data = sleep, progress = FALSE)),
        structure(
          list(
            Parameter = c("mu", "group-1", "group-2", "sig2", "g_group"),
            Median = c(
              1.52927326879743,
              -0.571283063863296,
              0.571283063863296,
              3.744464775706,
              0.334859267892042
            ),
            CI = c(0.89, 0.89, 0.89, 0.89, 0.89),
            CI_low = c(
              0.735571459282669,
              -1.23832049919791,
              -0.0841668568133583,
              1.99555691696385,
              0.0191509488133218
            ),
            CI_high = c(
              2.16331957764619,
              0.0841668568133583,
              1.23832049919791,
              5.93393140134115,
              2.31176122287214
            ),
            pd = c(0.99925, 0.9265, 0.9265, 1, 1),
            ROPE_CI = c(0.89, 0.89, 0.89, 0.89, 0.89),
            ROPE_low = c(
              -0.201791972090071,
              -0.201791972090071,
              -0.201791972090071,
              -0.201791972090071,
              -0.201791972090071
            ),
            ROPE_high = c(
              0.201791972090071,
              0.201791972090071,
              0.201791972090071,
              0.201791972090071,
              0.201791972090071
            ),
            ROPE_Percentage = c(0, 0.151923616961528, 0.151923616961528, 0, 0.378545352429093),
            BF = c(
              1.26592514964916,
              1.26592514964916,
              1.26592514964916,
              1.26592514964916,
              1.26592514964916
            ),
            Prior_Distribution = c(NA, "cauchy", "cauchy", NA, NA),
            Prior_Location = c(NA, 0, 0, NA, NA),
            Prior_Scale = c(NA, 0.5, 0.5, NA, NA)
          ),
          row.names = c(4L, 2L, 3L, 5L, 1L),
          class = c("describe_posterior", "see_describe_posterior", "data.frame"),
          ci_method = "hdi"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    }
  }
}
