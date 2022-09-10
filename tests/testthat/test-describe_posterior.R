if (requiet("testthat") &&
  requiet("bayestestR") &&
  requiet("rstanarm") &&
  requiet("brms") &&
  requiet("httr") &&
  requiet("insight") &&
  requiet("BayesFactor")) {
  test_that("describe_posterior", {
    set.seed(333)

    # numeric -------------------------------------------------

    x <- distribution_normal(40000)

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
      "ROPE_high", "ROPE_Percentage", "ROPE_Equivalence", "log_BF"
    ))

    expect_warning(describe_posterior(
      x,
      centrality = "all",
      dispersion = TRUE,
      test = "all",
      ci = c(0.8, 0.9)
    ))
    # rez <- suppressWarnings(describe_posterior(
    #   x,
    #   centrality = "all",
    #   dispersion = TRUE,
    #   test = "all",
    #   ci = c(0.8, 0.9)
    # ))
    # expect_equal(dim(rez), c(2, 19))

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
    expect_warning(expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all")))
    # rez <- suppressWarnings(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
    # expect_equal(dim(rez), c(4, 19))
    expect_warning(expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9))))
    # rez <- suppressWarnings(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = c(0.8, 0.9)))
    # expect_equal(dim(rez), c(8, 19))
    rez <- describe_posterior(x, centrality = NULL, dispersion = TRUE, test = NULL, ci_method = "quantile")
    expect_equal(dim(rez), c(4, 4))
  })


  .runThisTest <- Sys.getenv("RunAllbayestestRTests") == "yes"
  if (.runThisTest && Sys.info()["sysname"] != "Darwin") {
    test_that("describe_posterior", {
      set.seed(333)
      # Rstanarm
      x <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, refresh = 0, iter = 500)
      expect_warning(rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all"))
      expect_equal(dim(rez), c(2, 21))
      expect_equal(colnames(rez), c(
        "Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
        "CI_high", "p_MAP", "pd", "p_ROPE", "ps", "ROPE_CI", "ROPE_low",
        "ROPE_high", "ROPE_Percentage", "ROPE_Equivalence", "log_BF", "Rhat",
        "ESS"
      ))
      expect_warning(rez <- describe_posterior(
        x,
        centrality = "all",
        dispersion = TRUE,
        test = "all",
        ci = c(0.8, 0.9)
      ))
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

      # x <- brms::brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars, refresh = 0)
      # rez <- describe_posterior(x, centrality = "all", dispersion = TRUE, ci = c(0.8, 0.9))
      #
      # expect_equal(dim(rez), c(4, 16))
      # expect_equal(colnames(rez), c(
      #   "Parameter", "Median", "MAD", "Mean", "SD", "MAP", "CI", "CI_low",
      #   "CI_high", "pd", "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage",
      #   "Rhat", "ESS"
      # ))
      #
      # rez <- describe_posterior(
      #   x,
      #   centrality = NULL,
      #   dispersion = TRUE,
      #   test = NULL,
      #   ci_method = "quantile",
      #   diagnostic = NULL
      # )
      #
      # expect_equal(dim(rez), c(2, 4))
      #
      # model <- brms::brm(
      #   mpg ~ drat,
      #   data = mtcars,
      #   chains = 2,
      #   algorithm = "meanfield",
      #   refresh = 0
      # )
      #
      # expect_equal(nrow(describe_posterior(model)), 2)

      # rstanarm -------------------------------------------------

      model <- rstanarm::stan_glm(mpg ~ drat,
        data = mtcars,
        algorithm = "meanfield",
        refresh = 0
      )

      expect_equal(nrow(describe_posterior(model)), 2)

      model <- suppressWarnings(rstanarm::stan_glm(mpg ~ drat,
        data = mtcars,
        algorithm = "optimizing",
        refresh = 0
      ))

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

    if (requiet("insight")) {
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
      expect_warning(expect_warning(rez <- describe_posterior(x, ci_method = "SI", test = "bf")))


      # test si
      set.seed(555)
      expect_warning(rez_si <- si(x))
      expect_equal(rez$CI_low, rez_si$CI_low, tolerance = 0.1)
      expect_equal(rez$CI_high, rez_si$CI_high, tolerance = 0.1)

      # test BF
      set.seed(555)
      rez_bf <- suppressWarnings(bayesfactor_parameters(x))
      expect_equal(rez$log_BF, log(as.numeric(rez_bf)), tolerance = 0.1)
    })

    # BayesFactor -------------------------------------------------

    if (getRversion() >= "4.0") {
      set.seed(123)
      expect_equal(
        as.data.frame(describe_posterior(correlationBF(
          mtcars$wt,
          mtcars$mpg,
          rscale = 0.5
        ))),
        structure(
          list(
            Parameter = "rho",
            Median = -0.833281858269296,
            CI = 0.95,
            CI_low = -0.919418102114416,
            CI_high = -0.715602277241063,
            pd = 1,
            ROPE_CI = 0.95,
            ROPE_low = -0.05,
            ROPE_high = 0.05,
            ROPE_Percentage = 0,
            log_BF = 17.328704623688,
            BF = 33555274.5519413,
            Prior_Distribution = "beta",
            Prior_Location = 2,
            Prior_Scale = 2
          ),
          row.names = 1L,
          class = "data.frame",
          ci_method = "hdi"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )

      set.seed(123)
      expect_equal(
        describe_posterior(ttestBF(mtcars$wt, mu = 3), ci = 0.95, ci_method = "hdi"),
        structure(
          list(
            Parameter = "Difference", Median = 0.192275922178887, CI = 0.95,
            CI_low = -0.172955539648102, CI_high = 0.526426796879103, pd = 0.85875,
            ROPE_CI = 0.95, ROPE_low = -0.0978457442989697, ROPE_high = 0.0978457442989697,
            ROPE_Percentage = 0.257300710339384, log_BF = -0.94971351422473,
            BF = 0.386851835128661, Prior_Distribution = "cauchy",
            Prior_Location = 0, Prior_Scale = 0.707106781186548
          ),
          row.names = 1L, class = c("describe_posterior", "see_describe_posterior", "data.frame"),
          ci_method = "hdi", object_name = "ttestBF(mtcars$wt, mu = 3)"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )

      set.seed(123)
      expect_warning(expect_equal(
        describe_posterior(
          contingencyTableBF(
            x = table(mtcars$am, mtcars$cyl),
            sampleType = "poisson"
          ),
          ci = 0.95,
          ci_method = "hdi"
        ),
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
              7.33170140780154,
              3.96252503900368,
              3.06206636495483,
              10.7088156207511,
              2.26008072419983,
              NA
            ),
            CI = c(
              0.95, 0.95, 0.95, 0.95, 0.95, 0.95,
              NA
            ),
            CI_low = c(
              0.537476720942068,
              3.33553818106395,
              1.05013765177975,
              0.746538992318074,
              5.49894434136364,
              0.275642629940081,
              NA
            ),
            CI_high = c(
              6.62852027141624,
              12.6753970192515,
              7.74693313388489,
              6.87239730676778,
              16.9198964674968,
              5.4533083861175,
              NA
            ),
            pd = c(1, 1, 1, 1, 1, 1, NA),
            ROPE_CI = c(
              0.95, 0.95, 0.95,
              0.95, 0.95, 0.95, NA
            ),
            ROPE_low = c(
              -0.1, -0.1, -0.1, -0.1,
              -0.1, -0.1, NA
            ),
            ROPE_high = c(
              0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
              NA
            ),
            ROPE_Percentage = c(0, 0, 0, 0, 0, 0, NA),
            log_BF = c(
              3.84187678153378,
              3.84187678153378,
              3.84187678153378,
              3.84187678153378,
              3.84187678153378,
              3.84187678153378,
              NA
            ),
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
            Prior_Scale = c(
              NA,
              NA, NA, NA, NA, NA, 1
            )
          ),
          row.names = c(
            1L, 4L, 2L, 5L, 3L,
            6L, 7L
          ),
          class = c("describe_posterior", "see_describe_posterior")
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      ))

      set.seed(123)
      expect_warning(expect_equal(
        describe_posterior(contingencyTableBF(
          x = table(mtcars$am, mtcars$cyl),
          sampleType = "indepMulti",
          fixedMargin = "cols",
          priorConcentration = 1.6
        ), ci = 0.95),
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
              3.33359102240953,
              7.27094924961528,
              4.13335763121549,
              3.36172537199681,
              10.3872621523407,
              2.56061336771352,
              NA
            ),
            CI = c(
              0.95, 0.95, 0.95, 0.95, 0.95, 0.95,
              NA
            ),
            CI_low = c(
              0.912122089726423,
              3.51744611674693,
              1.39218072401004,
              0.923175932880601,
              6.18021898129278,
              0.465587711080369,
              NA
            ),
            CI_high = c(
              6.61128887457661,
              11.4058892728414,
              7.61378018576518,
              6.65522159416386,
              15.1209075845299,
              5.35853420162441,
              NA
            ),
            pd = c(1, 1, 1, 1, 1, 1, NA),
            ROPE_CI = c(
              0.95, 0.95, 0.95,
              0.95, 0.95, 0.95, NA
            ),
            ROPE_low = c(
              -0.1, -0.1, -0.1, -0.1,
              -0.1, -0.1, NA
            ),
            ROPE_high = c(
              0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
              NA
            ),
            ROPE_Percentage = c(0, 0, 0, 0, 0, 0, NA),
            log_BF = c(
              2.49338780738881,
              2.49338780738881,
              2.49338780738881,
              2.49338780738881,
              2.49338780738881,
              2.49338780738881,
              NA
            ),
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
            Prior_Scale = c(
              NA,
              NA, NA, NA, NA, NA, 1.6
            )
          ),
          row.names = c(
            1L, 4L, 2L, 5L,
            3L, 6L, 7L
          ),
          class = c(
            "describe_posterior", "see_describe_posterior",
            "data.frame"
          ),
          ci_method = "hdi",
          object_name = "contingencyTableBF(x = table(mtcars$am, mtcars$cyl), sampleType = \"indepMulti\", fixedMargin = \"cols\", priorConcentration = 1.6)"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      ))

      set.seed(123)
      expect_equal(
        describe_posterior(anovaBF(extra ~ group, data = sleep, progress = FALSE), ci_method = "hdi", ci = 0.95),
        structure(
          list(
            Parameter = c(
              "mu", "group-1", "group-2", "sig2",
              "g_group"
            ),
            Median = c(
              1.53667371296145,
              -0.571674439385088,
              0.571674439385088,
              3.69268743002151,
              0.349038661644431
            ),
            CI = c(
              0.95,
              0.95, 0.95, 0.95, 0.95
            ),
            CI_low = c(
              0.691696017646264,
              -1.31604531656452,
              -0.229408603643392,
              1.75779899540302,
              0.0192738130412634
            ),
            CI_high = c(
              2.43317955922589,
              0.229408603643392,
              1.31604531656452,
              6.88471056133351,
              5.30402785651874
            ),
            pd = c(0.99975, 0.927, 0.927, 1, 1),
            ROPE_CI = c(
              0.95, 0.95,
              0.95, 0.95, 0.95
            ),
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
            ROPE_Percentage = c(
              0,
              0.162325703762168, 0.162325703762168, 0, 0.346487766377269
            ),
            log_BF = c(
              0.235803198474248,
              0.235803198474248,
              0.235803198474248,
              0.235803198474248,
              0.235803198474248
            ),
            BF = c(
              1.26592514964916,
              1.26592514964916,
              1.26592514964916,
              1.26592514964916,
              1.26592514964916
            ),
            Prior_Distribution = c(NA, "cauchy", "cauchy", NA, NA),
            Prior_Location = c(NA, 0, 0, NA, NA),
            Prior_Scale = c(
              NA,
              0.5, 0.5, NA, NA
            )
          ),
          row.names = c(4L, 2L, 3L, 5L, 1L),
          class = c(
            "describe_posterior",
            "see_describe_posterior", "data.frame"
          ),
          ci_method = "hdi",
          object_name = "anovaBF(extra ~ group, data = sleep, progress = FALSE)"
        ),
        tolerance = 0.1,
        ignore_attr = TRUE
      )
    }
  }
}
