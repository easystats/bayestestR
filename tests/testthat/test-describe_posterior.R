if (require("testthat") && require("bayestestR") && require("rstanarm") && require("brms") && require("httr") && require("insight") && require("BayesFactor")) {
  test_that("describe_posterior", {
    set.seed(333)

    # numeric -------------------------------------------------

    x <- distribution_normal(1000)
    rez <- expect_warning(describe_posterior(x, centrality = "all", dispersion = TRUE, test = "all", ci = 0.89))

    printed <- format(rez)
    expect_true("89% CI" %in% names(printed))

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

    set.seed(123)
    expect_equal(
      describe_posterior(correlationBF(mtcars$wt, mtcars$mpg, rscale = 0.5)),
      structure(
        list(
          Parameter = "rho",
          Median = -0.831943247361451,
          CI = 0.89,
          CI_low = -0.905808386185782,
          CI_high = -0.733075007588833,
          pd = 1,
          ROPE_CI = 89,
          ROPE_low = -0.1,
          ROPE_high = 0.1,
          ROPE_Percentage = 0,
          BF = 33555274.5519413,
          Prior_Distribution = "beta",
          Prior_Location = 2,
          Prior_Scale = 2
        ),
        row.names = 1L,
        class = "data.frame"
      ),
      tolerance = 0.1
    )

    set.seed(123)
    expect_equal(
      describe_posterior(ttestBF(mtcars$wt, mu = 3)),
      structure(
        list(
          Parameter = "Difference",
          Median = -0.194256864792614,
          CI = 0.89,
          CI_low = -0.488064417728344,
          CI_high = 0.0885434267686307,
          pd = 0.855,
          ROPE_CI = 89,
          ROPE_low = -0.0978457442989697,
          ROPE_high = 0.0978457442989697,
          ROPE_Percentage = 0.270148834597023,
          BF = 0.386851835160946,
          Prior_Distribution = "cauchy",
          Prior_Location = 0,
          Prior_Scale = 0.707106781186548
        ),
        row.names = 1L,
        class = "data.frame"
      ),
      tolerance = 0.1
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
            3.10002076660402,
            7.3149408472942,
            3.95395054865364,
            3.10529518036243,
            10.63829836629,
            2.32888617807367,
            NA
          ),
          CI = c(
            0.89, 0.89, 0.89, 0.89, 0.89, 0.89,
            NA
          ),
          CI_low = c(
            0.831859344044788,
            3.60813029676568,
            1.4086833254684,
            0.808714742291165,
            6.06392097174864,
            0.363551360995603,
            NA
          ),
          CI_high = c(
            5.73940934376484,
            11.3110978432929,
            7.06581003479593,
            5.77323478142797,
            15.534719072373,
            4.49962736575365,
            NA
          ),
          pd = c(1, 1, 1, 1, 1, 1, NA),
          ROPE_CI = c(
            89, 89, 89, 89,
            89, 89, NA
          ),
          ROPE_low = c(
            -0.1, -0.1, -0.1, -0.1, -0.1, -0.1,
            NA
          ),
          ROPE_high = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, NA),
          ROPE_Percentage = c(
            0,
            0, 0, 0, 0, 0, NA
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
        class = "data.frame"
      ),
      tolerance = 0.1
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
            3.34424390326332,
            7.25533265329878,
            4.15418100716339,
            3.34379401154391,
            10.3858983218456,
            2.52387799585576,
            NA
          ),
          CI = c(
            0.89, 0.89, 0.89, 0.89, 0.89, 0.89,
            NA
          ),
          CI_low = c(
            1.16770676374516,
            3.95575688998107,
            1.64030740777095,
            1.15811520892223,
            6.70620880680777,
            0.586022836188971,
            NA
          ),
          CI_high = c(
            5.83434397453877,
            10.4719589003557,
            6.8390088651287,
            5.79426926336237,
            14.0810357965959,
            4.64751978579163,
            NA
          ),
          pd = c(1, 1, 1, 1, 1, 1, NA),
          ROPE_CI = c(
            89,
            89, 89, 89, 89, 89, NA
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
          BF = c(
            12.1022066941064,
            12.1022066941064,
            12.1022066941064,
            12.1022066941064,
            12.1022066941064,
            12.1022066941064,
            NA
          ),
          Prior_Distribution = c(
            NA, NA, NA, NA,
            NA, NA, "independent multinomial"
          ),
          Prior_Location = c(
            NA, NA,
            NA, NA, NA, NA, 0
          ),
          Prior_Scale = c(NA, NA, NA, NA, NA, NA, 1.6)
        ),
        row.names = c(1L, 4L, 2L, 5L, 3L, 6L, 7L),
        class = "data.frame"
      ),
      tolerance = 0.1
    )

    set.seed(123)
    expect_equal(
      describe_posterior(anovaBF(extra ~ group, data = sleep, progress = FALSE)),
      structure(
        list(
          Parameter = c(
            "mu", "group-1", "group-2", "sig2",
            "g_group"
          ),
          Median = c(
            1.53022432533339,
            -0.58131970309442,
            0.58131970309442,
            3.7097691667823,
            0.353808192491004
          ),
          CI = c(
            0.89, 0.89, 0.89,
            0.89, 0.89
          ),
          CI_low = c(
            0.833311821360455,
            -1.21079773990579,
            -0.0651043934099907,
            1.9411352338509,
            0.0187016247190792
          ),
          CI_high = c(
            2.24472681256556,
            0.0651043934099907,
            1.21079773990579,
            5.87552771664977,
            2.25280269158369
          ),
          pd = c(0.99975, 0.9355, 0.9355, 1, 1),
          ROPE_CI = c(
            89, 89,
            89, 89, 89
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
            0.130862117382758,
            0.130862117382758,
            0,
            0.358326312833474
          ),
          BF = c(
            1.26592514964916,
            1.26592514964916,
            1.26592514964916,
            1.26592514964916,
            1.26592514964916
          ),
          Prior_Distribution = c(
            NA,
            "cauchy", "cauchy", NA, NA
          ),
          Prior_Location = c(
            NA, 0, 0,
            NA, NA
          ),
          Prior_Scale = c(NA, 0.5, 0.5, NA, NA)
        ),
        row.names = c(
          4L,
          2L, 3L, 5L, 1L
        ),
        class = "data.frame"
      ),
      tolerance = 0.1
    )
  }
}
