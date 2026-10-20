test_that("bayesfactor() passes verbose to bayesfactor_models()", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(qsec ~ wt, data = mtcars)
  expect_warning(bayesfactor(m1, m2), "probably not all models were fit")
  expect_no_warning(bayesfactor(m1, m2, verbose = FALSE))
})

test_that("BFBayesFactor methods pass verbose to insight::get_parameters()", {
  skip_if_not_or_load_if_installed("BayesFactor")
  set.seed(123)
  x <- BayesFactor::ttestBF(
    mtcars$mpg[mtcars$am == 1],
    mtcars$mpg[mtcars$am == 0]
  )

  get_parameters <- insight::get_parameters
  seen <- list()
  local_mocked_bindings(
    get_parameters = function(x, ...) {
      verbose <- list(...)[["verbose"]]
      seen[[length(seen) + 1]] <<- if (is.null(verbose)) NA else verbose
      get_parameters(x, ...)
    },
    .package = "insight"
  )

  funs <- list(
    hdi = hdi,
    eti = eti,
    bci = bci,
    describe_posterior = describe_posterior,
    equivalence_test = equivalence_test
  )
  for (i in names(funs)) {
    seen <- list()
    suppressWarnings(suppressMessages(funs[[i]](x, verbose = FALSE)))
    expect_identical(seen, list(FALSE), info = i)
  }
})
