test_that("bayesian_as_frequentist() names its response argument `response`", {
  expect_named(
    formals(convert_bayesian_as_frequentist),
    c("model", "data", "REML", "response")
  )

  # `resp`, the old name, still works by partial matching
  withr::local_options(warnPartialMatchArgs = FALSE)
  call <- match.call(convert_bayesian_as_frequentist, quote(f(m, resp = "gear")))
  expect_identical(call$response, "gear")
})
