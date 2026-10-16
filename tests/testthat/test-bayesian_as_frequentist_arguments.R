test_that("bayesian_as_frequentist() names its response argument `response`", {
  expect_named(
    formals(convert_bayesian_as_frequentist),
    c("model", "data", "REML", "response")
  )
  expect_identical(bayesian_as_frequentist, convert_bayesian_as_frequentist)

  # `resp`, the name before 0.20.0.1, still matches `response` by partial
  # matching, because no other argument starts with "resp"
  call <- match.call(convert_bayesian_as_frequentist, quote(f(m, resp = "gear")))
  expect_identical(call$response, "gear")
})
