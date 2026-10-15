# collect all warnings of an expression, with line wrapping removed
collect_warnings <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  gsub("\\s+", " ", msgs)
}


test_that("simulate_prior warns when a prior cannot be simulated", {
  skip_if_not_installed("rstanarm")
  set.seed(123)
  model <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ wt + am,
    data = mtcars,
    prior = rstanarm::laplace(0, 1),
    chains = 1,
    iter = 200,
    refresh = 0,
    seed = 123
  ))

  expect_identical(
    collect_warnings(simulate_prior(model, n = 10)),
    "Can't simulate priors from these distributions: laplace (wt, am)."
  )
  out <- suppressWarnings(simulate_prior(model, n = 10))
  expect_named(out, c("(Intercept)", "wt", "am"))
  expect_false(anyNA(out[["(Intercept)"]]))
  expect_true(all(is.na(out$wt)))
  expect_true(all(is.na(out$am)))

  expect_silent(simulate_prior(model, n = 10, verbose = FALSE))

  # check_prior() gives the warning from simulate_prior() only once
  expect_identical(
    collect_warnings(check_prior(model)),
    "Can't simulate priors from these distributions: laplace (wt, am)."
  )
})


test_that("simulate_prior warns when a simulated prior is all missing", {
  priors <- data.frame(
    Parameter = c("b_wt", "b_am"),
    Distribution = c("normal", "uniform"),
    Location = c(0, NA),
    Scale = c(1, NA),
    stringsAsFactors = FALSE
  )
  expect_identical(
    collect_warnings(.simulate_prior(priors, n = 10)),
    "Can't simulate priors from these distributions: uniform (b_am)."
  )
  out <- suppressWarnings(.simulate_prior(priors, n = 10))
  expect_false(anyNA(out$b_wt))
  expect_true(all(is.na(out$b_am)))
})


test_that("simulate_prior names parameters whose prior distribution is unknown", {
  # brms category-specific terms, such as `cs(treat)`, have no reported prior
  priors <- data.frame(
    Parameter = c("b_wt", "bcs_treat[1]", "bcs_treat[2]"),
    Distribution = c("uniform", NA, NA),
    Location = NA_real_,
    Scale = NA_real_,
    stringsAsFactors = FALSE
  )
  expect_identical(
    collect_warnings(.simulate_prior(priors, n = 10)),
    paste(
      "Can't simulate priors from these distributions: uniform (b_wt),",
      "unknown (bcs_treat[1], bcs_treat[2])."
    )
  )
})
