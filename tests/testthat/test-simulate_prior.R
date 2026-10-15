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

  expect_warning(
    simulate_prior(model, n = 10),
    "Can't simulate priors from a laplace distribution (wt, am)",
    fixed = TRUE
  )
  out <- suppressWarnings(simulate_prior(model, n = 10))
  expect_named(out, c("(Intercept)", "wt", "am"))
  expect_false(anyNA(out[["(Intercept)"]]))
  expect_true(all(is.na(out$wt)))
  expect_true(all(is.na(out$am)))

  expect_silent(simulate_prior(model, n = 10, verbose = FALSE))
})


test_that("simulate_prior warns when a simulated prior is all missing", {
  priors <- data.frame(
    Parameter = c("b_wt", "b_am"),
    Distribution = c("normal", "uniform"),
    Location = c(0, NA),
    Scale = c(1, NA),
    stringsAsFactors = FALSE
  )
  expect_warning(
    .simulate_prior(priors, n = 10),
    "Can't simulate priors from a uniform distribution (b_am)",
    fixed = TRUE
  )
  out <- suppressWarnings(.simulate_prior(priors, n = 10))
  expect_false(anyNA(out$b_wt))
  expect_true(all(is.na(out$b_am)))
})
