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

  msg <- "Can't simulate priors from these distributions:\\s+laplace\\s+\\(wt,\\s+am\\)\\."
  expect_warning(simulate_prior(model, n = 10), msg)
  # check_prior() does not add its own warning
  w <- capture_warnings(check_prior(model))
  expect_length(w, 1)
  expect_match(w, msg)
})


test_that("simulate_prior names priors that give only missing values", {
  priors <- data.frame(
    Parameter = c("b_wt", "b_am", "bcs_treat[1]"),
    Distribution = c("normal", "uniform", NA),
    Location = c(0, NA, NA),
    Scale = c(1, NA, NA),
    stringsAsFactors = FALSE
  )
  expect_warning(
    .simulate_prior(priors, n = 10),
    "uniform\\s+\\(b_am\\),\\s+unknown\\s+\\(bcs_treat\\[1\\]\\)\\."
  )
})
