# values from before `method` was added
test_that("hdi() method = 'samples' keeps the old results", {
  set.seed(123)
  x <- rnorm(1000)
  ci <- c(0.8, 0.9, 0.95)

  out <- hdi(x, ci = ci)
  expect_identical(c(out$CI_low, out$CI_high), sort(x)[c(101, 52, 21, 901, 952, 971)])
  expect_identical(hdi(x, ci = ci, method = "samples"), out)
})


test_that("hdi() method = 'density' gives the old spi() results", {
  skip_if_not_installed("quadprog")
  set.seed(123)
  x <- rnorm(1000)
  ci <- c(0.8, 0.9, 0.95)

  out <- hdi(x, ci = ci, method = "density")
  expect_length(out$CI_low, 3)
  expect_length(out$CI_high, 3)
  expect_lt(
    max(abs(out$CI_low - c(-1.2705321018, -1.5797881308, -2.0789555251))),
    1e-6
  )
  expect_lt(
    max(abs(out$CI_high - c(1.2571196872, 1.7331967093, 1.8809695750))),
    1e-6
  )
  expect_identical(
    capture.output(print(hdi(x, method = "density"))),
    "95% SPI: [-2.08, 1.88]"
  )
})


test_that("hdi() method = 'density' works for data frame, draws, rvar and mcmc input", {
  skip_if_not_installed("quadprog")
  skip_if_not_installed("posterior")
  set.seed(123)
  draws <- matrix(rnorm(2000), ncol = 2, dimnames = list(NULL, c("a", "b")))
  ci <- c(0.8, 0.95)

  expected <- do.call(
    rbind,
    lapply(1:2, function(j) hdi(draws[, j], ci = ci, method = "density"))
  )
  expected <- c(expected$CI_low, expected$CI_high)

  out <- hdi(as.data.frame(draws), ci = ci, method = "density")
  expect_identical(c(out$CI_low, out$CI_high), expected)
  out <- hdi(posterior::as_draws_df(draws), ci = ci, method = "density")
  expect_identical(c(out$CI_low, out$CI_high), expected)

  d <- data.frame(g = c("a", "b"))
  d$r <- posterior::rvar(draws)
  out <- hdi(d, ci = ci, rvar_col = "r", method = "density")
  expect_identical(c(out$CI_low, out$CI_high), expected)

  # coda is not in Suggests; `mcpar` is needed when another test loaded coda
  x_mcmc <- structure(draws, mcpar = c(1, nrow(draws), 1), class = c("mcmc", "matrix"))
  out <- hdi(x_mcmc, ci = ci, method = "density")
  expect_identical(c(out$CI_low, out$CI_high), expected)
})


test_that("ci(), describe_posterior() and ci_method = 'spi' use the density method", {
  skip_if_not_installed("quadprog")
  set.seed(123)
  x <- rnorm(1000)
  density_hdi <- hdi(x, method = "density")
  expected <- c(density_hdi$CI_low, density_hdi$CI_high)

  expect_no_warning(out <- ci(x, method = "SPI"), class = "deprecatedWarning")
  expect_identical(c(out$CI_low, out$CI_high), expected)
  expect_no_warning(
    out <- describe_posterior(x, ci_method = "spi", test = "pd"),
    class = "deprecatedWarning"
  )
  expect_identical(c(out$CI_low, out$CI_high), expected)
  out <- hdi(x, ci_method = "spi")
  expect_identical(c(out$CI_low, out$CI_high), expected)

  # an explicit `method` wins over `ci_method`
  expect_identical(hdi(x, method = "samples", ci_method = "spi"), hdi(x))

  expect_no_warning(
    rope(x, range = c(-0.1, 0.1), ci_method = "SPI"),
    class = "deprecatedWarning"
  )
})


test_that("hdi() stops for an unknown method", {
  expect_error(hdi(rnorm(10), method = "foo"), "samples.*density")
})
