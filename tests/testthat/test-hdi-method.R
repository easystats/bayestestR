# Values recorded from upstream/main 396471bf, before `method` was added:
# the samples bounds are order statistics of `x`, and the density bounds are
# the results of the old `spi()`.
test_that("hdi() method = 'samples' keeps the old results", {
  set.seed(123)
  x <- rnorm(1000)
  x_sorted <- sort(x)
  ci <- c(0.8, 0.9, 0.95)

  out <- hdi(x, ci = ci)
  expect_identical(out$CI_low, x_sorted[c(101, 52, 21)])
  expect_identical(out$CI_high, x_sorted[c(901, 952, 971)])
  expect_identical(hdi(x, ci = ci, method = "samples"), out)
  expect_identical(capture.output(print(hdi(x))), "95% HDI: [-1.97, 1.98]")
})


test_that("hdi() method = 'density' gives the old spi() results", {
  skip_if_not_installed("quadprog")
  set.seed(123)
  x <- rnorm(1000)
  ci <- c(0.8, 0.9, 0.95)

  out <- hdi(x, ci = ci, method = "density")
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

  # expected bounds from the numeric method, one row per column and ci
  expected <- do.call(
    rbind,
    lapply(seq_len(ncol(draws)), function(j) {
      hdi(draws[, j], ci = ci, method = "density")
    })
  )

  # data frame
  out <- hdi(as.data.frame(draws), ci = ci, method = "density")
  expect_identical(out$CI_low, expected$CI_low)
  expect_identical(out$CI_high, expected$CI_high)

  # posterior draws_df
  out <- hdi(posterior::as_draws_df(draws), ci = ci, method = "density")
  expect_identical(out$CI_low, expected$CI_low)
  expect_identical(out$CI_high, expected$CI_high)

  # data frame with an rvar column
  d <- data.frame(g = c("a", "b"))
  d$r <- posterior::rvar(draws)
  out <- hdi(d, ci = ci, rvar_col = "r", method = "density")
  expect_identical(out$CI_low, expected$CI_low)
  expect_identical(out$CI_high, expected$CI_high)

  # mcmc objects, built by hand because coda is not in Suggests; `mcpar` lets
  # coda's as.data.frame() method work when another test loaded coda
  x_mcmc <- structure(
    draws,
    mcpar = c(1, nrow(draws), 1),
    class = c("mcmc", "matrix")
  )
  out <- hdi(x_mcmc, ci = ci, method = "density")
  expect_identical(out$CI_low, expected$CI_low)
  expect_identical(out$CI_high, expected$CI_high)
})


test_that("hdi() method = 'density' for stanreg models", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_installed("quadprog")

  m <- insight::download_model("stanreg_merMod_5")
  p <- insight::get_parameters(m, effects = "all")
  ci <- c(0.5, 0.8)

  out <- hdi(m, ci = ci, effects = "all", method = "density")
  expected <- do.call(
    rbind,
    lapply(p, function(i) hdi(i, ci = ci, method = "density"))
  )
  expect_identical(out$CI_low, expected$CI_low)
  expect_identical(out$CI_high, expected$CI_high)
  expect_s3_class(out, "bayestestR_spi")
})


test_that("ci(), describe_posterior() and ci_method = 'spi' use the density method", {
  skip_if_not_installed("quadprog")
  set.seed(123)
  x <- rnorm(1000)
  density_hdi <- hdi(x, method = "density")
  samples_hdi <- hdi(x)

  expect_no_warning(out <- ci(x, method = "SPI"), class = "deprecatedWarning")
  expect_identical(out$CI_low, density_hdi$CI_low)
  expect_identical(out$CI_high, density_hdi$CI_high)

  out <- ci(x, method = "HDI")
  expect_identical(out$CI_low, samples_hdi$CI_low)
  expect_identical(out$CI_high, samples_hdi$CI_high)

  expect_no_warning(
    out <- describe_posterior(x, ci_method = "spi", test = "pd"),
    class = "deprecatedWarning"
  )
  expect_identical(out$CI_low, density_hdi$CI_low)
  expect_identical(out$CI_high, density_hdi$CI_high)

  out <- hdi(x, ci_method = "spi")
  expect_identical(out$CI_low, density_hdi$CI_low)
  expect_identical(out$CI_high, density_hdi$CI_high)

  # an explicit `method` wins over `ci_method`
  out <- hdi(x, method = "samples", ci_method = "spi")
  expect_identical(out$CI_low, samples_hdi$CI_low)
  expect_identical(out$CI_high, samples_hdi$CI_high)

  expect_no_warning(
    rope(x, range = c(-0.1, 0.1), ci_method = "SPI"),
    class = "deprecatedWarning"
  )
})


test_that("hdi() stops for an unknown method", {
  set.seed(123)
  x <- rnorm(1000)
  expect_error(hdi(x, method = "foo"), "samples.*density")
  expect_error(
    hdi(data.frame(a = x, b = x), method = "foo"),
    "samples.*density"
  )
})
