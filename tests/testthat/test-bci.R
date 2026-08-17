test_that("bci() acceleration agrees with the jackknife estimator from boot", {
  skip_if_not_installed("boot")

  set.seed(42)
  dat <- rexp(30)
  bs <- boot::boot(dat, function(d, i) mean(d[i]), R = 10000)

  # Reference: Efron's a from the empirical influence values of the data
  L <- boot::empinf(bs)
  a_jackknife <- sum(L^3) / (6 * sum(L^2)^1.5)

  # What .bci() computes from the replicates alone
  draws <- as.numeric(bs$t)
  deviation <- draws - mean(draws)
  a_draws <- mean(deviation^3) / (6 * mean(deviation^2)^1.5)

  expect_identical(sign(a_draws), sign(a_jackknife))
  expect_equal(a_draws, a_jackknife, tolerance = 0.2)

  # End to end: interval endpoints against boot.ci() on the same replicates
  ci_boot <- boot::boot.ci(bs, conf = 0.95, type = "bca")$bca[4:5]
  ci_bci <- bci(draws, ci = 0.95)
  expect_equal(ci_bci$CI_low, ci_boot[1], tolerance = 0.02)
  expect_equal(ci_bci$CI_high, ci_boot[2], tolerance = 0.02)
})

test_that("bci() endpoints follow the BCa formula with a = skewness / 6", {
  set.seed(3)
  x <- rgamma(4000, shape = 2)

  m <- mean(x)
  deviation <- x - m
  a <- mean(deviation^3) / (6 * mean(deviation^2)^1.5)
  z <- qnorm(sum(x < m) / length(x))
  lower_p <- pnorm(z + (z + qnorm(0.025)) / (1 - a * (z + qnorm(0.025))))
  upper_p <- pnorm(z + (z + qnorm(0.975)) / (1 - a * (z + qnorm(0.975))))
  expected <- unname(quantile(x, c(lower_p, upper_p)))

  out <- bci(x, ci = 0.95)
  expect_equal(c(out$CI_low, out$CI_high), expected, tolerance = 1e-8)
})

test_that("bci() handles constant draws", {
  out <- bci(rep(1, 100), verbose = FALSE)
  expect_identical(c(out$CI_low, out$CI_high), c(1, 1))
})

test_that("bci() handles missing draws", {
  x <- c(seq_len(100), NA_real_)
  out <- bci(x)
  expected <- bci(x[!is.na(x)])
  expect_equal(out[c("CI", "CI_low", "CI_high")], expected[c("CI", "CI_low", "CI_high")])

  out <- bci(rep(NA_real_, 100), verbose = FALSE)
  expect_true(all(is.na(c(out$CI_low, out$CI_high))))
})

test_that("bci() prints as a BCa interval", {
  out <- bcai(matrix(rchisq(200, 2), ncol = 1))
  printed <- capture.output(print(out, digits = 3))

  expect_s3_class(out, "bayestestR_bci")
  expect_true(any(grepl("BCa", printed, fixed = TRUE)))
  expect_false(any(grepl("ETI", printed, fixed = TRUE)))
})
