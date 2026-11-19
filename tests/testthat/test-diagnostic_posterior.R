test_that("diagnostic_posterior default", {
  skip_on_os("mac")
  skip_if_not_or_load_if_installed("rstan")
  ## same as example("diagnostic_posterior")
  set.seed(101)
  mkdata <- function(nrow = 1000, ncol = 2, parnm = LETTERS[1:ncol]) {
    x <- as.data.frame(replicate(ncol, rnorm(nrow)))
    names(x) <- parnm
    x
  }
  dd <- replicate(5, mkdata(), simplify = FALSE)
  dp <- diagnostic_posterior(dd)
  expect_equal(dp$Parameter, c("A", "B"))
  expect_equal(dp$Rhat, c(1.001218705610197, 0.9997185343161158), tolerance = 1e-3)
  expect_true(is.numeric(dp$ESS_tail))
  expect_true(all(dp$ESS_tail > 0))
  expect_named(dp, c("Parameter", "ESS_tail", "ESS_bulk", "Rhat", "MCSE"))
  expect_equal(dp$MCSE, c(0.01453021, 0.01679645), tolerance = 1e-3)

  adims <- list(npar = 2, nchains = 4, nsamp = 1000)
  dd2 <- with(
    adims,
    array(
      rnorm(npar * nchains * nsamp),
      dim = c(nsamp, nchains, npar),
      dimnames = list(NULL, NULL, LETTERS[1:npar])
    )
  )
  skip_on_os("linux")
  dp2 <- diagnostic_posterior(dd2)
  expect_equal(dp2$Parameter, c("A", "B"))
  expect_equal(dp2$Rhat, c(1.00125536, 1.00087302), tolerance = 1e-3)
  expect_true(is.numeric(dp2$ESS_tail))
  expect_true(all(dp2$ESS_tail > 0))
  expect_equal(dp2$MCSE, c(0.02251173, 0.02118743), tolerance = 1e-3)
})
