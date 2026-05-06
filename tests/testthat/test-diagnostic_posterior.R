test_that("diagnostic_posterior default", {
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
  expect_equal(dp$Rhat, c(1.001218705610197, 0.9997185343161158), tolerance = 1e-5)
  expect_true(is.numeric(dp$ESS))
  expect_true(all(dp$ESS > 0))
  expect_equal(dp$MCSE, c(0.02455353655881737, 0.019981604021430396), tolerance = 1e-5)

  adims <- list(npar = 2, nchains = 4, nsamp = 1000)
  dd2 <- with(
    adims,
    array(
      rnorm(npar * nchains * nsamp),
      dim = c(nsamp, nchains, npar),
      dimnames = list(NULL, NULL, LETTERS[1:npar])
    )
  )
  dp2 <- diagnostic_posterior(dd2)
  expect_equal(dp2$Parameter, c("A", "B"))
  expect_equal(dp2$Rhat, c(1.0012553633007766, 1.0008730199159905), tolerance = 1e-5)
  expect_true(is.numeric(dp2$ESS))
  expect_true(all(dp2$ESS > 0))
  expect_equal(dp2$MCSE, c(0.03291257472426944, 0.03590011166377985), tolerance = 1e-5)
})
