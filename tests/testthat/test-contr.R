
test_that("contr.equalprior | gen", {
  skip_on_cran()
  set.seed(1234)

  k <- 3
  g <- 4.1

  contr1 <- contr.equalprior(k, contrasts = TRUE)
  contr2 <- contr.equalprior(k, contrasts = FALSE)

  samps1 <- replicate(ncol(contr1), {
    rnorm(4e3, 0, g)
  })
  samps2 <- replicate(ncol(contr2), {
    rnorm(4e3, 0, g)
  })

  means1 <- t(contr1 %*% t(samps1))
  means2 <- t(contr2 %*% t(samps2))

  expect_equal(mean(apply(means1, 2, sd)), mean(apply(means2, 2, sd)), tolerance = 0.1)
})

test_that("contr.equalprior | pairs", {
  skip_on_cran()
  set.seed(1234)

  k <- 3
  g <- 4.1

  contr1 <- contr.equalprior_pairs(k, contrasts = TRUE)
  contr2 <- contr.equalprior_pairs(k, contrasts = FALSE)

  samps1 <- replicate(ncol(contr1), {
    rnorm(4e3, 0, g)
  })
  samps2 <- replicate(ncol(contr2), {
    rnorm(4e3, 0, g)
  })

  means1 <- t(contr1 %*% t(samps1))
  means2 <- t(contr2 %*% t(samps2))

  w <- matrix(c(-1, 1, 0,
                1, 0, -1,
                0, -1, 1), 3,3)

  pairs1 <- t(w %*% t(means1))
  pairs2 <- t(w %*% t(means2))

  expect_equal(mean(apply(pairs1, 2, sd)), g, tolerance = 0.1)
  expect_equal(mean(apply(pairs1, 2, sd)), mean(apply(pairs2, 2, sd)), tolerance = 0.1)
})


test_that("contr.equalprior | dev", {
  skip_on_cran()
  set.seed(1234)

  k <- 3
  g <- 4.1

  contr1 <- contr.equalprior_deviations(k, contrasts = TRUE)
  contr2 <- contr.equalprior_deviations(k, contrasts = FALSE)

  samps1 <- replicate(ncol(contr1), {
    rnorm(4e3, 0, g)
  })
  samps2 <- replicate(ncol(contr2), {
    rnorm(4e3, 0, g)
  })

  means1 <- t(contr1 %*% t(samps1))
  means2 <- t(contr2 %*% t(samps2))

  expect_equal(mean(apply(means1, 2, sd)), g, tolerance = 0.1)
  expect_equal(mean(apply(means1, 2, sd)), mean(apply(means2, 2, sd)), tolerance = 0.1)
})

