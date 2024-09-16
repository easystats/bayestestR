test_that("p_significance", {
  # numeric
  set.seed(333)
  x <- distribution_normal(10000, 1, 1)
  ps <- p_significance(x)
  expect_equal(as.numeric(ps), 0.816, tolerance = 0.1)
  expect_s3_class(ps, "p_significance")
  expect_s3_class(ps, "data.frame")
  expect_identical(dim(ps), c(1L, 2L))
  expect_identical(
    capture.output(print(ps)),
    c(
      "Practical Significance (threshold: 0.10)",
      "",
      "Parameter |   ps",
      "----------------",
      "Posterior | 0.82"
    )
  )

  # non-symmetric intervals
  ps <- p_significance(x, threshold = c(0.05, 0.2))
  expect_equal(as.numeric(ps), 0.7881, tolerance = 0.1)
  # should be identical, both ranges have same distance to the mean 1
  ps <- p_significance(x, threshold = c(1.8, 1.95))
  expect_equal(as.numeric(ps), 0.7881, tolerance = 0.1)

  set.seed(333)
  x <- data.frame(replicate(4, rnorm(100)))
  pd <- p_significance(x)
  expect_identical(dim(pd), c(4L, 2L))

  # error:
  expect_error(p_significance(x, threshold = 1:3))
})

test_that("stanreg", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("rstanarm")

  m <- insight::download_model("stanreg_merMod_5")

  expect_equal(
    p_significance(m, effects = "all")$ps[1],
    0.99,
    tolerance = 1e-2
  )
})

test_that("brms", {
  skip_if_offline()
  skip_if_not_or_load_if_installed("brms")

  m2 <- insight::download_model("brms_1")

  expect_equal(
    p_significance(m2, effects = "all")$ps,
    c(1.0000, 0.9985, 0.9785),
    tolerance = 0.01
  )

  out <- p_significance(m2, threshold = list(1, "default", 2), effects = "all")
  expect_equal(
    out$ps,
    c(1.00000, 0.99850, 0.12275),
    tolerance = 0.01
  )
  expect_equal(
    attributes(out)$threshold,
    list(c(-1, 1), c(-0.60269480520891, 0.60269480520891), c(-2, 2)),
    tolerance = 1e-4
  )
})
