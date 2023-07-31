test_that("p_significance", {
  # numeric
  set.seed(333)
  x <- distribution_normal(10000, 1, 1)
  ps <- p_significance(x)
  expect_equal(as.numeric(ps), 0.816, tolerance = 0.1)
  expect_s3_class(ps, "p_significance")
  expect_s3_class(ps, "data.frame")
  expect_equal(dim(ps), c(1, 1))
  expect_output(print(ps), "Practical Significance \\(threshold: 0.10\\): 0.82")

  x <- data.frame(replicate(4, rnorm(100)))
  pd <- p_significance(x)
  expect_equal(dim(pd), c(4, 2))
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
  skip_if_not_or_load_if_installed("rstanarm")

  m2 <- insight::download_model("brms_1")

  expect_equal(
    p_significance(m2, effects = "all")$ps,
    c(1.0000, 0.9985, 0.9785),
    tolerance = 0.01
  )
})
