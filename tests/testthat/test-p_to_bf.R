test_that("p_to_bf works", {
  skip_if_not_or_load_if_installed("parameters")

  m <- lm(mpg ~ hp + cyl + am, data = mtcars)
  p <- coef(summary(m))[, "Pr(>|t|)"]

  # BF by hand
  bfs <- p_to_bf(m, log = FALSE)
  log_bfs <- p_to_bf(m, log = TRUE)
  bfs_manual <- 3 * p * sqrt(nrow(mtcars))

  expect_equal(bfs$BF, 1 / bfs_manual, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(log_bfs$log_BF, -log(bfs_manual), tolerance = 1e-4, ignore_attr = TRUE)
})


test_that("p_to_bf works for mixed models", {
  skip_if_not_or_load_if_installed("parameters")

  npk.aovE <- aov(yield ~ N * P * K + Error(block), npk)
  expect_error(
    p_to_bf(npk.aovE),
    "Argument `n_obs` must be specified for mixed models."
  )
  expect_error(p_to_bf(npk.aovE, n_obs = 6), NA)

  skip_if_not_installed("lmerTest")
  mod <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_error(
    p_to_bf(mod),
    "Argument `n_obs` must be specified for mixed models."
  )
  expect_error(p_to_bf(mod, n_obs = nlevels(lme4::sleepstudy$Subject)), NA)
})
