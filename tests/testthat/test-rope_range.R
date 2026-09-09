test_that("rope_range cor", {
  x <- cor.test(ToothGrowth$len, ToothGrowth$dose)
  expect_equal(rope_range(x), c(-0.05, 0.05), tolerance = 1e-3)
})

test_that("rope_range gaussian", {
  data(mtcars)
  mod <- lm(mpg ~ gear + hp, data = mtcars)
  expect_equal(
    rope_range(mod),
    c(-0.1 * sd(mtcars$mpg), 0.1 * sd(mtcars$mpg)),
    tolerance = 1e-3
  )
})

test_that("rope_range log gaussian", {
  data(iris)
  mod <- lm(log(Sepal.Length) ~ Species, data = iris)
  expect_equal(rope_range(mod), c(-0.01, 0.01), tolerance = 1e-3)
})

test_that("rope_range log gaussian 2", {
  data(mtcars)
  mod <- glm(mpg ~ gear + hp, data = mtcars, family = gaussian("log"))
  expect_equal(rope_range(mod), c(-0.01, 0.01), tolerance = 1e-3)
})

test_that("rope_range logistic", {
  data(mtcars)
  mod <- glm(am ~ gear + hp, data = mtcars, family = binomial())
  expect_equal(
    rope_range(mod),
    c(-1 * 0.1 * pi / sqrt(3), 0.1 * pi / sqrt(3)),
    tolerance = 1e-3
  )
})


test_that("rope_range", {
  skip_on_cran()

  # 2026-09-06: currently errors on CRAN, R devel Win
  #
  # Error in `compileCode(f, code, language = language, verbose = verbose)`: odel = model55b8683e76bd__namespace::model55b8683e76bd_; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]'
  #    22 |     return 0.5 * z.p.transpose() * z.inv_e_metric_ * z.p;
  #  Error in `sink(type = "output")`: invalid connection

  skip_if_not_or_load_if_installed("brms")
  model <- suppressWarnings(brms::brm(mpg ~ wt + gear, data = mtcars, iter = 300))

  expect_equal(
    rope_range(model),
    c(-0.6026948, 0.6026948),
    tolerance = 0.01
  )
})

test_that("rope_range (multivariate)", {
  skip_on_cran()

  # 2026-09-06: currently errors on CRAN, R devel Win
  #
  # Error in `compileCode(f, code, language = language, verbose = verbose)`: odel = model55b8683e76bd__namespace::model55b8683e76bd_; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]'
  #    22 |     return 0.5 * z.p.transpose() * z.inv_e_metric_ * z.p;
  #  Error in `sink(type = "output")`: invalid connection

  skip_if_not_or_load_if_installed("brms")
  model <- suppressWarnings(
    brms::brm(
      brms::bf(mvbind(mpg, disp) ~ wt + gear) + brms::set_rescor(TRUE),
      data = mtcars,
      iter = 300
    )
  )

  expect_equal(
    rope_range(model),
    list(
      mpg = c(-0.602694, 0.602694),
      disp = c(-12.393869, 12.393869)
    ),
    tolerance = 0.01
  )
})
