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

test_that("rope_range gaussian, transformed response", {
  data(mtcars)
  mod <- lm(scale(mpg) ~ gear + hp, data = mtcars)
  expect_equal(rope_range(mod), c(-0.1, 0.1), tolerance = 1e-3)

  mod <- lm(sqrt(mpg) ~ gear + hp, data = mtcars)
  expect_equal(
    rope_range(mod),
    c(-0.1 * sd(sqrt(mtcars$mpg)), 0.1 * sd(sqrt(mtcars$mpg))),
    tolerance = 1e-3
  )

  mod <- lm(I(mpg * 10) ~ gear + hp, data = mtcars)
  expect_equal(
    rope_range(mod),
    c(-0.1 * sd(mtcars$mpg * 10), 0.1 * sd(mtcars$mpg * 10)),
    tolerance = 1e-3
  )
})

test_that("rope_range gaussian, user-defined response function without model frame", {
  data(mtcars)
  times_ten <- function(y) y * 10
  mod <- lm(times_ten(mpg) ~ gear + hp, data = mtcars)
  # the fallback without a model frame finds the function in the formula's environment
  expect_equal(
    bayestestR:::.response_term_values(mod, "times_ten(mpg)", model_frame = NULL),
    mtcars$mpg * 10,
    ignore_attr = TRUE
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

test_that("rope_range (multivariate, ordinal with weights)", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("brms")

  set.seed(123)
  d <- data.frame(
    y_1 = factor(sample(1:3, 60, TRUE), ordered = TRUE),
    y_2 = factor(sample(1:3, 60, TRUE), ordered = TRUE),
    n = sample(1:3, 60, TRUE)
  )
  model <- suppressWarnings(
    brms::brm(
      brms::bf(mvbind(y_1, y_2) | resp_weights(n) ~ 1, family = brms::cumulative()) +
        brms::set_rescor(FALSE),
      data = d,
      iter = 300,
      chains = 1,
      refresh = 0
    )
  )

  # one ROPE per response, the weights are not a response
  expect_no_warning(out <- rope_range(model))
  expect_equal(
    out,
    list(
      y1 = c(-0.1 * pi / sqrt(3), 0.1 * pi / sqrt(3)),
      y2 = c(-0.1 * pi / sqrt(3), 0.1 * pi / sqrt(3))
    ),
    tolerance = 1e-3
  )
})
