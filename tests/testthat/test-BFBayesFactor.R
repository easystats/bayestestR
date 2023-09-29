test_that("p_direction", {
  skip_if_not_or_load_if_installed("BayesFactor")
  set.seed(333)
  x <- BayesFactor::correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
  expect_equal(as.numeric(p_direction(x)), 0.9225, tolerance = 1)
})

test_that("p_direction: BF t.test one sample", {
  skip_if_not_or_load_if_installed("BayesFactor")
  data(sleep)
  diffScores <- sleep$extra[1:10] - sleep$extra[11:20]
  x <- BayesFactor::ttestBF(x = diffScores)
  expect_equal(as.numeric(p_direction(x)), 0.99675, tolerance = 1)
})


test_that("p_direction: BF t.test two samples", {
  skip_if_not_or_load_if_installed("BayesFactor")
  data(chickwts)
  chickwts <- chickwts[chickwts$feed %in% c("horsebean", "linseed"), ]
  chickwts$feed <- factor(chickwts$feed)
  x <- BayesFactor::ttestBF(formula = weight ~ feed, data = chickwts)
  expect_equal(as.numeric(p_direction(x)), 1, tolerance = 1)
})

test_that("p_direction: BF t.test meta-analytic", {
  skip_if_not_or_load_if_installed("BayesFactor")
  t <- c(-0.15, 2.39, 2.42, 2.43)
  N <- c(100, 150, 97, 99)
  x <- BayesFactor::meta.ttestBF(t = t, n1 = N, rscale = 1)
  expect_equal(as.numeric(p_direction(x)), 0.99975, tolerance = 1)
})

skip_if_not_or_load_if_installed("BayesFactor")

# ---------------------------
# "BF ANOVA"
data(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose)
levels(ToothGrowth$dose) <- c("Low", "Medium", "High")
x <- BayesFactor::anovaBF(len ~ supp * dose, data = ToothGrowth)
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 91.9, tol = 0.1)
})

# BF ANOVA Random ---------------------------

data(puzzles)
x <- BayesFactor::anovaBF(RT ~ shape * color + ID, data = puzzles, whichRandom = "ID")
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 91.9, tol = 0.1)
})


# ---------------------------
# "BF lm"
x <- BayesFactor::lmBF(len ~ supp + dose, data = ToothGrowth)
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 91.9, tol = 0.1)
})


x2 <- BayesFactor::lmBF(len ~ supp + dose + supp:dose, data = ToothGrowth)
x <- x / x2
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 91.9, tol = 0.1)
})


test_that("rope_range", {
  skip_if_not_or_load_if_installed("BayesFactor")
  x <- BayesFactor::lmBF(len ~ supp + dose, data = ToothGrowth)
  expect_equal(rope_range(x)[2], sd(ToothGrowth$len) / 10, tolerance = 1e-4)

  x <- BayesFactor::ttestBF(
    ToothGrowth$len[ToothGrowth$supp == "OJ"],
    ToothGrowth$len[ToothGrowth$supp == "VC"]
  )
  expect_equal(rope_range(x)[2], sd(ToothGrowth$len) / 10, tolerance = 1e-4)

  x <- BayesFactor::ttestBF(formula = len ~ supp, data = ToothGrowth)
  expect_equal(rope_range(x)[2], sd(ToothGrowth$len) / 10, tolerance = 1e-4)

  # else
  x <- BayesFactor::correlationBF(ToothGrowth$len, ToothGrowth$dose)
  expect_equal(rope_range(x, verbose = FALSE), c(-0.05, 0.05))
})
