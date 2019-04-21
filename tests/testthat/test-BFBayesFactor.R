if (require("testthat") && require("bayestestR") && require("BayesFactor")) {

context("BF correlation")
x <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 91.9, tol=0.1)
})


# ---------------------------
context("BF t.test one sample")
data(sleep)
diffScores <- sleep$extra[1:10] - sleep$extra[11:20]
x <- ttestBF(x = diffScores)
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 99.88, tol=0.1)
})


# ---------------------------
context("BF t.test two samples")
data(chickwts)
chickwts <- chickwts[chickwts$feed %in% c("horsebean","linseed"),]
chickwts$feed <- factor(chickwts$feed)
x <- ttestBF(formula = weight ~ feed, data = chickwts)
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 100, tol=0.1)
})

# ---------------------------
context("BF t.test meta-analytic")
t <- c(-.15, 2.39, 2.42, 2.43)
N <- c(100, 150, 97, 99)
x <- meta.ttestBF(t=t, n1=N, rscale=1)
test_that("p_direction", {
  expect_equal(as.numeric(p_direction(x)), 100, tol=0.1)
})

# ---------------------------
# context("BF ANOVA")
# data(ToothGrowth)
# ToothGrowth$dose <- factor(ToothGrowth$dose)
# levels(ToothGrowth$dose) <- c("Low", "Medium", "High")
# x <- anovaBF(len ~ supp*dose, data=ToothGrowth)
# test_that("p_direction", {
#   expect_equal(as.numeric(p_direction(x)), 91.9, tol=0.1)
# })

# ---------------------------
# context("BF ANOVA Random")
# data(puzzles)
# x <- anovaBF(RT ~ shape*color + ID, data = puzzles, whichRandom="ID")
# test_that("p_direction", {
#   expect_equal(as.numeric(p_direction(x)), 91.9, tol=0.1)
# })


# ---------------------------
# context("BF lm")
# x <- lmBF(len ~ supp + dose, data = ToothGrowth)
# test_that("p_direction", {
#   expect_equal(as.numeric(p_direction(x)), 91.9, tol=0.1)
# })


# x2 <- lmBF(len ~ supp + dose + supp:dose, data = ToothGrowth)
# x <- x / x2
# test_that("p_direction", {
#   expect_equal(as.numeric(p_direction(x)), 91.9, tol=0.1)
# })
}