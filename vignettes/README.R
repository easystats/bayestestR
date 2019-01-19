## ----eval=FALSE, message=FALSE, warning=FALSE----------------------------
#  install.packages("devtools")
#  library("devtools")
#  install_github("DominiqueMakowski/bayestestR")

## ----message=FALSE, warning=FALSE----------------------------------------
library("bayestestR")

## ----message=FALSE, warning=FALSE, results='hide'------------------------
posterior <- rnorm(1000)
hdi(posterior, CI = 90)
hdi(posterior, CI = c(80, 90, 95))

## ----message=FALSE, warning=FALSE, results='hide'------------------------
map_estimate(rnorm(1000, 1, 1))

## ----message=FALSE, warning=FALSE, results='hide'------------------------
rope(posterior = rnorm(1000, 1, 1), bounds = c(-0.1, 0.1))

## ----message=FALSE, warning=FALSE, results='hide'------------------------
rope_test(posterior = rnorm(1000, 1, 1), bounds = c(-0.1, 0.1))

## ----message=FALSE, warning=FALSE, results='hide'------------------------
p_rope(posterior = rnorm(1000, 1, 1), bounds = c(-0.1, 0.1))

## ----message=FALSE, warning=FALSE, results='hide'------------------------
p_direction(rnorm(1000, mean = 1, sd = 1))

## ----message=FALSE, warning=FALSE, results='hide'------------------------
p_map(posterior = rnorm(1000, 1, 1))

## ----message=FALSE, warning=FALSE----------------------------------------
x <- rnorm_perfect(n = 10)
plot(density(x))

