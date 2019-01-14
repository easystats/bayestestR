# bayestestR

[![Build Status](https://travis-ci.org/DominiqueMakowski/bayestestR.svg?branch=master)](https://travis-ci.org/DominiqueMakowski/bayestestR)
[![codecov](https://codecov.io/gh/DominiqueMakowski/bayestestR/branch/master/graph/badge.svg)](https://codecov.io/gh/DominiqueMakowski/bayestestR)
[![HitCount](http://hits.dwyl.io/DominiqueMakowski/bayestestR.svg)](http://hits.dwyl.io/DominiqueMakowski/bayestestR)

Describe your Posterior Distributions


---


## Goal

`bayestestR` is a lightweight package providing utilities to describe posterior distributions and Bayesian models.


## Installation


Run the following:

```R
install.packages("devtools")
library("devtools")
install_github("DominiqueMakowski/bayestestR")
library("bayestestR")
```


## Functions

- **`pd()`**: Compute the Probability of Direction (pd, also known as the Maximum Probability of Effect - MPE). It varies between 50% and 100% and can be interpreted as the probability that a parameter (described by its posterior distribution) is positive or negative (following  the median’s sign). It is defined as the proportion of the posterior distribution of the median's sign.

```R
# Compute the pdSimulate a posterior distribution of mean 1 and SD 1
pd(rnorm(1000, mean = 1, sd = 1))
```

- **`rnorm_perfect()`**: Generate a sample of size n with a near-perfect normal distribution.

```R
x <- rnorm_perfect(n = 10)
plot(density(x))
```