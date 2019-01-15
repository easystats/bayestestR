# bayestestR

[![Build Status](https://travis-ci.org/DominiqueMakowski/bayestestR.svg?branch=master)](https://travis-ci.org/DominiqueMakowski/bayestestR)
[![codecov](https://codecov.io/gh/DominiqueMakowski/bayestestR/branch/master/graph/badge.svg)](https://codecov.io/gh/DominiqueMakowski/bayestestR)
[![HitCount](http://hits.dwyl.io/DominiqueMakowski/bayestestR.svg)](http://hits.dwyl.io/DominiqueMakowski/bayestestR)

Utilities for Analyzing Bayesian Posterior Distributions and Models


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


- **`bayes_p()`**: Compute a Bayesian equivalent p value, related to the odds that a parameter (described by its posterior distribution) has againt the null hypothesis (h0) using Mills' (2014, 2017) Objective Bayesian Hypothesis Testing paradigm.

```R
bayes_p(posterior = rnorm(1000, 0, 1))
bayes_p(posterior = rnorm(1000, 10, 1))
```


- **`rope()`**: Compute the proportion of the HDI of a posterior distribution that lies within a region of practical equivalence.

```R
rope(posterior = rnorm(1000, 0, 0.01), rope = c(-0.1, 0.1))
rope(posterior = rnorm(1000, 0, 1), rope = c(-0.1, 0.1))
rope(posterior = rnorm(1000, 1, 0.01), rope = c(-0.1, 0.1))
```

- **`rope_overlap()`**: Compute how much of the posterior distribution is within a distributed region of practical equivalence.

```R
rope_overlap(posterior = rnorm(1000, 0, 0.01), rope = c(-0.1, 0.1))
rope_overlap(posterior = rnorm(1000, 0, 1), rope = c(-0.1, 0.1))
rope_overlap(posterior = rnorm(1000, 1, 0.01), rope = c(-0.1, 0.1))
```

- **`rope_test()`**: Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke 2018) to check whether parameter values should be accepted or rejected against an explicitely formulated "null hypothesis".

```R
rope_test(posterior = rnorm(1000, 0, 0.01), rope = c(-0.1, 0.1))
rope_test(posterior = rnorm(1000, 0, 1), rope = c(-0.1, 0.1))
rope_test(posterior = rnorm(1000, 1, 0.01), rope = c(-0.1, 0.1))
```


- **`pd()`**: Compute the Probability of Direction (pd, also known as the Maximum Probability of Effect - MPE). It varies between 50\% and 100\% and can be interpreted as the probability that a parameter (described by its posterior distribution) is positive or negative (following  the median’s sign). It is defined as the proportion of the posterior distribution of the median's sign.

```R
# Compute the pdSimulate a posterior distribution of mean 1 and SD 1
pd(rnorm(1000, mean = 1, sd = 1))
```

- **`hdi()`**: Compute the highest density interval (HDI) of a posterior distribution, i.e., the interval which contains all points within the interval have a higher probability density than points outside the interval.

```R
posterior <- rnorm(1000)
hdi(posterior, prob = 0.9)
hdi(posterior, prob = c(0.8, 0.9, 0.95))
```

- **`map_estimate()`**: Find the Highest Maximum A Posteriori (MAP) estimate of a posterior.

```R
posterior <- rnorm(1000)
map_estimate(posterior)
```

- **`rnorm_perfect()`**: Generate a sample of size n with a near-perfect normal distribution.

```R
x <- rnorm_perfect(n = 10)
plot(density(x))
```

- **`overlap()`**: Calculate the overlap coefficient between two kernel density estimates.


```R
overlap(x=rnorm(1000, 0, 1), y=rnorm(1000, 1, 1))
```

## Credits

Parts of the code in this package was inspired / shamelessly copied from other great packages that you must check out and cite, such as [sjstats](https://github.com/strengejacke/sjstats) or [BayesTesting.jl](https://github.com/tszanalytics/BayesTesting.jl). All credits go to their authors.