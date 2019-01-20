
bayestestR <img src='man/figures/logo.svg' align="right" height="139" />
========================================================================

[![Build Status](https://travis-ci.org/DominiqueMakowski/bayestestR.svg?branch=master)](https://travis-ci.org/DominiqueMakowski/bayestestR) [![codecov](https://codecov.io/gh/DominiqueMakowski/bayestestR/branch/master/graph/badge.svg)](https://codecov.io/gh/DominiqueMakowski/bayestestR) [![HitCount](http://hits.dwyl.io/DominiqueMakowski/bayestestR.svg)](http://hits.dwyl.io/DominiqueMakowski/bayestestR)[![Documentation](https://img.shields.io/badge/documentation-bayestestR-orange.svg?colorB=E91E63)](https://dominiquemakowski.github.io/bayestestR/) <!-- Add this to the README manually! --> <!-- [![Documentation](https://img.shields.io/badge/documentation-bayestestR-orange.svg?colorB=E91E63)](https://dominiquemakowski.github.io/bayestestR/) -->

Goal
----

`bayestestR` is a lightweight package providing utilities to describe posterior distributions and Bayesian models.

Installation
------------

Run the following:

``` r
install.packages("devtools")
devtools::install_github("DominiqueMakowski/bayestestR")
```

``` r
library("bayestestR")
```

Functions
---------

:boom: Check-out the [**documentation**](https://dominiquemakowski.github.io/bayestestR/) :boom:

### Posterior Description

-   **`hdi()`**: Compute the Highest Density Interval (HDI) of a posterior distribution, i.e., the interval which contains all points within the interval have a higher probability density than points outside the interval. The HDI is used in the context of Bayesian posterior characterisation as Credible Interval (CI).

``` r
hdi(posterior = rnorm(1000), CI = 90)
```

-   **`map_estimate()`**: Find the Highest Maximum A Posteriori (MAP) estimate of a posterior. It corresponds to the 'peak' of the posterior distribution.

``` r
map_estimate(rnorm(1000, 1, 1))
```

-   **`rope()`**: Compute the proportion of the HDI of a posterior distribution that lies within a region of practical equivalence.

``` r
rope(posterior = rnorm(1000, 1, 1), bounds = c(-0.1, 0.1))
```

### Null-Hypothesis Significance Testing (NHST)

-   **`rope_test()`**: Perform a Test for Practical Equivalence based on the "HDI+ROPE decision rule" (Kruschke 2018) to check whether parameter values should be accepted or rejected against an explicitely formulated "null hypothesis".

``` r
rope_test(posterior = rnorm(1000, 1, 1), bounds = c(-0.1, 0.1))
```

-   **`p_rope()`**: The ROPE-based p-value represents the maximum Credible Interval (HDI) that does not contain (positive values) or is entirely contained (negative values) in the negligible values space defined by the ROPE. A ROPE-based p of 97% means that there is a probability of .97 that a parameter (desccribed by its posterior distribution) is outside the ROPE.

``` r
p_rope(posterior = rnorm(1000, 1, 1), bounds = c(-0.1, 0.1))
```

-   **`p_direction()`**: Compute the Probability of Direction (p, also known as the Maximum Probability of Effect - MPE), a Bayesian equivalent of the p-value (altough differently expressed). It varies between 50% and 100% and can be interpreted as the probability that a parameter (described by its posterior distribution) is positive or negative (following the median's sign). It is defined as the proportion of the posterior distribution of the median's sign. It is used as an index of effect existence, i.e., whether the probability that the effect is in the same direction than the point-estimate (independently of the effect's size or significance). This p-value is fairly similar to its frequentist counterpart (i.e., is strongly correlated).

``` r
p_direction(rnorm(1000, mean = 1, sd = 1))
```

-   **`p_map()`**: Compute a Bayesian equivalent of the p-value, related to the odds that a parameter (described by its posterior distribution) has againt the null hypothesis (h0) using Mills' (2014, 2017) Objective Bayesian Hypothesis Testing paradigm. It is mathematically based on the density at the Maximum A Priori (MAP).

``` r
p_map(posterior = rnorm(1000, 1, 1))
```

### Utilities

-   **`rnorm_perfect()`**: Generate a sample of size n with a near-perfect normal distribution.

``` r
rnorm_perfect(n = 10)
```

Credits
-------

You can cite the package as following:

-   Makowski, (2019). *Understand and Describe Bayesian Models and Posterior Distributions using BayestestR*. CRAN. doi: .

Please remember that parts of the code in this package was inspired / shamelessly copied from other great packages that you must check out and cite, such as [sjstats](https://github.com/strengejacke/sjstats) or [BayesTesting.jl](https://github.com/tszanalytics/BayesTesting.jl). All credits go to their authors.
