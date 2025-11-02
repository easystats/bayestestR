# Empirical Distributions

Generate a sequence of n-quantiles, i.e., a sample of size `n` with a
near-perfect distribution.

## Usage

``` r
distribution(type = "normal", ...)

distribution_custom(n, type = "norm", ..., random = FALSE)

distribution_beta(n, shape1, shape2, ncp = 0, random = FALSE, ...)

distribution_binomial(n, size = 1, prob = 0.5, random = FALSE, ...)

distribution_binom(n, size = 1, prob = 0.5, random = FALSE, ...)

distribution_cauchy(n, location = 0, scale = 1, random = FALSE, ...)

distribution_chisquared(n, df, ncp = 0, random = FALSE, ...)

distribution_chisq(n, df, ncp = 0, random = FALSE, ...)

distribution_gamma(n, shape, scale = 1, random = FALSE, ...)

distribution_mixture_normal(n, mean = c(-3, 3), sd = 1, random = FALSE, ...)

distribution_normal(n, mean = 0, sd = 1, random = FALSE, ...)

distribution_gaussian(n, mean = 0, sd = 1, random = FALSE, ...)

distribution_nbinom(n, size, prob, mu, phi, random = FALSE, ...)

distribution_poisson(n, lambda = 1, random = FALSE, ...)

distribution_student(n, df, ncp, random = FALSE, ...)

distribution_t(n, df, ncp, random = FALSE, ...)

distribution_student_t(n, df, ncp, random = FALSE, ...)

distribution_tweedie(n, xi = NULL, mu, phi, power = NULL, random = FALSE, ...)

distribution_uniform(n, min = 0, max = 1, random = FALSE, ...)
```

## Arguments

- type:

  Can be any of the names from base R's
  [Distributions](https://rdrr.io/r/stats/Distributions.html), like
  `"cauchy"`, `"pois"` or `"beta"`.

- ...:

  Arguments passed to or from other methods.

- n:

  the number of observations

- random:

  Generate near-perfect or random (simple wrappers for the base R `r*`
  functions) distributions. When `random = FALSE`, these function return
  `q*(ppoints(n), ...)`.

- shape1, shape2:

  non-negative parameters of the Beta distribution.

- ncp:

  non-centrality parameter.

- size:

  number of trials (zero or more).

- prob:

  probability of success on each trial.

- location, scale:

  location and scale parameters.

- df:

  degrees of freedom (non-negative, but can be non-integer).

- shape:

  Shape parameter.

- mean:

  vector of means.

- sd:

  vector of standard deviations.

- mu:

  the mean

- phi:

  Corresponding to `glmmTMB`'s implementation of nbinom distribution,
  where `size=mu/phi`.

- lambda:

  vector of (non-negative) means.

- xi:

  For tweedie distributions, the value of `xi` such that the variance is
  `var(Y) = phi * mu^xi`.

- power:

  Alias for `xi`.

- min, max:

  lower and upper limits of the distribution. Must be finite.

## Examples

``` r
library(bayestestR)
x <- distribution(n = 10)
plot(density(x))


x <- distribution(type = "gamma", n = 100, shape = 2)
plot(density(x))
```
