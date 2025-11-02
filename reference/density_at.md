# Density Probability at a Given Value

Compute the density value at a given point of a distribution (i.e., the
value of the `y` axis of a value `x` of a distribution).

## Usage

``` r
density_at(posterior, x, precision = 2^10, method = "kernel", ...)
```

## Arguments

- posterior:

  Vector representing a posterior distribution.

- x:

  The value of which to get the approximate probability.

- precision:

  Number of points of density data. See the `n` parameter in `density`.

- method:

  Density estimation method. Can be `"kernel"` (default), `"logspline"`
  or `"KernSmooth"`.

- ...:

  Currently not used.

## Examples

``` r
library(bayestestR)
posterior <- distribution_normal(n = 10)
density_at(posterior, 0)
#> [1] 0.3206131
density_at(posterior, c(0, 1))
#> [1] 0.3206131 0.2374056
```
