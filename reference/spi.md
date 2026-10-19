# Shortest Probability Interval (SPI)

`spi()` is deprecated. Use `hdi(method = "density")` instead, which
returns the same interval. See
[`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md) for
how the interval is computed.

## Usage

``` r
spi(x, ...)
```

## Arguments

- x:

  Vector representing a posterior distribution, or a data frame of such
  vectors. Can also be a Bayesian model. See
  [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md).

- ...:

  Arguments passed to
  [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md).

## Value

A data frame with following columns:

- `Parameter` The model parameter(s), if `x` is a model-object. If `x`
  is a vector, this column is missing.

- `CI` The probability of the credible interval.

- `CI_low`, `CI_high` The lower and upper credible interval limits for
  the parameters.

## References

Liu, Y., Gelman, A., & Zheng, T. (2015). Simulation-efficient shortest
probability intervals. Statistics and Computing, 25(4), 809–819.
https://doi.org/10.1007/s11222-015-9563-8

## Examples

``` r
library(bayestestR)

posterior <- rnorm(1000)
# instead of spi(posterior), use:
hdi(posterior, method = "density")
#> 95% SPI: [-2.19, 1.93]
```
