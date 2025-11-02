# Convert between Probability of Direction (pd) and p-value.

Enables a conversion between Probability of Direction (pd) and p-value.

## Usage

``` r
pd_to_p(pd, ...)

# S3 method for class 'numeric'
pd_to_p(pd, direction = "two-sided", verbose = TRUE, ...)

p_to_pd(p, direction = "two-sided", ...)

convert_p_to_pd(p, direction = "two-sided", ...)

convert_pd_to_p(pd, ...)
```

## Arguments

- pd:

  A Probability of Direction (pd) value (between 0 and 1). Can also be a
  data frame with a column named `pd`, `p_direction`, or `PD`, as
  returned by
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).
  In this case, the column is converted to p-values and the new data
  frame is returned.

- ...:

  Arguments passed to or from other methods.

- direction:

  What type of p-value is requested or provided. Can be `"two-sided"`
  (default, two tailed) or `"one-sided"` (one tailed).

- verbose:

  Toggle off warnings.

- p:

  A p-value.

## Value

A p-value or a data frame with a p-value column.

## Details

Conversion is done using the following equation (see *Makowski et al.,
2019*):

When `direction = "two-sided"`

p = 2 \* (1 - p_(d))

When `direction = "one-sided"`

p = 1 - p_(d)

Note that this conversion is only valid when the lowest possible values
of pd is 0.5 - i.e., when the posterior represents continuous parameter
space (see
[`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)).
If any pd \< 0.5 are detected, they are converted to a p of 1, and a
warning is given.

## References

Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D.
(2019). *Indices of Effect Existence and Significance in the Bayesian
Framework*. Frontiers in Psychology 2019;10:2767.
[doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

## Examples

``` r
pd_to_p(pd = 0.95)
#> [1] 0.1
pd_to_p(pd = 0.95, direction = "one-sided")
#> [1] 0.05
```
