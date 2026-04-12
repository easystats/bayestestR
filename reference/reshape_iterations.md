# Reshape estimations with multiple iterations (draws) to long format

Reshape a wide data.frame of iterations (such as posterior draws or
bootsrapped samples) as columns to long format. Instead of having all
iterations as columns (e.g., `iter_1, iter_2, ...`), will return 3
columns with the `\*_index` (the previous index of the row), the
`\*_group` (the iteration number) and the `\*_value` (the value of said
iteration).

## Usage

``` r
reshape_iterations(x, prefix = c("draw", "iter", "iteration", "sim"))

reshape_draws(x, prefix = c("draw", "iter", "iteration", "sim"))
```

## Arguments

- x:

  A data.frame containing posterior draws obtained from
  `estimate_response` or `estimate_link`.

- prefix:

  The prefix of the draws (for instance, `"iter_"` for columns named as
  `iter_1, iter_2, iter_3`). If more than one are provided, will search
  for the first one that matches.

## Value

Data frame of reshaped draws in long format.

## Examples

``` r
# \donttest{
if (require("rstanarm")) {
  model <- stan_glm(mpg ~ am, data = mtcars, refresh = 0)
  draws <- insight::get_predicted(model)
  long_format <- reshape_iterations(draws)
  head(long_format)
}
#>   Predicted iter_index iter_group iter_value
#> 1  24.38890          1          1   24.05244
#> 2  24.38890          2          1   24.05244
#> 3  24.38890          3          1   24.05244
#> 4  17.14047          4          1   17.27725
#> 5  17.14047          5          1   17.27725
#> 6  17.14047          6          1   17.27725
# }
```
