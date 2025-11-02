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
#> 1  24.33140          1          1   25.69413
#> 2  24.33140          2          1   25.69413
#> 3  24.33140          3          1   25.69413
#> 4  17.12509          4          1   16.64822
#> 5  17.12509          5          1   16.64822
#> 6  17.12509          6          1   16.64822
# }
```
