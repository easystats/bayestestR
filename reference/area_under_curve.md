# Area under the Curve (AUC)

Based on the DescTools `AUC` function. It can calculate the area under
the curve with a naive algorithm or a more elaborated spline approach.
The curve must be given by vectors of xy-coordinates. This function can
handle unsorted x values (by sorting x) and ties for the x values (by
ignoring duplicates).

## Usage

``` r
area_under_curve(x, y, method = c("trapezoid", "step", "spline"), ...)

auc(x, y, method = c("trapezoid", "step", "spline"), ...)
```

## Arguments

- x:

  Vector of x values.

- y:

  Vector of y values.

- method:

  Method to compute the Area Under the Curve (AUC). Can be `"trapezoid"`
  (default), `"step"` or `"spline"`. If "trapezoid", the curve is formed
  by connecting all points by a direct line (composite trapezoid rule).
  If "step" is chosen then a stepwise connection of two points is used.
  For calculating the area under a spline interpolation the splinefun
  function is used in combination with integrate.

- ...:

  Arguments passed to or from other methods.

## See also

DescTools

## Examples

``` r
library(bayestestR)
posterior <- distribution_normal(1000)

dens <- estimate_density(posterior)
dens <- dens[dens$x > 0, ]
x <- dens$x
y <- dens$y

area_under_curve(x, y, method = "trapezoid")
#> [1] 0.4980638
area_under_curve(x, y, method = "step")
#> [1] 0.4992903
area_under_curve(x, y, method = "spline")
#> [1] 0.4980639
```
