# Confidence/Credible/Compatibility Interval (CI)

Compute Confidence/Credible/Compatibility Intervals (CI) or Support
Intervals (SI) for Bayesian and frequentist models. The Documentation is
accessible for:

## Usage

``` r
ci(x, ...)

# S3 method for class 'numeric'
ci(x, ci = 0.95, method = "ETI", verbose = TRUE, BF = 1, ...)

# S3 method for class 'data.frame'
ci(x, ci = 0.95, method = "ETI", BF = 1, rvar_col = NULL, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
ci(
  x,
  ci = 0.95,
  method = "ETI",
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  BF = 1,
  ...
)
```

## Arguments

- x:

  A `stanreg` or `brmsfit` model, or a vector representing a posterior
  distribution.

- ...:

  Currently not used.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default to `0.95` (`95%`).

- method:

  Can be
  ["ETI"](https://easystats.github.io/bayestestR/reference/eti.md)
  (default),
  ["HDI"](https://easystats.github.io/bayestestR/reference/hdi.md),
  ["BCI"](https://easystats.github.io/bayestestR/reference/bci.md),
  ["SPI"](https://easystats.github.io/bayestestR/reference/spi.md) or
  ["SI"](https://easystats.github.io/bayestestR/reference/si.md).

- verbose:

  Toggle off warnings.

- BF:

  The amount of support required to be included in the support interval.

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

- effects:

  Should variables for fixed effects (`"fixed"`), random effects
  (`"random"`) or both (`"all"`) be returned? Only applies to mixed
  models. May be abbreviated.

  For models of from packages **brms** or **rstanarm** there are
  additional options:

  - `"fixed"` returns fixed effects.

  - `"random_variance"` return random effects parameters (variance and
    correlation components, e.g. those parameters that start with `sd_`
    or `cor_`).

  - `"grouplevel"` returns random effects group level estimates, i.e.
    those parameters that start with `r_`.

  - `"random"` returns both `"random_variance"` and `"grouplevel"`.

  - `"all"` returns fixed effects and random effects variances.

  - `"full"` returns all parameters.

- component:

  Which type of parameters to return, such as parameters for the
  conditional model, the zero-inflated part of the model, the dispersion
  term, etc. See details in section *Model Components*. May be
  abbreviated. Note that the *conditional* component also refers to the
  *count* or *mean* component - names may differ, depending on the
  modeling package. There are three convenient shortcuts (not applicable
  to *all* model classes):

  - `component = "all"` returns all possible parameters.

  - If `component = "location"`, location parameters such as
    `conditional`, `zero_inflated`, `smooth_terms`, or `instruments` are
    returned (everything that are fixed or random effects - depending on
    the `effects` argument - but no auxiliary parameters).

  - For `component = "distributional"` (or `"auxiliary"`), components
    like `sigma`, `dispersion`, `beta` or `precision` (and other
    auxiliary parameters) are returned.

- parameters:

  Regular expression pattern that describes the parameters that should
  be returned. Meta-parameters (like `lp__` or `prior_`) are filtered by
  default, so only parameters that typically appear in the
  [`summary()`](https://rdrr.io/r/base/summary.html) are returned. Use
  `parameters` to select specific parameters for the output.

## Value

A data frame with following columns:

- `Parameter` The model parameter(s), if `x` is a model-object. If `x`
  is a vector, this column is missing.

- `CI` The probability of the credible interval.

- `CI_low`, `CI_high` The lower and upper credible interval limits for
  the parameters.

## Details

- [Bayesian
  models](https://easystats.github.io/bayestestR/articles/credible_interval.html)

- [Frequentist
  models](https://easystats.github.io/parameters/reference/ci.default.html)

## Note

When it comes to interpretation, we recommend thinking of the CI in
terms of an "uncertainty" or "compatibility" interval, the latter being
defined as "Given any value in the interval and the background
assumptions, the data should not seem very surprising" (*Gelman &
Greenland 2019*).

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Model components

Possible values for the `component` argument depend on the model class.
Following are valid options:

- `"all"`: returns all model components, applies to all models, but will
  only have an effect for models with more than just the conditional
  model component.

- `"conditional"`: only returns the conditional component, i.e. "fixed
  effects" terms from the model. Will only have an effect for models
  with more than just the conditional model component.

- `"smooth_terms"`: returns smooth terms, only applies to GAMs (or
  similar models that may contain smooth terms).

- `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.

- `"location"`: returns location parameters such as `conditional`,
  `zero_inflated`, or `smooth_terms` (everything that are fixed or
  random effects - depending on the `effects` argument - but no
  auxiliary parameters).

- `"distributional"` (or `"auxiliary"`): components like `sigma`,
  `dispersion`, `beta` or `precision` (and other auxiliary parameters)
  are returned.

For models of class `brmsfit` (package **brms**), even more options are
possible for the `component` argument, which are not all documented in
detail here. See also
[[`?insight::find_parameters`](https://easystats.github.io/insight/reference/find_parameters.html)](https://easystats.github.io/insight/reference/find_parameters.BGGM.html).

## References

Gelman A, Greenland S. Are confidence intervals better termed
"uncertainty intervals"? BMJ 2019;l5381. 10.1136/bmj.l5381

## See also

Other ci:
[`bci()`](https://easystats.github.io/bayestestR/reference/bci.md),
[`eti()`](https://easystats.github.io/bayestestR/reference/eti.md),
[`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
[`si()`](https://easystats.github.io/bayestestR/reference/si.md),
[`spi()`](https://easystats.github.io/bayestestR/reference/spi.md)

## Examples

``` r
library(bayestestR)

posterior <- rnorm(1000)
ci(posterior, method = "ETI")
#> 95% ETI: [-1.93, 1.97]
ci(posterior, method = "HDI")
#> 95% HDI: [-1.85, 2.04]

df <- data.frame(replicate(4, rnorm(100)))
ci(df, method = "ETI", ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> Parameter |       80% ETI |       89% ETI |       95% ETI
#> ---------------------------------------------------------
#> X1        | [-1.46, 1.27] | [-1.60, 1.62] | [-1.96, 1.96]
#> X2        | [-1.19, 1.34] | [-1.42, 1.48] | [-2.02, 1.84]
#> X3        | [-1.26, 1.18] | [-1.72, 1.51] | [-2.20, 1.66]
#> X4        | [-1.01, 1.61] | [-1.33, 1.73] | [-1.85, 2.22]
ci(df, method = "HDI", ci = c(0.80, 0.89, 0.95))
#> Highest Density Interval
#> 
#> Parameter |       80% HDI |       89% HDI |       95% HDI
#> ---------------------------------------------------------
#> X1        | [-1.64, 1.02] | [-1.55, 1.73] | [-1.97, 1.80]
#> X2        | [-1.44, 0.99] | [-1.44, 1.49] | [-2.29, 1.72]
#> X3        | [-0.98, 1.35] | [-1.46, 1.56] | [-2.34, 1.52]
#> X4        | [-1.00, 1.62] | [-1.22, 1.89] | [-1.47, 2.39]

model <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt,
  data = mtcars, chains = 2, iter = 200, refresh = 0
))
ci(model, method = "ETI", ci = c(0.80, 0.89))
#> Equal-Tailed Interval
#> 
#> Parameter   |        80% ETI |        89% ETI | Effects |   Component
#> ---------------------------------------------------------------------
#> (Intercept) | [34.88, 39.92] | [34.57, 40.72] |   fixed | conditional
#> wt          | [-6.12, -4.71] | [-6.44, -4.53] |   fixed | conditional
ci(model, method = "HDI", ci = c(0.80, 0.89))
#> Highest Density Interval 
#> 
#> Parameter   |        80% HDI |        89% HDI
#> ---------------------------------------------
#> (Intercept) | [34.55, 39.51] | [34.47, 40.19]
#> wt          | [-6.15, -4.75] | [-6.14, -4.41]
bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
ci(bf, method = "ETI")
#> Equal-Tailed Interval
#> 
#> Parameter  |      95% ETI
#> -------------------------
#> Difference | [0.73, 1.08]
ci(bf, method = "HDI")
#> Highest Density Interval
#> 
#> Parameter  |      95% HDI
#> -------------------------
#> Difference | [0.74, 1.09]
model <- emmeans::emtrends(model, ~1, "wt", data = mtcars)
ci(model, method = "ETI")
#> Equal-Tailed Interval
#> 
#> X1      |        95% ETI
#> ------------------------
#> overall | [-6.69, -4.42]
ci(model, method = "HDI")
#> Highest Density Interval
#> 
#> X1      |        95% HDI
#> ------------------------
#> overall | [-6.53, -4.38]
```
