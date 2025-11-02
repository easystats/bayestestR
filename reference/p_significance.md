# Practical Significance (ps)

Compute the probability of **Practical Significance** (***ps***), which
can be conceptualized as a unidirectional equivalence test. It returns
the probability that effect is above a given threshold corresponding to
a negligible effect in the median's direction. Mathematically, it is
defined as the proportion of the posterior distribution of the median
sign above the threshold.

## Usage

``` r
p_significance(x, ...)

# S3 method for class 'numeric'
p_significance(x, threshold = "default", ...)

# S3 method for class 'get_predicted'
p_significance(
  x,
  threshold = "default",
  use_iterations = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
p_significance(x, threshold = "default", rvar_col = NULL, ...)

# S3 method for class 'brmsfit'
p_significance(
  x,
  threshold = "default",
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  Vector representing a posterior distribution. Can also be a `stanreg`
  or `brmsfit` model.

- ...:

  Currently not used.

- threshold:

  The threshold value that separates significant from negligible effect,
  which can have following possible values:

  - `"default"`, in which case the range is set to `0.1` if input is a
    vector, and based on
    [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
    if a (Bayesian) model is provided.

  - a single numeric value (e.g., 0.1), which is used as range around
    zero (i.e. the threshold range is set to -0.1 and 0.1, i.e. reflects
    a symmetric interval)

  - a numeric vector of length two (e.g., `c(-0.2, 0.1)`), useful for
    asymmetric intervals

  - a list of numeric vectors, where each vector corresponds to a
    parameter

  - a list of *named* numeric vectors, where names correspond to
    parameter names. In this case, all parameters that have no matching
    name in `threshold` will be set to `"default"`.

- use_iterations:

  Logical, if `TRUE` and `x` is a `get_predicted` object, (returned by
  [`insight::get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html)),
  the function is applied to the iterations instead of the predictions.
  This only applies to models that return iterations for predicted
  values (e.g., `brmsfit` models).

- verbose:

  Toggle off warnings.

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

Values between 0 and 1 corresponding to the probability of practical
significance (ps).

## Details

`p_significance()` returns the proportion of a probability distribution
(`x`) that is outside a certain range (the negligible effect, or ROPE,
see argument `threshold`). If there are values of the distribution both
below and above the ROPE, `p_significance()` returns the higher
probability of a value being outside the ROPE. Typically, this value
should be larger than 0.5 to indicate practical significance. However,
if the range of the negligible effect is rather large compared to the
range of the probability distribution `x`, `p_significance()` will be
less than 0.5, which indicates no clear practical significance.

## Note

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

## Examples

``` r
library(bayestestR)

# Simulate a posterior distribution of mean 1 and SD 1
# ----------------------------------------------------
posterior <- rnorm(1000, mean = 1, sd = 1)
p_significance(posterior)
#> Practical Significance (threshold: 0.10)
#> 
#> Parameter |   ps
#> ----------------
#> Posterior | 0.80

# Simulate a dataframe of posterior distributions
# -----------------------------------------------
df <- data.frame(replicate(4, rnorm(100)))
p_significance(df)
#> Practical Significance (threshold: 0.10)
#> 
#> Parameter |   ps
#> ----------------
#> X1        | 0.51
#> X2        | 0.53
#> X3        | 0.46
#> X4        | 0.47
# \donttest{
# rstanarm models
# -----------------------------------------------
model <- rstanarm::stan_glm(mpg ~ wt + cyl,
  data = mtcars,
  chains = 2, refresh = 0
)
p_significance(model)
#> Practical Significance (threshold: 0.60) 
#> 
#> Parameter   |   ps
#> ------------------
#> (Intercept) | 1.00
#> wt          | 1.00
#> cyl         | 0.98
# multiple thresholds - asymmetric, symmetric, default
p_significance(model, threshold = list(c(-10, 5), 0.2, "default"))
#> Practical Significance 
#> 
#> Parameter   |   ps |           ROPE
#> -----------------------------------
#> (Intercept) | 1.00 | [-10.00, 5.00]
#> wt          | 1.00 | [ -0.20, 0.20]
#> cyl         | 0.98 | [ -0.60, 0.60]
# named thresholds
p_significance(model, threshold = list(wt = 0.2, `(Intercept)` = c(-10, 5)))
#> Practical Significance 
#> 
#> Parameter   |   ps |           ROPE
#> -----------------------------------
#> (Intercept) | 1.00 | [-10.00, 5.00]
#> wt          | 1.00 | [ -0.20, 0.20]
#> cyl         | 0.98 | [ -0.60, 0.60]
# }
```
