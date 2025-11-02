# Probability of being in the ROPE

Compute the proportion of the whole posterior distribution that doesn't
lie within a region of practical equivalence (ROPE). It is equivalent to
running `rope(..., ci = 1)`.

## Usage

``` r
p_rope(x, ...)

# S3 method for class 'numeric'
p_rope(x, range = "default", verbose = TRUE, ...)

# S3 method for class 'data.frame'
p_rope(x, range = "default", rvar_col = NULL, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
p_rope(
  x,
  range = "default",
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

  Other arguments passed to
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md).

- range:

  ROPE's lower and higher bounds. Should be `"default"` or depending on
  the number of outcome variables a vector or a list. For models with
  one response, `range` can be:

  - a vector of length two (e.g., `c(-0.1, 0.1)`),

  - a list of numeric vector of the same length as numbers of parameters
    (see 'Examples').

  - a list of *named* numeric vectors, where names correspond to
    parameter names. In this case, all parameters that have no matching
    name in `range` will be set to `"default"`.

  In multivariate models, `range` should be a list with another list
  (one for each response variable) of numeric vectors . Vector names
  should correspond to the name of the response variables. If
  `"default"` and input is a vector, the range is set to `c(-0.1, 0.1)`.
  If `"default"` and input is a Bayesian model,
  [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
  is used. See 'Examples'.

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

p_rope(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#> Proportion of samples inside the ROPE [-0.10, 0.10]: > .999
p_rope(x = mtcars, range = c(-0.1, 0.1))
#> Proportion of samples inside the ROPE [-0.10, 0.10]
#> 
#> Parameter | p (ROPE)
#> --------------------
#> mpg       |   < .001
#> cyl       |   < .001
#> disp      |   < .001
#> hp        |   < .001
#> drat      |   < .001
#> wt        |   < .001
#> qsec      |   < .001
#> vs        |   0.562 
#> am        |   0.594 
#> gear      |   < .001
#> carb      |   < .001
```
