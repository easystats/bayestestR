# Effective Sample Size (ESS)

Effective Sample Size (ESS) is a measure of how much independent
information there is in autocorrelated chains. It is used to assess the
quality of MCMC samples. A higher ESS indicates more reliable estimates.
For most applications, an effective sample size greater than 1,000 is
sufficient for stable estimates (Bürkner, 2017). This function returns
the effective sample size (ESS) for various Bayesian model objects. For
`brmsfit` objects, the returned ESS corresponds to the bulk-ESS (and the
tail-ESS is also returned).

## Usage

``` r
effective_sample(model, ...)

# S3 method for class 'brmsfit'
effective_sample(
  model,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  ...
)
```

## Arguments

- model:

  A `stanreg`, `stanfit`, `brmsfit`, `blavaan`, or `MCMCglmm` object.

- ...:

  Currently not used.

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

A data frame with two columns: Parameter name and effective sample size
(ESS).

## Details

- **Effective Sample (ESS)** should be as large as possible, altough for
  most applications, an effective sample size greater than 1,000 is
  sufficient for stable estimates (Bürkner, 2017). The ESS corresponds
  to the number of independent samples with the same estimation power as
  the N autocorrelated samples. It is is a measure of “how much
  independent information there is in autocorrelated chains” (*Kruschke
  2015, p182-3*).

- **Bulk-ESS** is useful as a diagnostic for the sampling efficiency in
  the bulk of the posterior. It is defined as the effective sample size
  for rank normalized values using split chains. It can be interpreted
  as the reliability of indices of central tendency (mean, median,
  etc.).

- **Tail-ESS** is useful as a diagnostic for the sampling efficiency in
  the tails of the posterior. It is defined as the minimum of the
  effective sample sizes for 5% and 95% quantiles. It can be interpreted
  as the reliability of indices that depend on the tails of the
  distribution (e.g., credible intervals, tail probabilities, etc.).

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

- Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R,
  JAGS, and Stan. Academic Press.

- Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel
  models using Stan. Journal of Statistical Software, 80(1), 1-28

- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
  (2021). Rank-normalization, folding, and localization: An improved
  R-hat for assessing convergence of MCMC. Bayesian Analysis, 16(2),
  667-718.

## Examples

``` r
# \donttest{
model <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt + gear,
  data = mtcars,
  chains = 2,
  iter = 200,
  refresh = 0
))
effective_sample(model)
#>     Parameter ESS ESS_tail
#> 1 (Intercept) 219       95
#> 2          wt 227      117
#> 3        gear 231      185

model <- suppressWarnings(brms::brm(
  mpg ~ wt,
  data = mtcars,
  chains = 2,
  iter = 200,
  refresh = 0
))
#> Compiling Stan program...
#> Start sampling
effective_sample(model)
#>     Parameter ESS ESS_tail
#> 1 b_Intercept 123       85
#> 2        b_wt 127       80
# }
```
