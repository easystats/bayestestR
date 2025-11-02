# Region of Practical Equivalence (ROPE)

Compute the proportion of the HDI (default to the `89%` HDI) of a
posterior distribution that lies within a region of practical
equivalence.

## Usage

``` r
rope(x, ...)

# S3 method for class 'numeric'
rope(
  x,
  range = "default",
  ci = 0.95,
  ci_method = "ETI",
  complement = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
rope(
  x,
  range = "default",
  ci = 0.95,
  ci_method = "ETI",
  complement = FALSE,
  rvar_col = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'stanreg'
rope(
  x,
  range = "default",
  ci = 0.95,
  ci_method = "ETI",
  complement = FALSE,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'brmsfit'
rope(
  x,
  range = "default",
  ci = 0.95,
  ci_method = "ETI",
  complement = FALSE,
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

- ci:

  The Credible Interval (CI) probability, corresponding to the
  proportion of HDI, to use for the percentage in ROPE.

- ci_method:

  The type of interval to use to quantify the percentage in ROPE. Can be
  'HDI' (default) or 'ETI'. See
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md).

- complement:

  Should the probabilities above/below the ROPE (the *complementary*
  probabilities) be returned as well? See
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)
  as well.

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

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## ROPE

Statistically, the probability of a posterior distribution of being
different from 0 does not make much sense (the probability of a single
value null hypothesis in a continuous distribution is 0). Therefore, the
idea underlining ROPE is to let the user define an area around the null
value enclosing values that are *equivalent to the null* value for
practical purposes (*Kruschke 2010, 2011, 2014*).

Kruschke (2018) suggests that such null value could be set, by default,
to the -0.1 to 0.1 range of a standardized parameter (negligible effect
size according to Cohen, 1988). This could be generalized: For instance,
for linear models, the ROPE could be set as `0 +/- .1 * sd(y)`. This
ROPE range can be automatically computed for models using the
[`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
function.

Kruschke (2010, 2011, 2014) suggests using the proportion of the `95%`
(or `89%`, considered more stable)
[HDI](https://easystats.github.io/bayestestR/reference/hdi.md) that
falls within the ROPE as an index for "null-hypothesis" testing (as
understood under the Bayesian framework, see
[`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)).

## Sensitivity to parameter's scale

It is important to consider the unit (i.e., the scale) of the predictors
when using an index based on the ROPE, as the correct interpretation of
the ROPE as representing a region of practical equivalence to zero is
dependent on the scale of the predictors. Indeed, the percentage in ROPE
depend on the unit of its parameter. In other words, as the ROPE
represents a fixed portion of the response's scale, its proximity with a
coefficient depends on the scale of the coefficient itself.

## Multicollinearity - Non-independent covariates

When parameters show strong correlations, i.e. when covariates are not
independent, the joint parameter distributions may shift towards or away
from the ROPE. Collinearity invalidates ROPE and hypothesis testing
based on univariate marginals, as the probabilities are conditional on
independence. Most problematic are parameters that only have partial
overlap with the ROPE region. In case of collinearity, the (joint)
distributions of these parameters may either get an increased or
decreased ROPE, which means that inferences based on `rope()` are
inappropriate (*Kruschke 2014, 340f*).

`rope()` performs a simple check for pairwise correlations between
parameters, but as there can be collinearity between more than two
variables, a first step to check the assumptions of this hypothesis
testing is to look at different pair plots. An even more sophisticated
check is the projection predictive variable selection (*Piironen and
Vehtari 2017*).

## Strengths and Limitations

**Strengths:** Provides information related to the practical relevance
of the effects.

**Limitations:** A ROPE range needs to be arbitrarily defined. Sensitive
to the scale (the unit) of the predictors. Not sensitive to highly
significant effects.

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

- Cohen, J. (1988). Statistical power analysis for the behavioural
  sciences.

- Kruschke, J. K. (2010). What to believe: Bayesian methods for data
  analysis. Trends in cognitive sciences, 14(7), 293-300.
  [doi:10.1016/j.tics.2010.05.001](https://doi.org/10.1016/j.tics.2010.05.001)
  .

- Kruschke, J. K. (2011). Bayesian assessment of null values via
  parameter estimation and model comparison. Perspectives on
  Psychological Science, 6(3), 299-312.
  [doi:10.1177/1745691611406925](https://doi.org/10.1177/1745691611406925)
  .

- Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with
  R, JAGS, and Stan. Academic Press.
  [doi:10.1177/2515245918771304](https://doi.org/10.1177/2515245918771304)
  .

- Kruschke, J. K. (2018). Rejecting or accepting parameter values in
  Bayesian estimation. Advances in Methods and Practices in
  Psychological Science, 1(2), 270-280.
  [doi:10.1177/2515245918771304](https://doi.org/10.1177/2515245918771304)
  .

- Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of
  Effect Existence and Significance in the Bayesian Framework. Frontiers
  in Psychology 2019;10:2767.
  [doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

- Piironen, J., & Vehtari, A. (2017). Comparison of Bayesian predictive
  methods for model selection. Statistics and Computing, 27(3), 711–735.
  [doi:10.1007/s11222-016-9649-y](https://doi.org/10.1007/s11222-016-9649-y)

## Examples

``` r
library(bayestestR)

rope(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#> # Proportion of samples inside the ROPE [-0.10, 0.10]:
#> 
#> Inside ROPE
#> -----------
#> 100.00 %   
#> 
rope(x = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#> # Proportion of samples inside the ROPE [-0.10, 0.10]:
#> 
#> Inside ROPE
#> -----------
#> 8.32 %     
#> 
rope(x = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
#> # Proportion of samples inside the ROPE [-0.10, 0.10]:
#> 
#> Inside ROPE
#> -----------
#> 0.00 %     
#> 
rope(x = rnorm(1000, 1, 1), ci = c(0.90, 0.95))
#> # Proportions of samples inside the ROPE [-0.10, 0.10]:
#> 
#> ROPE for the 90% HDI:
#> 
#> Inside ROPE
#> -----------
#> 4.89 %     
#> 
#> 
#> ROPE for the 95% HDI:
#> 
#> Inside ROPE
#> -----------
#> 4.63 %     
#> 
#> 
# \donttest{
model <- suppressWarnings(
  rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
)
rope(model)
#> # Proportion of samples inside the ROPE [-0.60, 0.60]:
#> 
#> Parameter   | Inside ROPE
#> -------------------------
#> (Intercept) |      0.00 %
#> wt          |      0.00 %
#> gear        |     52.11 %
#> 
rope(model, ci = c(0.90, 0.95))
#> # Proportions of samples inside the ROPE [-0.60, 0.60]:
#> 
#> ROPE for the 90% HDI:
#> 
#> Parameter   | Inside ROPE
#> -------------------------
#> (Intercept) |      0.00 %
#> wt          |      0.00 %
#> gear        |     55.00 %
#> 
#> 
#> ROPE for the 95% HDI:
#> 
#> Parameter   | Inside ROPE
#> -------------------------
#> (Intercept) |      0.00 %
#> wt          |      0.00 %
#> gear        |     52.11 %
#> 
#> 

# multiple ROPE ranges
rope(model, range = list(c(-10, 5), c(-0.2, 0.2), "default"))
#> # Proportion of samples inside the ROPE [-10.00, 5.00]:
#> 
#> Parameter   | Inside ROPE
#> -------------------------
#> (Intercept) |      0.00 %
#> wt          |      0.00 %
#> gear        |      9.47 %
#> 

# named ROPE ranges
rope(model, range = list(gear = c(-3, 2), wt = c(-0.2, 0.2)))
#> # Proportion of samples inside the ROPE [-0.10, 0.10]:
#> 
#> Parameter   | Inside ROPE
#> -------------------------
#> (Intercept) |      0.00 %
#> wt          |      0.00 %
#> gear        |    100.00 %
#> 

rope(emmeans::emtrends(model, ~1, "wt"), ci = c(0.90, 0.95))
#> # Proportions of samples inside the ROPE [-0.10, 0.10]:
#> 
#> ROPE for the 90% HDI:
#> 
#> X1      | Inside ROPE
#> ---------------------
#> overall |      0.00 %
#> 
#> 
#> ROPE for the 95% HDI:
#> 
#> X1      | Inside ROPE
#> ---------------------
#> overall |      0.00 %
#> 
#> 

model <- brms::brm(mpg ~ wt + cyl, data = mtcars, refresh = 0)
#> Compiling Stan program...
#> Start sampling
rope(model)
#> Possible multicollinearity between b_cyl and b_wt (r = 0.78). This might
#>   lead to inappropriate results. See 'Details' in '?rope'.
#> # Proportion of samples inside the ROPE [-0.60, 0.60]:
#> 
#> Parameter | Inside ROPE
#> -----------------------
#> Intercept |      0.00 %
#> wt        |      0.00 %
#> cyl       |      0.00 %
#> 
rope(model, ci = c(0.90, 0.95))
#> Possible multicollinearity between b_cyl and b_wt (r = 0.78). This might
#>   lead to inappropriate results. See 'Details' in '?rope'.
#> # Proportions of samples inside the ROPE [-0.60, 0.60]:
#> 
#> ROPE for the 90% HDI:
#> 
#> Parameter | Inside ROPE
#> -----------------------
#> Intercept |      0.00 %
#> wt        |      0.00 %
#> cyl       |      0.00 %
#> 
#> 
#> ROPE for the 95% HDI:
#> 
#> Parameter | Inside ROPE
#> -----------------------
#> Intercept |      0.00 %
#> wt        |      0.00 %
#> cyl       |      0.00 %
#> 
#> 

model <- brms::brm(
  brms::bf(brms::mvbind(mpg, disp) ~ wt + cyl) + brms::set_rescor(rescor = TRUE),
  data = mtcars,
  refresh = 0
)
#> Compiling Stan program...
#> Start sampling
rope(model)
#> Possible multicollinearity between b_mpg_cyl and b_mpg_wt (r = 0.79),
#>   b_disp_cyl and b_disp_wt (r = 0.79). This might lead to inappropriate
#>   results. See 'Details' in '?rope'.
#> # Proportion of samples inside the ROPE.
#> ROPE with depends on outcome variable.
#> 
#> Parameter      | Inside ROPE |      ROPE width
#> ----------------------------------------------
#> mpg_Intercept  |      0.00 % |   [-0.60, 0.60]
#> mpg_wt         |      0.00 % |   [-0.60, 0.60]
#> mpg_cyl        |      0.00 % |   [-0.60, 0.60]
#> disp_Intercept |      0.00 % | [-12.39, 12.39]
#> disp_wt        |      0.00 % | [-12.39, 12.39]
#> disp_cyl       |      0.00 % | [-12.39, 12.39]
#> 
rope(model, ci = c(0.90, 0.95))
#> Possible multicollinearity between b_mpg_cyl and b_mpg_wt (r = 0.79),
#>   b_disp_cyl and b_disp_wt (r = 0.79). This might lead to inappropriate
#>   results. See 'Details' in '?rope'.
#> # Proportions of samples inside the ROPE.
#> ROPE with depends on outcome variable.
#> 
#> ROPE for the 90% HDI:
#> 
#> Parameter      | Inside ROPE |      ROPE width
#> ----------------------------------------------
#> mpg_Intercept  |      0.00 % |   [-0.60, 0.60]
#> mpg_wt         |      0.00 % |   [-0.60, 0.60]
#> mpg_cyl        |      0.00 % |   [-0.60, 0.60]
#> disp_Intercept |      0.00 % | [-12.39, 12.39]
#> disp_wt        |      0.00 % | [-12.39, 12.39]
#> disp_cyl       |      0.00 % | [-12.39, 12.39]
#> 
#> 
#> ROPE for the 95% HDI:
#> 
#> Parameter      | Inside ROPE |      ROPE width
#> ----------------------------------------------
#> mpg_Intercept  |      0.00 % |   [-0.60, 0.60]
#> mpg_wt         |      0.00 % |   [-0.60, 0.60]
#> mpg_cyl        |      0.00 % |   [-0.60, 0.60]
#> disp_Intercept |      0.00 % | [-12.39, 12.39]
#> disp_wt        |      0.00 % | [-12.39, 12.39]
#> disp_cyl       |      0.00 % | [-12.39, 12.39]
#> 
#> 

# different ROPE ranges for model parameters. For each response, a named
# list (with the name of the response variable) is required as list-element
# for the `range` argument.
rope(
  model,
  range = list(
    mpg = list(b_mpg_wt = c(-1, 1), b_mpg_cyl = c(-2, 2)),
    disp = list(b_disp_wt = c(-5, 5), b_disp_cyl = c(-4, 4))
  )
)
#> Possible multicollinearity between b_mpg_cyl and b_mpg_wt (r = 0.79),
#>   b_disp_cyl and b_disp_wt (r = 0.79). This might lead to inappropriate
#>   results. See 'Details' in '?rope'.
#> # Proportion of samples inside the ROPE.
#> ROPE with depends on outcome variable.
#> 
#> Parameter      | Inside ROPE |    ROPE width
#> --------------------------------------------
#> mpg_Intercept  |      0.00 % | [-0.10, 0.10]
#> mpg_wt         |      0.00 % | [-1.00, 1.00]
#> mpg_cyl        |     89.82 % | [-2.00, 2.00]
#> disp_Intercept |      0.00 % | [-0.10, 0.10]
#> disp_wt        |      0.00 % | [-5.00, 5.00]
#> disp_cyl       |      0.00 % | [-4.00, 4.00]
#> 

bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
rope(bf)
#> # Proportion of samples inside the ROPE [-0.10, 0.10]:
#> 
#> Parameter  | Inside ROPE
#> ------------------------
#> Difference |      0.00 %
#> 
rope(bf, ci = c(0.90, 0.95))
#> # Proportions of samples inside the ROPE [-0.10, 0.10]:
#> 
#> ROPE for the 90% HDI:
#> 
#> Parameter  | Inside ROPE
#> ------------------------
#> Difference |      0.00 %
#> 
#> 
#> ROPE for the 95% HDI:
#> 
#> Parameter  | Inside ROPE
#> ------------------------
#> Difference |      0.00 %
#> 
#> 
# }
```
