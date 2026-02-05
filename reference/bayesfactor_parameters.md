# Bayes Factors (BF) for a Single Parameter

This method computes Bayes factors against the null (either a point or
an interval), based on prior and posterior samples of a single
parameter. This Bayes factor indicates the degree by which the mass of
the posterior distribution has shifted further away from or closer to
the null value(s) (relative to the prior distribution), thus indicating
if the null value has become less or more likely given the observed
data.\
\
When the null is an interval, the Bayes factor is computed by comparing
the prior and posterior odds of the parameter falling within or outside
the null interval (Morey & Rouder, 2011; Liao et al., 2020); When the
null is a point, a Savage-Dickey density ratio is computed, which is
also an approximation of a Bayes factor comparing the marginal
likelihoods of the model against a model in which the tested parameter
has been restricted to the point null (Wagenmakers et al., 2010; Heck,
2019).\
\
Note that the `logspline` package is used for estimating densities and
probabilities, and must be installed for the function to work.\
\
`bayesfactor_pointnull()` and `bayesfactor_rope()` are wrappers around
`bayesfactor_parameters` with different defaults for the null to be
tested against (a point and a range, respectively). Aliases of the main
functions are prefixed with `bf_*`, like `bf_parameters()` or
`bf_pointnull()`.\
\
**For more info, in particular on specifying correct priors for factors
with more than 2 levels, see [the Bayes factors
vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).**

## Usage

``` r
bayesfactor_parameters(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  ...,
  verbose = TRUE
)

bayesfactor_pointnull(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  ...,
  verbose = TRUE
)

bayesfactor_rope(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = rope_range(posterior, verbose = FALSE),
  ...,
  verbose = TRUE
)

bf_parameters(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  ...,
  verbose = TRUE
)

bf_pointnull(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  ...,
  verbose = TRUE
)

bf_rope(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = rope_range(posterior, verbose = FALSE),
  ...,
  verbose = TRUE
)

# S3 method for class 'numeric'
bayesfactor_parameters(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  ...,
  verbose = TRUE
)

# S3 method for class 'stanreg'
bayesfactor_parameters(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  ...,
  verbose = TRUE
)

# S3 method for class 'data.frame'
bayesfactor_parameters(
  posterior,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  rvar_col = NULL,
  ...,
  verbose = TRUE
)
```

## Arguments

- posterior:

  A numerical vector, `stanreg` / `brmsfit` object, `emmGrid` or a data
  frame - representing a posterior distribution(s) from (see 'Details').

- prior:

  An object representing a prior distribution (see 'Details').

- direction:

  Test type (see 'Details'). One of `0`, `"two-sided"` (default, two
  tailed), `-1`, `"left"` (left tailed) or `1`, `"right"` (right
  tailed).

- null:

  Value of the null, either a scalar (for point-null) or a range (for a
  interval-null).

- ...:

  Arguments passed to and from other methods. (Can be used to pass
  arguments to internal
  [`logspline::logspline()`](https://rdrr.io/pkg/logspline/man/logspline.html).)

- verbose:

  Toggle off warnings.

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

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

## Value

A data frame containing the (log) Bayes factor representing evidence
*against* the null (Use
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) to extract the
non-log Bayes factors; see examples).

## Details

This method is used to compute Bayes factors based on prior and
posterior distributions.

### One-sided & Dividing Tests (setting an order restriction)

One sided tests (controlled by `direction`) are conducted by restricting
the prior and posterior of the non-null values (the "alternative") to
one side of the null only (Morey & Wagenmakers, 2014). For example, if
we have a prior hypothesis that the parameter should be positive, the
alternative will be restricted to the region to the right of the null
(point or interval). For example, for a Bayes factor comparing the
"null" of `0-0.1` to the alternative `>0.1`, we would set
`bayesfactor_parameters(null = c(0, 0.1), direction = ">")`.\
\
It is also possible to compute a Bayes factor for **dividing**
hypotheses - that is, for a null and alternative that are complementary,
opposing one-sided hypotheses (Morey & Wagenmakers, 2014). For example,
for a Bayes factor comparing the "null" of `<0` to the alternative `>0`,
we would set `bayesfactor_parameters(null = c(-Inf, 0))`.

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Setting the correct `prior`

For the computation of Bayes factors, the model priors must be proper
priors (at the very least they should be *not flat*, and it is
preferable that they be *informative*); As the priors for the
alternative get wider, the likelihood of the null value(s) increases, to
the extreme that for completely flat priors the null is infinitely more
favorable than the alternative (this is called *the
Jeffreys-Lindley-Bartlett paradox*). Thus, you should only ever try (or
want) to compute a Bayes factor when you have an informed prior.\
\
(Note that by default,
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) uses
flat priors for fixed-effects; See example below.)\
\
It is important to provide the correct `prior` for meaningful results,
to match the `posterior`-type input:

- **A numeric vector** - `prior` should also be a *numeric vector*,
  representing the prior-estimate.

- **A data frame** - `prior` should also be a *data frame*, representing
  the prior-estimates, in matching column order.

  - If `rvar_col` is specified, `prior` should be *the name of an `rvar`
    column* that represents the prior-estimates.

- **Supported Bayesian model (`stanreg`, `brmsfit`, etc.)**

  - `prior` should be *a model an equivalent model with MCMC samples
    from the priors **only***. See
    [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md).

  - If `prior` is set to `NULL`,
    [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)
    is called internally (not supported for `brmsfit_multiple` model).

- **Output from a `{marginaleffects}` function** - `prior` should also
  be *an equivalent output* from a `{marginaleffects}` function based on
  a prior-model (See
  [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)).

- **Output from an `{emmeans}` function**

  - `prior` should also be *an equivalent output* from an `{emmeans}`
    function based on a prior-model (See
    [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)).

  - `prior` can also be *the original (posterior) model*, in which case
    the function will try to "unupdate" the estimates (not supported if
    the estimates have undergone any transformations – `"log"`,
    `"response"`, etc. – or any `regrid`ing).

## Interpreting Bayes Factors

A Bayes factor greater than 1 can be interpreted as evidence against the
null, at which one convention is that a Bayes factor greater than 3 can
be considered as "substantial" evidence against the null (and vice
versa, a Bayes factor smaller than 1/3 indicates substantial evidence in
favor of the null-model) (Wetzels et al. 2011).

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

- Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., and Grasman, R.
  (2010). Bayesian hypothesis testing for psychologists: A tutorial on
  the Savage-Dickey method. Cognitive psychology, 60(3), 158-189.

- Heck, D. W. (2019). A caveat on the Savage–Dickey density ratio: The
  case of computing Bayes factors for regression parameters. British
  Journal of Mathematical and Statistical Psychology, 72(2), 316-333.

- Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between
  Bayesian order-restricted and point-null hypothesis tests. Statistics
  & Probability Letters, 92, 121-124.

- Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for
  testing interval null hypotheses. Psychological methods, 16(4), 406.

- Liao, J. G., Midya, V., & Berg, A. (2020). Connecting and contrasting
  the Bayes factor and a modified ROPE procedure for testing interval
  null hypotheses. The American Statistician, 1-19.

- Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J.,
  and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental
  Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on
  Psychological Science, 6(3), 291–298.
  [doi:10.1177/1745691611406923](https://doi.org/10.1177/1745691611406923)

## Author

Mattan S. Ben-Shachar

## Examples

``` r
library(bayestestR)
prior <- distribution_normal(1000, mean = 0, sd = 1)
posterior <- distribution_normal(1000, mean = .5, sd = .3)
(BF_pars <- bayesfactor_parameters(posterior, prior, verbose = FALSE))
#> Bayes Factor (Savage-Dickey density ratio)
#> 
#> BF  
#> ----
#> 1.21
#> 
#> * Evidence Against The Null: 0
#> 

as.numeric(BF_pars)
#> [1] 1.212843
# \donttest{
# rstanarm models
# ---------------
contrasts(sleep$group) <- contr.equalprior_pairs # see vingette
stan_model <- suppressWarnings(stan_lmer(
  extra ~ group + (1 | ID),
  data = sleep,
  refresh = 0
))
bayesfactor_parameters(stan_model, verbose = FALSE)
#> Bayes Factor (Savage-Dickey density ratio) 
#> 
#> Parameter   |   BF
#> ------------------
#> (Intercept) | 2.82
#> group1      | 2.96
#> 
#> * Evidence Against The Null: 0
#> 
bayesfactor_parameters(stan_model, null = rope_range(stan_model))
#> Sampling priors, please wait...
#> Bayes Factor (Null-Interval) 
#> 
#> Parameter   |   BF
#> ------------------
#> (Intercept) | 2.68
#> group1      | 2.68
#> 
#> * Evidence Against The Null: [-0.202, 0.202]
#> 

# emmGrid objects
# ---------------
group_diff <- pairs(emmeans(stan_model, ~group, data = sleep))
bayesfactor_parameters(group_diff, prior = stan_model, verbose = FALSE)
#> Bayes Factor (Savage-Dickey density ratio)
#> 
#> contrast        |   BF
#> ----------------------
#> group1 - group2 | 2.65
#> 
#> * Evidence Against The Null: 0
#> 

# Or
# group_diff_prior <- pairs(emmeans(unupdate(stan_model), ~group))
# bayesfactor_parameters(group_diff, prior = group_diff_prior, verbose = FALSE)
# }
# brms models
# -----------
# \dontrun{
contrasts(sleep$group) <- contr.equalprior_pairs # see vingette
my_custom_priors <-
  set_prior("student_t(3, 0, 1)", class = "b") +
  set_prior("student_t(3, 0, 1)", class = "sd", group = "ID")

brms_model <- suppressWarnings(brm(extra ~ group + (1 | ID),
  data = sleep,
  prior = my_custom_priors,
  refresh = 0
))
#> Compiling Stan program...
#> Start sampling
bayesfactor_parameters(brms_model, verbose = FALSE)
#> Bayes Factor (Savage-Dickey density ratio) 
#> 
#> Parameter   |    BF
#> -------------------
#> (Intercept) |  6.58
#> group1      | 11.41
#> 
#> * Evidence Against The Null: 0
#> 
# }
```
