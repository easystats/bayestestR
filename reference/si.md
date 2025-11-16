# Compute Support Intervals

A support interval contains only the values of the parameter that
predict the observed data better than average, by some degree *k*; these
are values of the parameter that are associated with an updating factor
greater or equal than *k*. From the perspective of the Savage-Dickey
Bayes factor, testing against a point null hypothesis for any value
within the support interval will yield a Bayes factor smaller than
*1/k*.

## Usage

``` r
si(posterior, ...)

# S3 method for class 'numeric'
si(posterior, prior = NULL, BF = 1, verbose = TRUE, ...)

# S3 method for class 'stanreg'
si(
  posterior,
  prior = NULL,
  BF = 1,
  verbose = TRUE,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  ...
)

# S3 method for class 'get_predicted'
si(
  posterior,
  prior = NULL,
  BF = 1,
  use_iterations = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
si(posterior, prior = NULL, BF = 1, rvar_col = NULL, verbose = TRUE, ...)
```

## Arguments

- posterior:

  A numerical vector, `stanreg` / `brmsfit` object, `emmGrid` or a data
  frame - representing a posterior distribution(s) from (see 'Details').

- ...:

  Arguments passed to and from other methods. (Can be used to pass
  arguments to internal
  [`logspline::logspline()`](https://rdrr.io/pkg/logspline/man/logspline.html).)

- prior:

  An object representing a prior distribution (see 'Details').

- BF:

  The amount of support required to be included in the support interval.

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

- use_iterations:

  Logical, if `TRUE` and `x` is a `get_predicted` object, (returned by
  [`insight::get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html)),
  the function is applied to the iterations instead of the predictions.
  This only applies to models that return iterations for predicted
  values (e.g., `brmsfit` models).

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

## Value

A data frame containing the lower and upper bounds of the SI.

Note that if the level of requested support is higher than observed in
the data, the interval will be `[NA,NA]`.

## Details

**For more info, in particular on specifying correct priors for factors
with more than 2 levels, see [the Bayes factors
vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).**

This method is used to compute support intervals based on prior and
posterior distributions. For the computation of support intervals, the
model priors must be proper priors (at the very least they should be
*not flat*, and it is preferable that they be *informative* - note that
by default,
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) uses
flat priors for fixed-effects; see example below).

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Choosing a value of `BF`

The choice of `BF` (the level of support) depends on what we want our
interval to represent:

- A `BF` = 1 contains values whose credibility is not decreased by
  observing the data.

- A `BF` \> 1 contains values who received more impressive support from
  the data.

- A `BF` \< 1 contains values whose credibility has *not* been
  impressively decreased by observing the data. Testing against values
  outside this interval will produce a Bayes factor larger than 1/`BF`
  in support of the alternative. E.g., if an SI (BF = 1/3) excludes 0,
  the Bayes factor against the point-null will be larger than 3.

## Setting the correct `prior`

For the computation of Bayes factors, the model priors must be proper
priors (at the very least they should be *not flat*, and it is
preferable that they be *informative*); As the priors for the
alternative get wider, the likelihood of the null value(s) increases, to
the extreme that for completely flat priors the null is infinitely more
favorable than the alternative (this is called *the
Jeffreys-Lindley-Bartlett paradox*). Thus, you should only ever try (or
want) to compute a Bayes factor when you have an informed prior.  
  
(Note that by default,
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) uses
flat priors for fixed-effects; See example below.)  
  
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

## References

Wagenmakers, E., Gronau, Q. F., Dablander, F., & Etz, A. (2018, November
22). The Support Interval.
[doi:10.31234/osf.io/zwnxb](https://doi.org/10.31234/osf.io/zwnxb)

## See also

Other ci:
[`bci()`](https://easystats.github.io/bayestestR/reference/bci.md),
[`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
[`eti()`](https://easystats.github.io/bayestestR/reference/eti.md),
[`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
[`spi()`](https://easystats.github.io/bayestestR/reference/spi.md)

## Examples

``` r
library(bayestestR)

prior <- distribution_normal(1000, mean = 0, sd = 1)
posterior <- distribution_normal(1000, mean = 0.5, sd = 0.3)

si(posterior, prior, verbose = FALSE)
#> BF = 1 SI: [0.04, 1.04]
# \donttest{
# rstanarm models
# ---------------
library(rstanarm)
contrasts(sleep$group) <- contr.equalprior_pairs # see vignette
stan_model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.9e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.29 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.191 seconds (Warm-up)
#> Chain 1:                0.207 seconds (Sampling)
#> Chain 1:                0.398 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.5e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.15 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.183 seconds (Warm-up)
#> Chain 2:                0.163 seconds (Sampling)
#> Chain 2:                0.346 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.6e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.16 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.17 seconds (Warm-up)
#> Chain 3:                0.165 seconds (Sampling)
#> Chain 3:                0.335 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.5e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.15 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.188 seconds (Warm-up)
#> Chain 4:                0.13 seconds (Sampling)
#> Chain 4:                0.318 seconds (Total)
#> Chain 4: 
si(stan_model, verbose = FALSE)
#> Support Interval
#> 
#> Parameter   |    BF = 1 SI | Effects |   Component
#> --------------------------------------------------
#> (Intercept) | [0.39, 2.64] |   fixed | conditional
#> group1      | [0.39, 2.73] |   fixed | conditional
si(stan_model, BF = 3, verbose = FALSE)
#> Support Interval
#> 
#> Parameter   |    BF = 3 SI | Effects |   Component
#> --------------------------------------------------
#> (Intercept) | [0.73, 2.32] |   fixed | conditional
#> group1      | [0.68, 2.44] |   fixed | conditional

# emmGrid objects
# ---------------
library(emmeans)
group_diff <- pairs(emmeans(stan_model, ~group))
si(group_diff, prior = stan_model, verbose = FALSE)
#> Support Interval
#> 
#> contrast        |      BF = 1 SI
#> --------------------------------
#> group1 - group2 | [-2.75, -0.37]

# brms models
# -----------
library(brms)
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
si(brms_model, verbose = FALSE)
#> Support Interval
#> 
#> Parameter   |    BF = 1 SI | Effects |   Component
#> --------------------------------------------------
#> b_Intercept | [0.65, 2.47] |   fixed | conditional
#> b_group1    | [0.70, 2.43] |   fixed | conditional
# }
```
