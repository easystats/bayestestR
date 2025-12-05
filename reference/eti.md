# Equal-Tailed Interval (ETI)

Compute the **Equal-Tailed Interval (ETI)** of posterior distributions
using the quantiles method. The probability of being below this interval
is equal to the probability of being above it. The ETI can be used in
the context of uncertainty characterisation of posterior distributions
as **Credible Interval (CI)**.

## Usage

``` r
eti(x, ...)

# S3 method for class 'numeric'
eti(x, ci = 0.95, verbose = TRUE, ...)

# S3 method for class 'data.frame'
eti(x, ci = 0.95, rvar_col = NULL, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
eti(
  x,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'get_predicted'
eti(x, ci = 0.95, use_iterations = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  Vector representing a posterior distribution, or a data frame of such
  vectors. Can also be a Bayesian model. **bayestestR** supports a wide
  range of models (see, for example, `methods("hdi")`) and not all of
  those are documented in the 'Usage' section, because methods for other
  classes mostly resemble the arguments of the `.numeric` or
  `.data.frame`methods.

- ...:

  Currently not used.

- ci:

  Value or vector of probability of the (credible) interval - CI
  (between 0 and 1) to be estimated. Default to `.95` (`95%`).

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

- use_iterations:

  Logical, if `TRUE` and `x` is a `get_predicted` object, (returned by
  [`insight::get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html)),
  the function is applied to the iterations instead of the predictions.
  This only applies to models that return iterations for predicted
  values (e.g., `brmsfit` models).

## Value

A data frame with following columns:

- `Parameter` The model parameter(s), if `x` is a model-object. If `x`
  is a vector, this column is missing.

- `CI` The probability of the credible interval.

- `CI_low`, `CI_high` The lower and upper credible interval limits for
  the parameters.

## Details

Unlike equal-tailed intervals (see `eti()`) that typically exclude
`2.5%` from each tail of the distribution and always include the median,
the HDI is *not* equal-tailed and therefore always includes the mode(s)
of posterior distributions. While this can be useful to better represent
the credibility mass of a distribution, the HDI also has some
limitations. See
[`spi()`](https://easystats.github.io/bayestestR/reference/spi.md) for
details.

The [`95%` or `89%` Credible Intervals
(CI)](https://easystats.github.io/bayestestR/articles/credible_interval.html)
are two reasonable ranges to characterize the uncertainty related to the
estimation (see
[here](https://easystats.github.io/bayestestR/articles/credible_interval.html)
for a discussion about the differences between these two values).

The `89%` intervals (`ci = 0.89`) are deemed to be more stable than, for
instance, `95%` intervals (*Kruschke, 2014*). An effective sample size
of at least 10.000 is recommended if one wants to estimate `95%`
intervals with high precision (*Kruschke, 2014, p. 183ff*).
Unfortunately, the default number of posterior samples for most Bayes
packages (e.g., `rstanarm` or `brms`) is only 4.000 (thus, you might
want to increase it when fitting your model). Moreover, 89 indicates the
arbitrariness of interval limits - its only remarkable property is being
the highest prime number that does not exceed the already unstable `95%`
threshold (*McElreath, 2015*).

However, `95%` has some [advantages
too](https://easystats.github.io/blog/posts/bayestestr_95/). For
instance, it shares (in the case of a normal posterior distribution) an
intuitive relationship with the standard deviation and it conveys a more
accurate image of the (artificial) bounds of the distribution. Also,
because it is wider, it makes analyses more conservative (i.e., the
probability of covering zero is larger for the `95%` CI than for lower
ranges such as `89%`), which is a good thing in the context of the
reproducibility crisis.

A `95%` equal-tailed interval (ETI) has `2.5%` of the distribution on
either side of its limits. It indicates the 2.5th percentile and the
97.5th percentile. In symmetric distributions, the two methods of
computing credible intervals, the ETI and the
[HDI](https://easystats.github.io/bayestestR/reference/hdi.md), return
similar results.

This is not the case for skewed distributions. Indeed, it is possible
that parameter values in the ETI have lower credibility (are less
probable) than parameter values outside the ETI. This property seems
undesirable as a summary of the credible values in a distribution.

On the other hand, the ETI range does change when transformations are
applied to the distribution (for instance, for a log odds scale to
probabilities): the lower and higher bounds of the transformed
distribution will correspond to the transformed lower and higher bounds
of the original distribution. On the contrary, applying transformations
to the distribution will change the resulting HDI.

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

## See also

Other ci:
[`bci()`](https://easystats.github.io/bayestestR/reference/bci.md),
[`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
[`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
[`si()`](https://easystats.github.io/bayestestR/reference/si.md),
[`spi()`](https://easystats.github.io/bayestestR/reference/spi.md)

## Examples

``` r
library(bayestestR)

posterior <- rnorm(1000)
eti(posterior)
#> 95% ETI: [-1.99, 1.84]
eti(posterior, ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> 80% ETI       |       89% ETI |       95% ETI
#> ---------------------------------------------
#> [-1.29, 1.25] | [-1.58, 1.53] | [-1.99, 1.84]

df <- data.frame(replicate(4, rnorm(100)))
eti(df)
#> Equal-Tailed Interval
#> 
#> Parameter |       95% ETI
#> -------------------------
#> X1        | [-1.80, 2.19]
#> X2        | [-1.70, 1.96]
#> X3        | [-1.91, 1.63]
#> X4        | [-1.87, 1.87]
eti(df, ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> Parameter |       80% ETI |       89% ETI |       95% ETI
#> ---------------------------------------------------------
#> X1        | [-1.09, 1.33] | [-1.72, 1.78] | [-1.80, 2.19]
#> X2        | [-0.96, 1.59] | [-1.40, 1.70] | [-1.70, 1.96]
#> X3        | [-1.07, 0.88] | [-1.52, 1.15] | [-1.91, 1.63]
#> X4        | [-1.20, 1.18] | [-1.67, 1.47] | [-1.87, 1.87]
# \donttest{
model <- suppressWarnings(
  rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
)
eti(model)
#> Equal-Tailed Interval
#> 
#> Parameter   |        95% ETI | Effects |   Component
#> ----------------------------------------------------
#> (Intercept) | [27.81, 47.11] |   fixed | conditional
#> wt          | [-6.63, -4.01] |   fixed | conditional
#> gear        | [-2.12,  1.63] |   fixed | conditional
eti(model, ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> Parameter   |        80% ETI |        89% ETI |        95% ETI | Effects |   Component
#> --------------------------------------------------------------------------------------
#> (Intercept) | [32.55, 44.06] | [30.67, 45.94] | [27.81, 47.11] |   fixed | conditional
#> wt          | [-6.20, -4.60] | [-6.38, -4.34] | [-6.63, -4.01] |   fixed | conditional
#> gear        | [-1.35,  0.86] | [-1.61,  1.33] | [-2.12,  1.63] |   fixed | conditional

eti(emmeans::emtrends(model, ~1, "wt", data = mtcars))
#> Equal-Tailed Interval
#> 
#> X1      |        95% ETI
#> ------------------------
#> overall | [-6.63, -4.01]

model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 7e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.07 seconds.
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
#> Chain 1:  Elapsed Time: 0.02 seconds (Warm-up)
#> Chain 1:                0.02 seconds (Sampling)
#> Chain 1:                0.04 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 4e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.04 seconds.
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
#> Chain 2:  Elapsed Time: 0.021 seconds (Warm-up)
#> Chain 2:                0.018 seconds (Sampling)
#> Chain 2:                0.039 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 3e-06 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
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
#> Chain 3:  Elapsed Time: 0.022 seconds (Warm-up)
#> Chain 3:                0.019 seconds (Sampling)
#> Chain 3:                0.041 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 3e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
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
#> Chain 4:  Elapsed Time: 0.022 seconds (Warm-up)
#> Chain 4:                0.019 seconds (Sampling)
#> Chain 4:                0.041 seconds (Total)
#> Chain 4: 
eti(model)
#> Equal-Tailed Interval
#> 
#> Parameter   |        95% ETI | Effects |   Component
#> ----------------------------------------------------
#> b_Intercept | [36.12, 43.25] |   fixed | conditional
#> b_wt        | [-4.78, -1.64] |   fixed | conditional
#> b_cyl       | [-2.38, -0.66] |   fixed | conditional
eti(model, ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> Parameter   |        80% ETI |        89% ETI |        95% ETI | Effects |   Component
#> --------------------------------------------------------------------------------------
#> b_Intercept | [37.42, 41.98] | [36.82, 42.64] | [36.12, 43.25] |   fixed | conditional
#> b_wt        | [-4.19, -2.20] | [-4.45, -1.95] | [-4.78, -1.64] |   fixed | conditional
#> b_cyl       | [-2.06, -0.96] | [-2.19, -0.82] | [-2.38, -0.66] |   fixed | conditional

bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
eti(bf)
#> Equal-Tailed Interval
#> 
#> Parameter  |      95% ETI
#> -------------------------
#> Difference | [0.85, 1.26]
eti(bf, ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> Parameter  |      80% ETI |      89% ETI |      95% ETI
#> -------------------------------------------------------
#> Difference | [0.92, 1.19] | [0.89, 1.22] | [0.85, 1.26]
# }
```
