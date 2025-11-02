# Bias Corrected and Accelerated Interval (BCa)

Compute the **Bias Corrected and Accelerated Interval (BCa)** of
posterior distributions.

## Usage

``` r
bci(x, ...)

bcai(x, ...)

# S3 method for class 'numeric'
bci(x, ci = 0.95, verbose = TRUE, ...)

# S3 method for class 'data.frame'
bci(x, ci = 0.95, rvar_col = NULL, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
bci(
  x,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'get_predicted'
bci(x, ci = 0.95, use_iterations = FALSE, verbose = TRUE, ...)
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

Unlike equal-tailed intervals (see
[`eti()`](https://easystats.github.io/bayestestR/reference/eti.md)) that
typically exclude `2.5%` from each tail of the distribution and always
include the median, the HDI is *not* equal-tailed and therefore always
includes the mode(s) of posterior distributions. While this can be
useful to better represent the credibility mass of a distribution, the
HDI also has some limitations. See
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

## References

DiCiccio, T. J. and B. Efron. (1996). Bootstrap Confidence Intervals.
Statistical Science. 11(3): 189–212. 10.1214/ss/1032280214

## See also

Other ci:
[`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
[`eti()`](https://easystats.github.io/bayestestR/reference/eti.md),
[`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
[`si()`](https://easystats.github.io/bayestestR/reference/si.md),
[`spi()`](https://easystats.github.io/bayestestR/reference/spi.md)

## Examples

``` r
posterior <- rnorm(1000)
bci(posterior)
#> 95% ETI: [-1.78, 2.10]
bci(posterior, ci = c(0.80, 0.89, 0.95))
#> Equal-Tailed Interval
#> 
#> 80% ETI       |       89% ETI |       95% ETI
#> ---------------------------------------------
#> [-1.17, 1.33] | [-1.52, 1.69] | [-1.78, 2.10]
```
