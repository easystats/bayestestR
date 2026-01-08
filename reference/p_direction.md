# Probability of Direction (pd)

Compute the **Probability of Direction** (***pd***, also known as the
Maximum Probability of Effect - *MPE*). This can be interpreted as the
probability that a parameter (described by its posterior distribution)
is strictly positive or negative (whichever is the most probable).
Although differently expressed, this index is fairly similar (*i.e.*, is
strongly correlated) to the frequentist **p-value** (see details).

## Usage

``` r
p_direction(x, ...)

pd(x, ...)

# S3 method for class 'numeric'
p_direction(
  x,
  method = "direct",
  null = 0,
  as_p = FALSE,
  remove_na = TRUE,
  ...
)

# S3 method for class 'data.frame'
p_direction(
  x,
  method = "direct",
  null = 0,
  as_p = FALSE,
  remove_na = TRUE,
  rvar_col = NULL,
  ...
)

# S3 method for class 'brmsfit'
p_direction(
  x,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  method = "direct",
  null = 0,
  as_p = FALSE,
  remove_na = TRUE,
  ...
)

# S3 method for class 'get_predicted'
p_direction(
  x,
  method = "direct",
  null = 0,
  as_p = FALSE,
  remove_na = TRUE,
  use_iterations = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A vector representing a posterior distribution, a data frame of
  posterior draws (samples be parameter). Can also be a Bayesian model.

- ...:

  Currently not used.

- method:

  Can be `"direct"` or one of methods of
  [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md),
  such as `"kernel"`, `"logspline"` or `"KernSmooth"`. See details.

- null:

  The value considered as a "null" effect. Traditionally 0, but could
  also be 1 in the case of ratios of change (OR, IRR, ...).

- as_p:

  If `TRUE`, the p-direction (pd) values are converted to a frequentist
  p-value using
  [`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md).

- remove_na:

  Should missing values be removed before computation? Note that `Inf`
  (infinity) are *not* removed.

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in `p_direction()`.

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

- verbose:

  Toggle off warnings.

## Value

Values between 0.5 and 1 *or* between 0 and 1 (see above) corresponding
to the probability of direction (pd).

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## What is the *pd*?

The Probability of Direction (pd) is an index of effect existence,
representing the certainty with which an effect goes in a particular
direction (i.e., is positive or negative / has a sign), typically
ranging from 0.5 to 1 (but see next section for cases where it can range
between 0 and 1). Beyond its simplicity of interpretation, understanding
and computation, this index also presents other interesting properties:

- Like other posterior-based indices, *pd* is solely based on the
  posterior distributions and does not require any additional
  information from the data or the model (e.g., such as priors, as in
  the case of Bayes factors).

- It is robust to the scale of both the response variable and the
  predictors.

- It is strongly correlated with the frequentist p-value, and can thus
  be used to draw parallels and give some reference to readers
  non-familiar with Bayesian statistics (Makowski et al., 2019).

## Relationship with the p-value

In most cases, it seems that the *pd* has a direct correspondence with
the frequentist one-sided *p*-value through the formula (for two-sided
*p*): p = 2 \* (1 - p_(d)) Thus, a two-sided p-value of respectively
`.1`, `.05`, `.01` and `.001` would correspond approximately to a *pd*
of `95%`, `97.5%`, `99.5%` and `99.95%`. See
[`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
for details.

## Possible Range of Values

The largest value *pd* can take is 1 - the posterior is strictly
directional. However, the smallest value *pd* can take depends on the
parameter space represented by the posterior.

**For a continuous parameter space**, exact values of 0 (or any point
null value) are not possible, and so 100% of the posterior has *some*
sign, some positive, some negative. Therefore, the smallest the *pd* can
be is 0.5 - with an equal posterior mass of positive and negative
values. Values close to 0.5 *cannot* be used to support the null
hypothesis (that the parameter does *not* have a direction) is a similar
why to how large p-values cannot be used to support the null hypothesis
(see
[`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md);
Makowski et al., 2019).

**For a discrete parameter space or a parameter space that is a mixture
between discrete and continuous spaces**, exact values of 0 (or any
point null value) *are* possible! Therefore, the smallest the *pd* can
be is 0 - with 100% of the posterior mass on 0. Thus values close to 0
can be used to support the null hypothesis (see van den Bergh et al.,
2021).

Examples of posteriors representing discrete parameter space:

- When a parameter can only take discrete values.

- When a mixture prior/posterior is used (such as the spike-and-slab
  prior; see van den Bergh et al., 2021).

- When conducting Bayesian model averaging (e.g.,
  [`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
  or
  [`brms::posterior_average`](https://paulbuerkner.com/brms/reference/posterior_average.brmsfit.html)).

## Methods of computation

The *pd* is defined as: \$\$p_d = max({Pr(\hat{\theta} \<
\theta\_{null}), Pr(\hat{\theta} \> \theta\_{null})})\$\$

The most simple and direct way to compute the *pd* is to compute the
proportion of positive (or larger than `null`) posterior samples, the
proportion of negative (or smaller than `null`) posterior samples, and
take the larger of the two. This "simple" method is the most
straightforward, but its precision is directly tied to the number of
posterior draws.

The second approach relies on [density
estimation](https://easystats.github.io/bayestestR/reference/estimate_density.md):
It starts by estimating the continuous-smooth density function (for
which many methods are available), and then computing the [area under
the
curve](https://easystats.github.io/bayestestR/reference/area_under_curve.md)
(AUC) of the density curve on either side of `null` and taking the
maximum between them. Note the this approach assumes a continuous
density function, and so **when the posterior represents a (partially)
discrete parameter space, only the direct method *must* be used** (see
above).

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

- Makowski, D., Ben-Shachar, M. S., Chen, S. A., & Lüdecke, D. (2019).
  Indices of effect existence and significance in the Bayesian
  framework. Frontiers in psychology, 10, 2767.
  [doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

- van den Bergh, D., Haaf, J. M., Ly, A., Rouder, J. N., &
  Wagenmakers, E. J. (2021). A cautionary note on estimating effect
  size. Advances in Methods and Practices in Psychological Science,
  4(1).
  [doi:10.1177/2515245921992035](https://doi.org/10.1177/2515245921992035)

## See also

[`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
to convert between Probability of Direction (pd) and p-value.

## Examples

``` r
library(bayestestR)

# Simulate a posterior distribution of mean 1 and SD 1
# ----------------------------------------------------
posterior <- rnorm(1000, mean = 1, sd = 1)
p_direction(posterior)
#> Probability of Direction
#> 
#> Parameter |     pd
#> ------------------
#> Posterior | 84.60%
p_direction(posterior, method = "kernel")
#> Probability of Direction
#> 
#> Parameter |     pd
#> ------------------
#> Posterior | 83.27%

# Simulate a dataframe of posterior distributions
# -----------------------------------------------
df <- data.frame(replicate(4, rnorm(100)))
p_direction(df)
#> Probability of Direction
#> 
#> Parameter |     pd
#> ------------------
#> X1        | 51.00%
#> X2        | 52.00%
#> X3        | 52.00%
#> X4        | 56.00%
p_direction(df, method = "kernel")
#> Probability of Direction
#> 
#> Parameter |     pd
#> ------------------
#> X1        | 51.65%
#> X2        | 52.15%
#> X3        | 50.93%
#> X4        | 57.91%

# \donttest{
# rstanarm models
# -----------------------------------------------
model <- rstanarm::stan_glm(mpg ~ wt + cyl,
  data = mtcars,
  chains = 2, refresh = 0
)
p_direction(model)
#> Probability of Direction 
#> 
#> Parameter   |   pd
#> ------------------
#> (Intercept) | 100%
#> wt          | 100%
#> cyl         | 100%
p_direction(model, method = "kernel")
#> Probability of Direction 
#> 
#> Parameter   |      pd
#> ---------------------
#> (Intercept) | 100.00%
#> wt          |  99.99%
#> cyl         |  99.99%

# emmeans
# -----------------------------------------------
p_direction(emmeans::emtrends(model, ~1, "wt", data = mtcars))
#> Probability of Direction
#> 
#> X1      |   pd
#> --------------
#> overall | 100%

# brms models
# -----------------------------------------------
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
#> Chain 1:  Elapsed Time: 0.017 seconds (Warm-up)
#> Chain 1:                0.013 seconds (Sampling)
#> Chain 1:                0.03 seconds (Total)
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
#> Chain 2:  Elapsed Time: 0.016 seconds (Warm-up)
#> Chain 2:                0.013 seconds (Sampling)
#> Chain 2:                0.029 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.016 seconds (Warm-up)
#> Chain 3:                0.014 seconds (Sampling)
#> Chain 3:                0.03 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 4e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.04 seconds.
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
#> Chain 4:  Elapsed Time: 0.017 seconds (Warm-up)
#> Chain 4:                0.015 seconds (Sampling)
#> Chain 4:                0.032 seconds (Total)
#> Chain 4: 
p_direction(model)
#> Probability of Direction 
#> 
#> Parameter   |     pd
#> --------------------
#> (Intercept) |   100%
#> wt          |   100%
#> cyl         | 99.98%
p_direction(model, method = "kernel")
#> Probability of Direction 
#> 
#> Parameter   |      pd
#> ---------------------
#> (Intercept) | 100.00%
#> wt          | 100.00%
#> cyl         |  99.98%

# BayesFactor objects
# -----------------------------------------------
bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
p_direction(bf)
#> Probability of Direction
#> 
#> Parameter  |   pd
#> -----------------
#> Difference | 100%
p_direction(bf, method = "kernel")
#> Probability of Direction
#> 
#> Parameter  |   pd
#> -----------------
#> Difference | 100%
# }
# Using "rvar_col"
x <- data.frame(mu = c(0, 0.5, 1), sigma = c(1, 0.5, 0.25))
x$my_rvar <- posterior::rvar_rng(rnorm, 3, mean = x$mu, sd = x$sigma)
x
#>    mu sigma         my_rvar
#> 1 0.0  1.00 -0.00015 ± 1.00
#> 2 0.5  0.50  0.50430 ± 0.51
#> 3 1.0  0.25  1.00141 ± 0.25
p_direction(x, rvar_col = "my_rvar")
#> Probability of Direction
#> 
#> mu   | sigma |     pd
#> ---------------------
#> 0.00 |  1.00 | 50.10%
#> 0.50 |  0.50 | 84.12%
#> 1.00 |  0.25 |   100%
```
