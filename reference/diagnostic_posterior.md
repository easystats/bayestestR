# Posteriors Sampling Diagnostic

Extract diagnostic metrics (Effective Sample Size (`ESS`), `Rhat` and
Monte Carlo Standard Error `MCSE`).

## Usage

``` r
diagnostic_posterior(posterior, ...)

# Default S3 method
diagnostic_posterior(posterior, diagnostic = "all", ...)

# S3 method for class 'stanreg'
diagnostic_posterior(
  posterior,
  diagnostic = "all",
  effects = "fixed",
  component = "location",
  parameters = NULL,
  ...
)
```

## Arguments

- posterior:

  A `stanreg`, `stanfit`, `brmsfit`, or `blavaan` object; a list of data
  frames or matrices representing MCMC chains (rows as samples, columns
  as parameters); or a 3D array (dimensions: samples, chains,
  parameters)

- ...:

  Currently only used for models of class `brmsfit`, where a `variable`
  argument can be used, which is directly passed to the
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  (i.e., `as.data.frame(x, variable = variable)`).

- diagnostic:

  Diagnostic metrics to compute. Character (vector) or list with one or
  more of these options: `"ESS"`, `"Rhat"`, `"MCSE"` or `"all"`.

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
  term, the instrumental variables or marginal effects be returned?
  Applies to models with zero-inflated and/or dispersion formula, or to
  models with instrumental variables (so called fixed-effects
  regressions), or models with marginal effects (from **mfx**). See
  details in section *Model Components* .May be abbreviated. Note that
  the *conditional* component also refers to the *count* or *mean*
  component - names may differ, depending on the modeling package. There
  are three convenient shortcuts (not applicable to *all* model
  classes):

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
  be returned.

## Details

**Effective Sample (ESS)** should be as large as possible, although for
most applications, an effective sample size greater than 1000 is
sufficient for stable estimates (*Bürkner, 2017*). The ESS corresponds
to the number of independent samples with the same estimation power as
the N autocorrelated samples. It is is a measure of "how much
independent information there is in autocorrelated chains" (*Kruschke
2015, p182-3*).

**Rhat** should be the closest to 1. It should not be larger than 1.1
(*Gelman and Rubin, 1992*) or 1.01 (*Vehtari et al., 2019*). The split
Rhat statistic quantifies the consistency of an ensemble of Markov
chains.

**Monte Carlo Standard Error (MCSE)** is another measure of accuracy of
the chains. It is defined as standard deviation of the chains divided by
their effective sample size (the formula for
[`mcse()`](https://easystats.github.io/bayestestR/reference/mcse.md) is
from Kruschke 2015, p. 187). The MCSE "provides a quantitative
suggestion of how big the estimation noise is".

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

- Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation
  using multiple sequences. Statistical science, 7(4), 457-472.

- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., and
  Bürkner, P. C. (2019). Rank-normalization, folding, and localization:
  An improved Rhat for assessing convergence of MCMC. arXiv preprint
  arXiv:1903.08008.

- Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R,
  JAGS, and Stan. Academic Press.

## Examples

``` r
# \donttest{
# rstanarm models
# -----------------------------------------------
model <- suppressWarnings(
  rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
)
diagnostic_posterior(model)
#>     Parameter     Rhat      ESS       MCSE
#> 1 (Intercept) 1.026862 148.0908 0.46193224
#> 2        gear 1.026872 167.9762 0.07793465
#> 3          wt 1.007318 165.5256 0.05915750

# brms models
# -----------------------------------------------
model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 5e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.05 seconds.
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
#> Chain 1:  Elapsed Time: 0.019 seconds (Warm-up)
#> Chain 1:                0.019 seconds (Sampling)
#> Chain 1:                0.038 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 3e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
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
#> Chain 2:  Elapsed Time: 0.019 seconds (Warm-up)
#> Chain 2:                0.018 seconds (Sampling)
#> Chain 2:                0.037 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.019 seconds (Warm-up)
#> Chain 3:                0.019 seconds (Sampling)
#> Chain 3:                0.038 seconds (Total)
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
#> Chain 4:  Elapsed Time: 0.018 seconds (Warm-up)
#> Chain 4:                0.017 seconds (Sampling)
#> Chain 4:                0.035 seconds (Total)
#> Chain 4: 
diagnostic_posterior(model)
#>     Parameter      Rhat      ESS       MCSE
#> 1 b_Intercept 0.9992935 5206.918 0.02449703
#> 2       b_cyl 1.0035762 1876.678 0.01017518
#> 3        b_wt 1.0033650 1924.503 0.01830200
# }
set.seed(101)
mkdata <- function(nrow = 1000, ncol = 2, parnm = LETTERS[1:ncol]) {
  x <- as.data.frame(replicate(ncol, rnorm(nrow)))
  names(x) <- parnm
  x
}
dd <- replicate(5, mkdata(), simplify = FALSE)
diagnostic_posterior(dd)
#>   Parameter      Rhat  ESS       MCSE
#> 1         A 1.0012187 2584 0.02455354
#> 2         B 0.9997185 2564 0.01998160
```
