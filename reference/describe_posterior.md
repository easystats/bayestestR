# Describe Posterior Distributions

Compute indices relevant to describe and characterize the posterior
distributions.

## Usage

``` r
describe_posterior(posterior, ...)

# S3 method for class 'numeric'
describe_posterior(
  posterior,
  centrality = "median",
  dispersion = FALSE,
  ci = 0.95,
  ci_method = "eti",
  test = c("p_direction", "rope"),
  rope_range = "default",
  rope_ci = 0.95,
  keep_iterations = FALSE,
  bf_prior = NULL,
  BF = 1,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
describe_posterior(
  posterior,
  centrality = "median",
  dispersion = FALSE,
  ci = 0.95,
  ci_method = "eti",
  test = c("p_direction", "rope"),
  rope_range = "default",
  rope_ci = 0.95,
  keep_iterations = FALSE,
  bf_prior = NULL,
  BF = 1,
  rvar_col = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'stanreg'
describe_posterior(
  posterior,
  centrality = "median",
  dispersion = FALSE,
  ci = 0.95,
  ci_method = "eti",
  test = c("p_direction", "rope"),
  rope_range = "default",
  rope_ci = 0.95,
  keep_iterations = FALSE,
  bf_prior = NULL,
  diagnostic = c("ESS", "Rhat"),
  priors = FALSE,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  BF = 1,
  verbose = TRUE,
  ...
)
```

## Arguments

- posterior:

  A vector, data frame or model of posterior draws. **bayestestR**
  supports a wide range of models (see `methods("describe_posterior")`)
  and not all of those are documented in the 'Usage' section, because
  methods for other classes mostly resemble the arguments of the
  `.numeric` method.

- ...:

  Additional arguments to be passed to or from methods.

- centrality:

  The point-estimates (centrality indices) to compute. Character
  (vector) or list with one or more of these options: `"median"`,
  `"mean"`, `"MAP"` (see
  [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md)),
  `"trimmed"` (which is just `mean(x, trim = threshold)`), `"mode"` or
  `"all"`.

- dispersion:

  Logical, if `TRUE`, computes indices of dispersion related to the
  estimate(s) (`SD` and `MAD` for `mean` and `median`, respectively).
  Dispersion is not available for `"MAP"` or `"mode"` centrality
  indices.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default to `0.95` (`95%`).

- ci_method:

  The type of index used for Credible Interval. Can be `"ETI"` (default,
  see
  [`eti()`](https://easystats.github.io/bayestestR/reference/eti.md)),
  `"HDI"` (see
  [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md)),
  `"BCI"` (see
  [`bci()`](https://easystats.github.io/bayestestR/reference/bci.md)),
  `"SPI"` (see
  [`spi()`](https://easystats.github.io/bayestestR/reference/spi.md)),
  or `"SI"` (see
  [`si()`](https://easystats.github.io/bayestestR/reference/si.md)).

- test:

  The indices of effect existence to compute. Character (vector) or list
  with one or more of these options: `"p_direction"` (or `"pd"`),
  `"rope"`, `"p_map"`, `"p_significance"` (or `"ps"`), `"p_rope"`,
  `"equivalence_test"` (or `"equitest"`), `"bayesfactor"` (or `"bf"`) or
  `"all"` to compute all tests. For each "test", the corresponding
  **bayestestR** function is called (e.g.
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md)
  or
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md))
  and its results included in the summary output.

- rope_range:

  ROPE's lower and higher bounds. Should be a vector of two values
  (e.g., `c(-0.1, 0.1)`), `"default"` or a list of numeric vectors of
  the same length as numbers of parameters. If `"default"`, the bounds
  are set to `x +- 0.1*SD(response)`.

- rope_ci:

  The Credible Interval (CI) probability, corresponding to the
  proportion of HDI, to use for the percentage in ROPE.

- keep_iterations:

  If `TRUE`, will keep all iterations (draws) of bootstrapped or
  Bayesian models. They will be added as additional columns named
  `iter_1, iter_2, ...`. You can reshape them to a long format by
  running
  [`reshape_iterations()`](https://easystats.github.io/bayestestR/reference/reshape_iterations.md).

- bf_prior:

  Distribution representing a prior for the computation of Bayes factors
  / SI. Used if the input is a posterior, otherwise (in the case of
  models) ignored.

- BF:

  The amount of support required to be included in the support interval.

- verbose:

  Toggle off warnings.

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

- diagnostic:

  Diagnostic metrics to compute. Character (vector) or list with one or
  more of these options: `"ESS"`, `"ESS_bulk"`, `"Rhat"`, `"MCSE"` or
  `"all"`. `"ESS"` returns the **tail-ESS** (the minimum of the
  effective sample sizes for the 5% and 95% quantiles), which is the
  most relevant diagnostic for assessing the reliability of credible
  intervals and other tail-based quantities. `"ESS_bulk"` additionally
  returns the **bulk-ESS** (the effective sample size for the bulk of
  the posterior, useful for assessing the reliability of central
  tendency estimates such as the mean or median). `"all"` includes both
  tail and bulk `"ESS"`, `"Rhat"`, and `"MCSE"`.

- priors:

  Add the prior used for each parameter.

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

## Details

One or more components of point estimates (like posterior mean or
median), intervals and tests can be omitted from the summary output by
setting the related argument to `NULL`. For example, `test = NULL` and
`centrality = NULL` would only return the HDI (or CI).

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
[`?insight::find_parameters`](https://easystats.github.io/insight/reference/find_parameters.BGGM.html).

## References

- Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D.
  (2019). *Indices of Effect Existence and Significance in the Bayesian
  Framework*. Frontiers in Psychology 2019;10:2767.
  [doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

- [Region of Practical Equivalence
  (ROPE)](https://easystats.github.io/bayestestR/articles/region_of_practical_equivalence.html)

- [Bayes
  factors](https://easystats.github.io/bayestestR/articles/bayes_factors.html)

## Examples

``` r
library(bayestestR)

x <- rnorm(1000)
describe_posterior(x, verbose = FALSE)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        95% CI |     pd |          ROPE | % in ROPE
#> -----------------------------------------------------------------------
#> Posterior |  -0.02 | [-1.89, 1.98] | 50.70% | [-0.10, 0.10] |     8.21%
describe_posterior(x,
  centrality = "all",
  dispersion = TRUE,
  test = "all",
  verbose = FALSE
)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |  MAD | Mean |   SD |   MAP |        95% CI | p (MAP)
#> -------------------------------------------------------------------------
#> Posterior |  -0.02 | 0.99 | 0.01 | 0.97 | -0.11 | [-1.89, 1.98] |   0.991
#> 
#> Parameter |     pd | p (ROPE) |   ps |          ROPE | % in ROPE
#> ----------------------------------------------------------------
#> Posterior | 50.70% |    0.078 | 0.46 | [-0.10, 0.10] |     8.21%
#> 
#> Parameter | Equivalence (ROPE) |   BF
#> -------------------------------------
#> Posterior |          Undecided | 1.00
describe_posterior(x, ci = c(0.80, 0.90), verbose = FALSE)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        80% CI |        90% CI |     pd |          ROPE | % in ROPE
#> ---------------------------------------------------------------------------------------
#> Posterior |  -0.02 | [-1.20, 1.19] | [-1.53, 1.61] | 50.70% | [-0.10, 0.10] |     8.21%

df <- data.frame(replicate(4, rnorm(100)))
describe_posterior(df, verbose = FALSE)
#> Summary of Posterior Distribution
#> 
#> Parameter |    Median |        95% CI |     pd |          ROPE | % in ROPE
#> --------------------------------------------------------------------------
#> X1        | -7.48e-03 | [-1.50, 2.19] | 50.00% | [-0.10, 0.10] |    11.70%
#> X2        |      0.03 | [-2.09, 1.38] | 50.00% | [-0.10, 0.10] |     6.38%
#> X3        |     -0.06 | [-1.58, 2.10] | 52.00% | [-0.10, 0.10] |     9.57%
#> X4        |     -0.23 | [-1.82, 1.85] | 60.00% | [-0.10, 0.10] |     6.38%
describe_posterior(
  df,
  centrality = "all",
  dispersion = TRUE,
  test = "all",
  verbose = FALSE
)
#> Summary of Posterior Distribution
#> 
#> Parameter |    Median |  MAD |  Mean |   SD |   MAP |        95% CI | p (MAP)
#> -----------------------------------------------------------------------------
#> X1        | -7.48e-03 | 0.89 |  0.04 | 0.97 | -0.34 | [-1.50, 2.19] |   0.939
#> X2        |      0.03 | 1.04 | -0.06 | 0.97 |  0.50 | [-2.09, 1.38] |   0.904
#> X3        |     -0.06 | 1.06 |  0.03 | 1.06 | -0.68 | [-1.58, 2.10] |   0.853
#> X4        |     -0.23 | 0.91 | -0.16 | 0.95 | -0.28 | [-1.82, 1.85] |   0.954
#> 
#> Parameter |     pd | p (ROPE) |   ps |          ROPE | % in ROPE
#> ----------------------------------------------------------------
#> X1        | 50.00% |    0.110 | 0.48 | [-0.10, 0.10] |    11.70%
#> X2        | 50.00% |    0.060 | 0.49 | [-0.10, 0.10] |     6.38%
#> X3        | 52.00% |    0.090 | 0.47 | [-0.10, 0.10] |     9.57%
#> X4        | 60.00% |    0.060 | 0.58 | [-0.10, 0.10] |     6.38%
#> 
#> Parameter | Equivalence (ROPE) |   BF
#> -------------------------------------
#> X1        |          Undecided | 1.00
#> X2        |          Undecided | 1.00
#> X3        |          Undecided | 1.00
#> X4        |          Undecided | 1.00
describe_posterior(df, ci = c(0.80, 0.90), verbose = FALSE)
#> Summary of Posterior Distribution
#> 
#> Parameter |    Median |        80% CI |        90% CI |     pd |          ROPE | % in ROPE
#> ------------------------------------------------------------------------------------------
#> X1        | -7.48e-03 | [-1.04, 1.30] | [-1.15, 1.70] | 50.00% | [-0.10, 0.10] |    11.70%
#> X2        |      0.03 | [-1.32, 1.07] | [-1.75, 1.23] | 50.00% | [-0.10, 0.10] |     6.38%
#> X3        |     -0.06 | [-1.11, 1.44] | [-1.34, 1.73] | 52.00% | [-0.10, 0.10] |     9.57%
#> X4        |     -0.23 | [-1.30, 1.06] | [-1.40, 1.64] | 60.00% | [-0.10, 0.10] |     6.38%

df <- data.frame(replicate(4, rnorm(20)))
head(reshape_iterations(
  describe_posterior(df, keep_iterations = TRUE, verbose = FALSE)
))
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        95% CI |     pd |          ROPE | % in ROPE
#> -----------------------------------------------------------------------
#> X1        |  -0.21 | [-1.70, 1.42] | 60.00% | [-0.10, 0.10] |     5.56%
#> X2        |  -0.21 | [-2.38, 2.41] | 55.00% | [-0.10, 0.10] |    11.11%
#> X3        |   0.22 | [-1.96, 2.61] | 55.00% | [-0.10, 0.10] |     5.56%
#> X4        |  -0.20 | [-1.58, 0.61] | 65.00% | [-0.10, 0.10] |    16.67%
#> X1        |  -0.21 | [-1.70, 1.42] | 60.00% | [-0.10, 0.10] |     5.56%
#> X2        |  -0.21 | [-2.38, 2.41] | 55.00% | [-0.10, 0.10] |    11.11%
#> 
#> Parameter | iter_index | iter_group | iter_value
#> ------------------------------------------------
#> X1        |          1 |          1 |       0.51
#> X2        |          2 |          1 |      -0.36
#> X3        |          3 |          1 |       1.32
#> X4        |          4 |          1 |       0.34
#> X1        |          1 |          2 |      -0.14
#> X2        |          2 |          2 |      -0.50

# \donttest{
# rstanarm models
# -----------------------------------------------
model <- suppressWarnings(
  rstanarm::stan_glm(
    mpg ~ wt + gear,
    data = mtcars, chains = 2, iter = 200,
    refresh = 0
  )
)
describe_posterior(model)
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |         95% CI |     pd |          ROPE | % in ROPE
#> --------------------------------------------------------------------------
#> (Intercept) |  38.94 | [30.09, 47.23] |   100% | [-0.60, 0.60] |        0%
#> wt          |  -5.37 | [-6.57, -4.25] |   100% | [-0.60, 0.60] |        0%
#> gear        |  -0.38 | [-2.03,  1.30] | 63.50% | [-0.60, 0.60] |    44.74%
#> 
#> Parameter   |  Rhat | ESS (tail)
#> --------------------------------
#> (Intercept) | 1.011 |        152
#> wt          | 1.017 |        155
#> gear        | 1.007 |        138
describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#> Warning: Bayes factors might not be precise.
#>   For precise Bayes factors, sampling at least 40,000 posterior samples is
#>   recommended.
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |  MAD |  Mean |   SD |   MAP |         95% CI | p (MAP)
#> -----------------------------------------------------------------------------
#> (Intercept) |  38.94 | 5.21 | 38.92 | 4.87 | 39.81 | [30.09, 47.23] |  < .001
#> wt          |  -5.37 | 0.63 | -5.43 | 0.64 | -5.19 | [-6.57, -4.25] |  < .001
#> gear        |  -0.38 | 0.92 | -0.34 | 0.90 | -0.80 | [-2.03,  1.30] |  0.843 
#> 
#> Parameter   |     pd | p (ROPE) |   ps |          ROPE | % in ROPE
#> ------------------------------------------------------------------
#> (Intercept) |   100% |   < .001 | 1.00 | [-0.60, 0.60] |        0%
#> wt          |   100% |   < .001 | 1.00 | [-0.60, 0.60] |        0%
#> gear        | 63.50% |   0.425  | 0.42 | [-0.60, 0.60] |    44.74%
#> 
#> Parameter   | Equivalence (ROPE) |       BF |  Rhat | ESS (tail)
#> ----------------------------------------------------------------
#> (Intercept) |           Rejected | 1.14e+05 | 1.011 |        152
#> wt          |           Rejected | 1.70e+04 | 1.017 |        155
#> gear        |          Undecided |    0.049 | 1.007 |        138
describe_posterior(model, ci = c(0.80, 0.90))
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |         80% CI |         90% CI |     pd |          ROPE
#> -------------------------------------------------------------------------------
#> (Intercept) |  38.94 | [33.06, 44.88] | [31.18, 46.44] |   100% | [-0.60, 0.60]
#> wt          |  -5.37 | [-6.18, -4.57] | [-6.44, -4.40] |   100% | [-0.60, 0.60]
#> gear        |  -0.38 | [-1.37,  0.86] | [-1.67,  1.20] | 63.50% | [-0.60, 0.60]
#> 
#> Parameter   | % in ROPE |  Rhat | ESS (tail)
#> --------------------------------------------
#> (Intercept) |        0% | 1.011 |        152
#> wt          |        0% | 1.017 |        155
#> gear        |    44.74% | 1.007 |        138
describe_posterior(model, rope_range = list(c(-10, 5), c(-0.2, 0.2), "default"))
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |         95% CI |     pd |           ROPE | % in ROPE
#> ---------------------------------------------------------------------------
#> (Intercept) |  38.94 | [30.09, 47.23] |   100% | [-10.00, 5.00] |        0%
#> wt          |  -5.37 | [-6.57, -4.25] |   100% | [ -0.20, 0.20] |        0%
#> gear        |  -0.38 | [-2.03,  1.30] | 63.50% | [ -0.10, 0.10] |     6.84%
#> 
#> Parameter   |  Rhat | ESS (tail)
#> --------------------------------
#> (Intercept) | 1.011 |        152
#> wt          | 1.017 |        155
#> gear        | 1.007 |        138

# emmeans estimates
# -----------------------------------------------
describe_posterior(emmeans::emtrends(model, ~1, "wt"))
#> Summary of Posterior Distribution
#> 
#> X1      | Median |         95% CI |   pd |          ROPE | % in ROPE
#> --------------------------------------------------------------------
#> overall |  -5.37 | [-6.57, -4.25] | 100% | [-0.10, 0.10] |        0%

# BayesFactor objects
# -----------------------------------------------
bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
describe_posterior(bf)
#> Summary of Posterior Distribution
#> 
#> Parameter  | Median |       95% CI |   pd |          ROPE | % in ROPE
#> ---------------------------------------------------------------------
#> Difference |   0.97 | [0.79, 1.15] | 100% | [-0.09, 0.09] |        0%
#> 
#> Parameter  |       BF |              Prior
#> ------------------------------------------
#> Difference | 1.27e+15 | Cauchy (0 +- 0.71)
describe_posterior(bf, centrality = "all", dispersion = TRUE, test = "all")
#> Summary of Posterior Distribution
#> 
#> Parameter  | Median |  MAD | Mean |   SD |  MAP |       95% CI | p (MAP) |   pd
#> -------------------------------------------------------------------------------
#> Difference |   0.96 | 0.09 | 0.97 | 0.09 | 0.96 | [0.79, 1.16] |  < .001 | 100%
#> 
#> Parameter  | p (ROPE) | ps |          ROPE | % in ROPE | Equivalence (ROPE)
#> ---------------------------------------------------------------------------
#> Difference |   < .001 |  1 | [-0.09, 0.09] |        0% |           Rejected
#> 
#> Parameter  |       BF |              Prior
#> ------------------------------------------
#> Difference | 1.27e+15 | Cauchy (0 +- 0.71)
describe_posterior(bf, ci = c(0.80, 0.90))
#> Summary of Posterior Distribution
#> 
#> Parameter  | Median |       80% CI |       90% CI |   pd |          ROPE
#> ------------------------------------------------------------------------
#> Difference |   0.97 | [0.84, 1.09] | [0.81, 1.12] | 100% | [-0.09, 0.09]
#> 
#> Parameter  | % in ROPE |       BF |              Prior
#> ------------------------------------------------------
#> Difference |        0% | 1.27e+15 | Cauchy (0 +- 0.71)
# }
```
