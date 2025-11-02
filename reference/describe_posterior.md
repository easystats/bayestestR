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
  more of these options: `"ESS"`, `"Rhat"`, `"MCSE"` or `"all"`.

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
[[`?insight::find_parameters`](https://easystats.github.io/insight/reference/find_parameters.html)](https://easystats.github.io/insight/reference/find_parameters.BGGM.html).

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
#> Posterior |  -0.05 | [-1.97, 2.03] | 52.20% | [-0.10, 0.10] |     8.63%
describe_posterior(x,
  centrality = "all",
  dispersion = TRUE,
  test = "all",
  verbose = FALSE
)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |  MAD |  Mean |   SD |   MAP |        95% CI | p (MAP)
#> --------------------------------------------------------------------------
#> Posterior |  -0.05 | 0.98 | -0.02 | 1.03 | -0.13 | [-1.97, 2.03] |   0.982
#> 
#> Parameter |     pd | p (ROPE) |   ps |          ROPE | % in ROPE
#> ----------------------------------------------------------------
#> Posterior | 52.20% |    0.082 | 0.48 | [-0.10, 0.10] |     8.63%
#> 
#> Parameter | Equivalence (ROPE) |   BF
#> -------------------------------------
#> Posterior |          Undecided | 1.00
describe_posterior(x, ci = c(0.80, 0.90), verbose = FALSE)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        80% CI |        90% CI |     pd |          ROPE | % in ROPE
#> ---------------------------------------------------------------------------------------
#> Posterior |  -0.05 | [-1.31, 1.31] | [-1.62, 1.69] | 52.20% | [-0.10, 0.10] |     8.63%

df <- data.frame(replicate(4, rnorm(100)))
describe_posterior(df, verbose = FALSE)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        95% CI |     pd |          ROPE | % in ROPE
#> -----------------------------------------------------------------------
#> X1        |  -0.12 | [-2.21, 1.86] | 56.00% | [-0.10, 0.10] |    10.64%
#> X2        |  -0.28 | [-2.06, 1.69] | 56.00% | [-0.10, 0.10] |     6.38%
#> X3        |  -0.12 | [-1.80, 1.66] | 56.00% | [-0.10, 0.10] |     4.26%
#> X4        |  -0.23 | [-2.19, 1.65] | 59.00% | [-0.10, 0.10] |     2.13%
describe_posterior(
  df,
  centrality = "all",
  dispersion = TRUE,
  test = "all",
  verbose = FALSE
)
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |  MAD |  Mean |   SD |   MAP |        95% CI | p (MAP)
#> --------------------------------------------------------------------------
#> X1        |  -0.12 | 1.20 | -0.18 | 1.10 | -0.11 | [-2.21, 1.86] |   0.997
#> X2        |  -0.28 | 1.19 | -0.10 | 1.07 | -0.64 | [-2.06, 1.69] |   0.796
#> X3        |  -0.12 | 0.89 | -0.03 | 0.99 | -0.32 | [-1.80, 1.66] |   0.919
#> X4        |  -0.23 | 1.09 | -0.21 | 1.06 | -0.40 | [-2.19, 1.65] |   0.958
#> 
#> Parameter |     pd | p (ROPE) |   ps |          ROPE | % in ROPE
#> ----------------------------------------------------------------
#> X1        | 56.00% |    0.100 | 0.50 | [-0.10, 0.10] |    10.64%
#> X2        | 56.00% |    0.060 | 0.53 | [-0.10, 0.10] |     6.38%
#> X3        | 56.00% |    0.040 | 0.53 | [-0.10, 0.10] |     4.26%
#> X4        | 59.00% |    0.020 | 0.57 | [-0.10, 0.10] |     2.13%
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
#> Parameter | Median |        80% CI |        90% CI |     pd |          ROPE | % in ROPE
#> ---------------------------------------------------------------------------------------
#> X1        |  -0.12 | [-1.58, 1.27] | [-1.89, 1.47] | 56.00% | [-0.10, 0.10] |    10.64%
#> X2        |  -0.28 | [-1.32, 1.51] | [-1.84, 1.64] | 56.00% | [-0.10, 0.10] |     6.38%
#> X3        |  -0.12 | [-1.27, 1.18] | [-1.54, 1.57] | 56.00% | [-0.10, 0.10] |     4.26%
#> X4        |  -0.23 | [-1.57, 1.13] | [-1.80, 1.41] | 59.00% | [-0.10, 0.10] |     2.13%

df <- data.frame(replicate(4, rnorm(20)))
head(reshape_iterations(
  describe_posterior(df, keep_iterations = TRUE, verbose = FALSE)
))
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        95% CI |     pd |          ROPE | % in ROPE
#> -----------------------------------------------------------------------
#> X1        |   0.31 | [-1.55, 1.59] | 70.00% | [-0.10, 0.10] |     5.56%
#> X2        |   0.12 | [-1.45, 1.55] | 50.00% | [-0.10, 0.10] |     5.56%
#> X3        |  -0.06 | [-2.77, 1.70] | 60.00% | [-0.10, 0.10] |    16.67%
#> X4        |   0.13 | [-1.67, 1.94] | 60.00% | [-0.10, 0.10] |    11.11%
#> X1        |   0.31 | [-1.55, 1.59] | 70.00% | [-0.10, 0.10] |     5.56%
#> X2        |   0.12 | [-1.45, 1.55] | 50.00% | [-0.10, 0.10] |     5.56%
#> 
#> Parameter | iter_index | iter_group | iter_value
#> ------------------------------------------------
#> X1        |          1 |          1 |       0.86
#> X2        |          2 |          1 |      -0.77
#> X3        |          3 |          1 |      -1.51
#> X4        |          4 |          1 |       1.32
#> X1        |          1 |          2 |       0.16
#> X2        |          2 |          2 |       0.74

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
#> (Intercept) |  39.04 | [29.35, 48.29] |   100% | [-0.60, 0.60] |        0%
#> wt          |  -5.43 | [-6.71, -4.23] |   100% | [-0.60, 0.60] |        0%
#> gear        |  -0.30 | [-1.86,  1.56] | 63.00% | [-0.60, 0.60] |    54.74%
#> 
#> Parameter   |  Rhat | ESS
#> -------------------------
#> (Intercept) | 1.002 | 169
#> wt          | 1.002 | 217
#> gear        | 0.997 | 211
describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#> Warning: Bayes factors might not be precise.
#>   For precise Bayes factors, sampling at least 40,000 posterior samples is
#>   recommended.
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |  MAD |  Mean |   SD |   MAP |         95% CI | p (MAP)
#> -----------------------------------------------------------------------------
#> (Intercept) |  39.04 | 3.98 | 38.71 | 4.71 | 40.40 | [29.35, 48.29] |  < .001
#> wt          |  -5.43 | 0.69 | -5.48 | 0.68 | -5.17 | [-6.71, -4.23] |  < .001
#> gear        |  -0.30 | 0.84 | -0.28 | 0.89 | -0.20 | [-1.86,  1.56] |  0.977 
#> 
#> Parameter   |     pd | p (ROPE) |   ps |          ROPE | % in ROPE
#> ------------------------------------------------------------------
#> (Intercept) |   100% |   < .001 | 1.00 | [-0.60, 0.60] |        0%
#> wt          |   100% |   < .001 | 1.00 | [-0.60, 0.60] |        0%
#> gear        | 63.00% |   0.520  | 0.35 | [-0.60, 0.60] |    54.74%
#> 
#> Parameter   | Equivalence (ROPE) |       BF |  Rhat | ESS
#> ---------------------------------------------------------
#> (Intercept) |           Rejected | 6.33e+03 | 1.002 | 169
#> wt          |           Rejected | 4.88e+04 | 1.002 | 217
#> gear        |          Undecided |    0.044 | 0.997 | 211
describe_posterior(model, ci = c(0.80, 0.90))
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |         80% CI |         90% CI |     pd |          ROPE
#> -------------------------------------------------------------------------------
#> (Intercept) |  39.04 | [33.38, 44.21] | [30.79, 46.08] |   100% | [-0.60, 0.60]
#> wt          |  -5.43 | [-6.36, -4.64] | [-6.66, -4.40] |   100% | [-0.60, 0.60]
#> gear        |  -0.30 | [-1.24,  0.74] | [-1.57,  1.30] | 63.00% | [-0.60, 0.60]
#> 
#> Parameter   | % in ROPE |  Rhat | ESS
#> -------------------------------------
#> (Intercept) |        0% | 1.002 | 169
#> wt          |        0% | 1.002 | 217
#> gear        |    54.74% | 0.997 | 211
describe_posterior(model, rope_range = list(c(-10, 5), c(-0.2, 0.2), "default"))
#> Summary of Posterior Distribution 
#> 
#> Parameter   | Median |         95% CI |     pd |           ROPE | % in ROPE
#> ---------------------------------------------------------------------------
#> (Intercept) |  39.04 | [29.35, 48.29] |   100% | [-10.00, 5.00] |        0%
#> wt          |  -5.43 | [-6.71, -4.23] |   100% | [ -0.20, 0.20] |        0%
#> gear        |  -0.30 | [-1.86,  1.56] | 63.00% | [ -0.10, 0.10] |     8.42%
#> 
#> Parameter   |  Rhat | ESS
#> -------------------------
#> (Intercept) | 1.002 | 169
#> wt          | 1.002 | 217
#> gear        | 0.997 | 211

# emmeans estimates
# -----------------------------------------------
describe_posterior(emmeans::emtrends(model, ~1, "wt"))
#> Summary of Posterior Distribution
#> 
#> X1      | Median |         95% CI |   pd |          ROPE | % in ROPE
#> --------------------------------------------------------------------
#> overall |  -5.43 | [-6.71, -4.23] | 100% | [-0.10, 0.10] |        0%

# BayesFactor objects
# -----------------------------------------------
bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
describe_posterior(bf)
#> Summary of Posterior Distribution
#> 
#> Parameter  | Median |       95% CI |   pd |          ROPE | % in ROPE
#> ---------------------------------------------------------------------
#> Difference |   1.06 | [0.86, 1.27] | 100% | [-0.10, 0.10] |        0%
#> 
#> Parameter  |       BF |              Prior
#> ------------------------------------------
#> Difference | 1.73e+15 | Cauchy (0 +- 0.71)
describe_posterior(bf, centrality = "all", dispersion = TRUE, test = "all")
#> Summary of Posterior Distribution
#> 
#> Parameter  | Median |  MAD | Mean |   SD |  MAP |       95% CI | p (MAP) |   pd
#> -------------------------------------------------------------------------------
#> Difference |   1.06 | 0.10 | 1.06 | 0.10 | 1.05 | [0.87, 1.27] |  < .001 | 100%
#> 
#> Parameter  | p (ROPE) | ps |          ROPE | % in ROPE | Equivalence (ROPE)
#> ---------------------------------------------------------------------------
#> Difference |   < .001 |  1 | [-0.10, 0.10] |        0% |           Rejected
#> 
#> Parameter  |       BF |              Prior
#> ------------------------------------------
#> Difference | 1.73e+15 | Cauchy (0 +- 0.71)
describe_posterior(bf, ci = c(0.80, 0.90))
#> Summary of Posterior Distribution
#> 
#> Parameter  | Median |       80% CI |       90% CI |   pd |          ROPE
#> ------------------------------------------------------------------------
#> Difference |   1.06 | [0.93, 1.19] | [0.89, 1.23] | 100% | [-0.10, 0.10]
#> 
#> Parameter  | % in ROPE |       BF |              Prior
#> ------------------------------------------------------
#> Difference |        0% | 1.73e+15 | Cauchy (0 +- 0.71)
# }
```
