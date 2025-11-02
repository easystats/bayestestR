# Generate posterior distributions weighted across models

Extract posterior samples of parameters, weighted across models.
Weighting is done by comparing posterior model probabilities, via
[`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md).

## Usage

``` r
weighted_posteriors(..., prior_odds = NULL, missing = 0, verbose = TRUE)

# S3 method for class 'data.frame'
weighted_posteriors(..., prior_odds = NULL, missing = 0, verbose = TRUE)

# S3 method for class 'stanreg'
weighted_posteriors(
  ...,
  prior_odds = NULL,
  missing = 0,
  verbose = TRUE,
  effects = "fixed",
  component = "conditional",
  parameters = NULL
)

# S3 method for class 'BFBayesFactor'
weighted_posteriors(
  ...,
  prior_odds = NULL,
  missing = 0,
  verbose = TRUE,
  iterations = 4000
)
```

## Arguments

- ...:

  Fitted models (see details), all fit on the same data, or a single
  `BFBayesFactor` object.

- prior_odds:

  Optional vector of prior odds for the models compared to the first
  model (or the denominator, for `BFBayesFactor` objects). For
  `data.frame`s, this will be used as the basis of weighting.

- missing:

  An optional numeric value to use if a model does not contain a
  parameter that appears in other models. Defaults to 0.

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

- iterations:

  For `BayesFactor` models, how many posterior samples to draw.

## Value

A data frame with posterior distributions (weighted across models) .

## Details

Note that across models some parameters might play different roles. For
example, the parameter `A` plays a different role in the model
`Y ~ A + B` (where it is a main effect) than it does in the model
`Y ~ A + B + A:B` (where it is a simple effect). In many cases centering
of predictors (mean subtracting for continuous variables, and effects
coding via `contr.sum` or orthonormal coding via
[`contr.equalprior_pairs`](https://easystats.github.io/bayestestR/reference/contr.equalprior.md)
for factors) can reduce this issue. In any case you should be mindful of
this issue.

See
[`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
details for more info on passed models.

Note that for `BayesFactor` models, posterior samples cannot be
generated from intercept only models.

This function is similar in function to
[`brms::posterior_average`](https://paulbuerkner.com/brms/reference/posterior_average.brmsfit.html).

## Note

For `BayesFactor < 0.9.12-4.3`, in some instances there might be some
problems of duplicate columns of random effects in the resulting data
frame.

## References

- Clyde, M., Desimone, H., & Parmigiani, G. (1996). Prediction via
  orthogonalized model mixing. Journal of the American Statistical
  Association, 91(435), 1197-1208.

- Hinne, M., Gronau, Q. F., van den Bergh, D., and Wagenmakers, E.
  (2019, March 25). A conceptual introduction to Bayesian Model
  Averaging.
  [doi:10.31234/osf.io/wgb64](https://doi.org/10.31234/osf.io/wgb64)

- Rouder, J. N., Haaf, J. M., & Vandekerckhove, J. (2018). Bayesian
  inference for psychology, part IV: Parameter estimation and Bayes
  factors. Psychonomic bulletin & review, 25(1), 102-113.

- van den Bergh, D., Haaf, J. M., Ly, A., Rouder, J. N., &
  Wagenmakers, E. J. (2019). A cautionary note on estimating effect
  size.

## See also

[`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
for Bayesian model averaging.

## Examples

``` r
# \donttest{
if (require("rstanarm") && require("see") && interactive()) {
  stan_m0 <- suppressWarnings(stan_glm(extra ~ 1,
    data = sleep,
    family = gaussian(),
    refresh = 0,
    diagnostic_file = file.path(tempdir(), "df0.csv")
  ))

  stan_m1 <- suppressWarnings(stan_glm(extra ~ group,
    data = sleep,
    family = gaussian(),
    refresh = 0,
    diagnostic_file = file.path(tempdir(), "df1.csv")
  ))

  res <- weighted_posteriors(stan_m0, stan_m1, verbose = FALSE)

  plot(eti(res))
}

## With BayesFactor
if (require("BayesFactor")) {
  extra_sleep <- ttestBF(formula = extra ~ group, data = sleep)

  wp <- weighted_posteriors(extra_sleep, verbose = FALSE)

  describe_posterior(extra_sleep, test = NULL, verbose = FALSE)
  # also considers the null
  describe_posterior(wp$delta, test = NULL, verbose = FALSE)
}
#> Summary of Posterior Distribution
#> 
#> Parameter | Median |        95% CI
#> ----------------------------------
#> Posterior |  -0.09 | [-1.37, 0.07]


## weighted prediction distributions via data.frames
if (require("rstanarm") && interactive()) {
  m0 <- suppressWarnings(stan_glm(
    mpg ~ 1,
    data = mtcars,
    family = gaussian(),
    diagnostic_file = file.path(tempdir(), "df0.csv"),
    refresh = 0
  ))

  m1 <- suppressWarnings(stan_glm(
    mpg ~ carb,
    data = mtcars,
    family = gaussian(),
    diagnostic_file = file.path(tempdir(), "df1.csv"),
    refresh = 0
  ))

  # Predictions:
  pred_m0 <- data.frame(posterior_predict(m0))
  pred_m1 <- data.frame(posterior_predict(m1))

  BFmods <- bayesfactor_models(m0, m1, verbose = FALSE)

  wp <- weighted_posteriors(
    pred_m0, pred_m1,
    prior_odds = as.numeric(BFmods)[2],
    verbose = FALSE
  )

  # look at first 5 prediction intervals
  hdi(pred_m0[1:5])
  hdi(pred_m1[1:5])
  hdi(wp[1:5]) # between, but closer to pred_m1
}
# }
```
