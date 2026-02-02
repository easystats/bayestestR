# Changelog

## bayestestR 0.17.0.xxx

CRAN release: 2025-08-29

### New functionality

- [`as.matrix()`](https://rdrr.io/r/base/matrix.html) for
  [`bayesfactor_restricted()`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md),
  to obtain a matrix of Bayes factors between all restricted models.

### Changes

- `as.matrix(<bf>)` now returns class `bayesfactor_matrix` and has a
  simpler printing.

- [`diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.md)
  works with ‘raw’ MCMC samples (i.e., lists of data frames or matrices
  representing samples of parameters from chains, or 3D arrays) as well
  as objects from rstanarm/brms/lavaan models.

## bayestestR 0.17.0

CRAN release: 2025-08-29

### Changes

- [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md)
  (and by extension
  [`p_rope()`](https://easystats.github.io/bayestestR/reference/p_rope.md))
  gain a new `complement` argument such that
  `rope(x, complement = TRUE)` returns the ROPE posterior probability
  together with the posterior probabilities above/below the ROPE (the
  *complementary* probabilities).

- Added
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods for *bayestestR* objects. The
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods also get a new `format` option, `format = "tt"`, to produce
  tables with the `tinytable` package.

- The long deprecated `rnorm_perfect()` function has been removed. Use
  [`distribution_normal()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  instead.

- Prepare for upcoming changes in *marginaleffects* (0.29.0).

## bayestestR 0.16.1

CRAN release: 2025-07-01

### Changes

- Improved efficiency for
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md).

- Minor improvements for models with multinomial response variables.

- Minor improvements for mixture models from package *brms*.

## bayestestR 0.16.0

CRAN release: 2025-05-20

### Changes

- Revised code-base to address changes in latest *insight* update.
  Dealing with larger models (many parameters, many posterior samples)
  from packages *brms* and *rstanarm* is more efficient now.
  Furthermore, the options for the `effects` argument have a new
  behavior. `"all"` only returns fixed effects and random effects
  variance components, but no longer the group level estimates. Use
  `effects = "full"` to return all parameters. This change is mainly to
  be more flexible and gain more efficiency for models with many
  parameters and / or many posterior draws.

## bayestestR 0.15.3

CRAN release: 2025-04-28

### Changes

- [`effective_sample()`](https://easystats.github.io/bayestestR/reference/effective_sample.md),
  and functions that call
  [`effective_sample()`](https://easystats.github.io/bayestestR/reference/effective_sample.md)
  (like
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  with the respective `test` option) now also return the tail ESS.

### Bug fixes

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  now returns a columns with response levels for *marginaleffects*
  objects applied to categorical or multinomial Stan models.

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  now returns a columns with response variables for *marginaleffects*
  objects applied to multivariate response Stan models.

- Fixed issue in
  [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md)
  and `point_estimate(centrality = "MAP")` for vectors with only one
  unique value.

## bayestestR 0.15.2

CRAN release: 2025-02-07

### Changes

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  no longer re-samples a model when computing indices.

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  calls tests only when needed. Before, there was a minimal overhead by
  calling tests that were not requested.

### Bug fixes

- Fixed failing test for Mac OS.

## bayestestR 0.15.1

CRAN release: 2025-01-17

### Changes

- Several minor changes to deal with recent changes in other packages.

### Bug fixes

- Fix to `emmeans` / `marginaleffects` / `data.frame(<rvar>)` methods
  when using multiple credible levels
  ([\#688](https://github.com/easystats/bayestestR/issues/688)).

## bayestestR 0.15.0

CRAN release: 2024-10-17

### Changes

- Support for
  [`posterior::rvar`](https://mc-stan.org/posterior/reference/rvar.html)-type
  column in data frames. For example, a data frame `df` with an `rvar`
  column `".pred"` can now be called directly via
  `p_direction(df, rvar_col = ".pred")`.

- Added support for [marginaleffects](https://marginaleffects.com/)

- The ROPE or threshold ranges in
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md),
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md),
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  and
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)
  can now be specified as a list. This allows for different ranges for
  different parameters.

- Results from objects generated by
  [emmeans](https://rvlenth.github.io/emmeans/) (`emmGrid`/`emm_list`)
  now return results with appended grid-data.

- Usability improvements for
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md):

  - Results from
    [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
    can directly be used in
    [`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md).

  - [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
    gets an `as_p` argument, to directly convert pd-values into
    frequentist p-values.

  - [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
    gets a `remove_na` argument, which defaults to `TRUE`, to remove
    `NA` values from the input before calculating the pd-values.

  - Besides the existing
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html) method,
    [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
    now also has an [`as.vector()`](https://rdrr.io/r/base/vector.html)
    method.

- [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  now accepts non-symmetric ranges for the `threshold` argument.

- [`p_to_pd()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  now also works with data frames returned by
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).
  If a data frame contains a `pd`, `p_direction` or `PD` column name,
  this is assumed to be the pd-values, which are then converted to
  p-values.

- [`p_to_pd()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  for data frame inputs gets a
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) and
  [`as.vector()`](https://rdrr.io/r/base/vector.html) method.

### Bug fixes

- Fixed warning in CRAN check results.

## bayestestR 0.14.0

CRAN release: 2024-07-24

### Breaking Changes

- Arguments named `group`, `at`, `group_by` and `split_by` will be
  deprecated in future releases of *easystats* packages. Please use `by`
  instead. This affects following functions in *bayestestR*:
  [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md).

### Changes

- [`bayesian_as_frequentist()`](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.md)
  now supports more model families from Bayesian models that can be
  successfully converted to their frequentists counterparts.

- [`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  now throws an informative error when Bayes factors for comparisons
  could not be calculated.

### Bug fixes

- Fixed issue in
  [`bayesian_as_frequentist()`](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.md)
  for *brms* models with `0 + Intercept` specification in the model
  formula.

## bayestestR 0.13.2

CRAN release: 2024-02-12

### Breaking Changes

- [`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  now returns 1 and a warning for values smaller than 0.5.

- [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md),
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md),
  [`p_map()`](https://easystats.github.io/bayestestR/reference/p_map.md),
  and
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  now return a data-frame when the input is a numeric vector. (making
  the output consistently a data frame for all inputs.)

- Argument `posteriors` was renamed into `posterior`. Before, there were
  a mix of both spellings, now it is consistently `posterior`.

### Changes

- Retrieving models from the environment was improved.

### Bug fixes

- Fixed issues in various
  [`format()`](https://rdrr.io/r/base/format.html) methods, which did
  not work properly for some few functions (like
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)).

- Fixed issue in
  [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md)
  for double vectors that also had other class attributes.

- Fixed several minor issues and tests.

## bayestestR 0.13.1

CRAN release: 2023-04-07

### Changes

- Improved speed performance when functions are called using
  [`do.call()`](https://rdrr.io/r/base/do.call.html).

- Improved speed performance to
  [`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  for `brmsfit` objects that already included a `marglik` element in the
  model object.

### New functionality

- [`as.logical()`](https://rdrr.io/r/base/logical.html) for
  [`bayesfactor_restricted()`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md)
  results, extracts the boolean vector(s) the mark which draws are part
  of the order restriction.

### Bug fixes

- [`p_map()`](https://easystats.github.io/bayestestR/reference/p_map.md)
  gains a new `null` argument to specify any non-0 nulls.

- Fixed non-working examples for `ci(method = "SI")`.

- Fixed wrong calculation of rope range for model objects in
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md).

- Some smaller bug fixes.

## bayestestR 0.13.0

CRAN release: 2022-09-18

### Breaking

- The minimum needed R version has been bumped to `3.6`.

- `contr.equalprior(contrasts = FALSE)` (previously `contr.orthonorm`)
  no longer returns an identity matrix, but a shifted `diag(n) - 1/n`,
  for consistency.

### New functionality

- [`p_to_bf()`](https://easystats.github.io/bayestestR/reference/p_to_bf.md),
  to convert p-values into Bayes factors. For more accurate approximate
  Bayes factors, use
  [`bic_to_bf()`](https://easystats.github.io/bayestestR/reference/bic_to_bf.md).
- *bayestestR* now supports objects of class `rvar` from package
  *posterior*.
- `contr.equalprior` (previously `contr.orthonorm`) gains two new
  functions: `contr.equalprior_pairs` and `contr.equalprior_deviations`
  to aide in setting more intuitive priors.

### Changes

- has been renamed *`contr.equalprior`* to be more explicit about its
  function.
- [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
  now accepts objects of class `parameters_model()` (from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)),
  to compute probability of direction for parameters of frequentist
  models.

## bayestestR 0.12.1

CRAN release: 2022-05-02

### Breaking

- `Bayesfactor_models()` for frequentist models now relies on the
  updated
  [`insight::get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.html).
  This might change some results for REML based models. See
  documentation.

- [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md)
  argument `group_by` is renamed `at`.

- All `distribution_*(random = FALSE)` functions now rely on
  [`ppoints()`](https://rdrr.io/r/stats/ppoints.html), which will result
  in slightly different results, especially with small `n`s.

- Uncertainty estimation now defaults to `"eti"` (formerly was `"hdi"`).

### Changes

- *bayestestR* functions now support `draws` objects from package
  *posterior*.

- [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
  now handles log(normal)-families and models with log-transformed
  outcomes.

- New function
  [`spi()`](https://easystats.github.io/bayestestR/reference/spi.md), to
  compute shortest probability intervals. Furthermore, the `"spi"`
  option was added as new method to compute uncertainty intervals.

### Bug fixes

- [`bci()`](https://easystats.github.io/bayestestR/reference/bci.md) for
  some objects incorrectly returned the equal-tailed intervals.

## bayestestR 0.11.5

CRAN release: 2021-10-30

- Fixes failing tests in CRAN checks.

## bayestestR 0.11.1

### New functions

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  gains a [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method, which is a short cut for
  `plot(estimate_density(describe_posterior()))`.

## bayestestR 0.11

### Bug fixes

- Fixed issues related to last *brms* update.

- Fixed bug in `describe_posterior.BFBayesFactor()` where Bayes factors
  were missing from out put (
  [\#442](https://github.com/easystats/bayestestR/issues/442) ).

## bayestestR 0.10.0

CRAN release: 2021-05-31

### Breaking

- All Bayes factors are now returned as `log(BF)` (column name
  `log_BF`). Printing is unaffected. To retrieve the raw BFs, you can
  run `exp(result$log_BF)`.

### New functions

- [`bci()`](https://easystats.github.io/bayestestR/reference/bci.md)
  (and its alias
  [`bcai()`](https://easystats.github.io/bayestestR/reference/bci.md))
  to compute bias-corrected and accelerated bootstrap intervals. Along
  with this new function,
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md) and
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  gain a new `ci_method` type, `"bci"`.

### Changes

- `contr.bayes` has been renamed *`contr.orthonorm`* to be more explicit
  about its function.

## bayestestR 0.9.0

CRAN release: 2021-04-08

### Breaking

- The default `ci` width has been changed to 0.95 instead of 0.89 (see
  [here](https://github.com/easystats/bayestestR/discussions/250)). This
  should not come as a surprise to the long-time users of `bayestestR`
  as we have been warning about this impending change for a while now :)

- Column names for
  [`bayesfactor_restricted()`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md)
  are now `p_prior` and `p_posterior` (was `Prior_prob` and
  `Posterior_prob`), to be consistent with
  [`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
  output.

- Removed the experimental function `mhdior`.

### General

- Support for `blavaan` models.

- Support for `blrm` models (*rmsb*).

- Support for `BGGM` models (*BGGM*).

- [`check_prior()`](https://easystats.github.io/bayestestR/reference/check_prior.md)
  and
  [`describe_prior()`](https://easystats.github.io/bayestestR/reference/describe_prior.md)
  should now also work for more ways of prior definition in models from
  *rstanarm* or *brms*.

### Bug fixes

- Fixed bug in [`print()`](https://rdrr.io/r/base/print.html) method for
  the
  [`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)
  function.

- Fixed remaining inconsistencies with CI values, which were not
  reported as fraction for
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md).

- Fixed issues with special prior definitions in
  [`check_prior()`](https://easystats.github.io/bayestestR/reference/check_prior.md),
  [`describe_prior()`](https://easystats.github.io/bayestestR/reference/describe_prior.md)
  and
  [`simulate_prior()`](https://easystats.github.io/bayestestR/reference/simulate_prior.md).

## bayestestR 0.8.2

CRAN release: 2021-01-26

### General

- Support for `bamlss` models.

- Roll-back R dependency to R \>= 3.4.

### Changes to functions

- All `.stanreg` methods gain a `component` argument, to also include
  auxiliary parameters.

### Bug fixes

- [`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  no longer errors for no reason when computing extremely un/likely
  direction hypotheses.

- `bayesfactor_pointull()` / `bf_pointull()` are now
  [`bayesfactor_pointnull()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  /
  [`bf_pointnull()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  (can *you* spot the difference?
  [\#363](https://github.com/easystats/bayestestR/issues/363) ).

## bayestestR 0.8.0

CRAN release: 2020-12-05

### New functions

- [`sexit()`](https://easystats.github.io/bayestestR/reference/sexit.md),
  a function for sequential effect existence and significance testing
  (SEXIT).

### General

- Added startup-message to warn users that default ci-width might change
  in a future update.

- Added support for *mcmc.list* objects.

### Bug fixes

- [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)
  gains a `newdata` argument to work with `brmsfit_multiple` models.

- Fixed issue in Bayes factor vignette (don’t evaluate code chunks if
  packages not available).

## bayestestR 0.7.5

CRAN release: 2020-10-22

### New functions

- Added [`as.matrix()`](https://rdrr.io/r/base/matrix.html) function for
  `bayesfactor_model` arrays.

- [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md),
  a utility function to get Bayesian models un-fitted from the data,
  representing the priors only.

### Changes to functions

- [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md)
  supports `emmeans` - both Bayesian and frequentist (
  [\#312](https://github.com/easystats/bayestestR/issues/312) - cross
  fix with `parameters`)

### Bug fixes

- Fixed issue with *default* rope range for `BayesFactor` models.

- Fixed issue in collinearity-check for
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md)
  for models with less than two parameters.

- Fixed issue in print-method for
  [`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)
  with `stanmvreg`-models, which displays the wrong name for the
  response-value.

- Fixed issue in
  [`effective_sample()`](https://easystats.github.io/bayestestR/reference/effective_sample.md)
  for models with only one parameter.

- [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
  for `BayesFactor` models returns non-`NA` values (
  [\#343](https://github.com/easystats/bayestestR/issues/343) )

## bayestestR 0.7.2

CRAN release: 2020-07-20

### New functions

- [`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md),
  to compute average direct and average causal mediation effects of
  multivariate response models (`brmsfit`, `stanmvreg`).

### Bug fixes

- [`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  works with `R<3.6.0`.

## bayestestR 0.7.0

CRAN release: 2020-06-19

### General

- Preliminary support for *stanfit* objects.

- Added support for *bayesQR* objects.

### Changes to functions

- [`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
  can now be used with data frames.

- Revised [`print()`](https://rdrr.io/r/base/print.html) for
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md).

- Improved value formatting for Bayesfactor functions.

### Bug fixes

- Link transformation are now taken into account for `emmeans` objets.
  E.g., in
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md).

- Fix
  [`diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.md)
  when algorithm is not “sampling”.

- Minor revisions to some documentations.

- Fix CRAN check issues for win-old-release.

## bayestestR 0.6.0

CRAN release: 2020-04-20

### Changes to functions

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  now also works on
  [`effectsize::standardize_posteriors()`](https://easystats.github.io/parameters/reference/standardize_parameters.html).

- [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  now also works on
  [`parameters::simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.html).

- [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
  supports more (frequentis) models.

### Bug fixes

- Fixed issue with
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  `data.frame`-methods of
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
  and
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md).

- Fix check issues for forthcoming insight-update.

## bayestestR 0.5.3

CRAN release: 2020-03-26

### General

- Support for *bcplm* objects (package **cplm**)

### Changes to functions

- [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md)
  now also works on grouped data frames.

### Bug fixes

- Fixed bug in
  [`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
  to properly weight Intercept-only `BFBayesFactor` models.

- Fixed bug in
  [`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
  when models have very low posterior probability (
  [\#286](https://github.com/easystats/bayestestR/issues/286) ).

- Fixed bug in
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md),
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md)
  and
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)
  for *brmsfit* models with monotonic effect.

- Fixed issues related to latest changes in `as.data.frame.brmsfit()`
  from the *brms* package.

## bayestestR 0.5.0

CRAN release: 2020-01-18

### General

- Added
  [`p_pointnull()`](https://easystats.github.io/bayestestR/reference/p_map.md)
  as an alias to `p_MAP()`.

- Added [`si()`](https://easystats.github.io/bayestestR/reference/si.md)
  function to compute support intervals.

- Added
  [`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
  for generating posterior samples averaged across models.

- Added [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method
  for
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md).

- [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  now also works for *brmsfit*-objects.

- [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md)
  now also works for *MCMCglmm*-objects.

- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)
  gets `effects` and `component` arguments for *stanreg* and *brmsfit*
  models, to print specific model components.

- Support for *mcmc* objects (package **coda**)

- Provide more distributions via
  [`distribution()`](https://easystats.github.io/bayestestR/reference/distribution.md).

- Added
  [`distribution_tweedie()`](https://easystats.github.io/bayestestR/reference/distribution.md).

- Better handling of `stanmvreg` models for
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md),
  [`diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.md)
  and
  [`describe_prior()`](https://easystats.github.io/bayestestR/reference/describe_prior.md).

### Breaking changes

- [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md):
  argument `centrality` default value changed from ‘median’ to ‘all’.

- [`p_rope()`](https://easystats.github.io/bayestestR/reference/p_rope.md),
  previously as exploratory index, was renamed as `mhdior()` (for *Max
  HDI inside/outside ROPE*), as
  [`p_rope()`](https://easystats.github.io/bayestestR/reference/p_rope.md)
  will refer to `rope(..., ci = 1)` (
  [\#258](https://github.com/easystats/bayestestR/issues/258) )

### Bug fixes

- Fixed mistake in description of
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md).

- Fixed error when computing BFs with `emmGrid` based on some non-linear
  models ( [\#260](https://github.com/easystats/bayestestR/issues/260)
  ).

- Fixed wrong output for percentage-values in
  `print.equivalence_test()`.

- Fixed issue in
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  for `BFBayesFactor`-objects with more than one model.

## bayestestR 0.4.0

CRAN release: 2019-10-20

### New functions / features

- `convert_bayesian_to_frequentist()` Convert (refit) Bayesian model as
  frequentist

- [`distribution_binomial()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  for perfect binomial distributions

- [`simulate_ttest()`](https://easystats.github.io/bayestestR/reference/simulate_correlation.md)
  Simulate data with a mean difference

- [`simulate_correlation()`](https://easystats.github.io/bayestestR/reference/simulate_correlation.md)
  Simulate correlated datasets

- [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  Compute the probability of Practical Significance (ps)

- [`overlap()`](https://easystats.github.io/bayestestR/reference/overlap.md)
  Compute overlap between two empirical distributions

- [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md):
  `method = "mixture"` argument added for mixture density estimation

### Bug fixes

- Fixed bug in
  [`simulate_prior()`](https://easystats.github.io/bayestestR/reference/simulate_prior.md)
  for stanreg-models when `autoscale` was set to `FALSE`

## bayestestR 0.3.0

CRAN release: 2019-09-22

### General

- revised [`print()`](https://rdrr.io/r/base/print.html)-methods for
  functions like
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md),
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md),
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  etc., in particular for model objects with random effects and/or
  zero-inflation component

### New functions / features

- [`check_prior()`](https://easystats.github.io/bayestestR/reference/check_prior.md)
  to check if prior is informative

- [`simulate_prior()`](https://easystats.github.io/bayestestR/reference/simulate_prior.md)
  to simulate model’s priors as distributions

- [`distribution_gamma()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  to generate a (near-perfect or random) Gamma distribution

- `contr.bayes` function for orthogonal factor coding (implementation
  from Singmann & Gronau’s
  [`bfrms`](https://github.com/bayesstuff/bfrms/), used for proper prior
  estimation when factor have 3 levels or more. See Bayes factor
  vignette \## Changes to functions

- Added support for `sim`, `sim.merMod` (from `arm::sim()`) and
  `MCMCglmm`-objects to many functions (like
  [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
  [`eti()`](https://easystats.github.io/bayestestR/reference/eti.md),
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md),
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md),
  [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md),
  …)

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  gets an `effects` and `component` argument, to include the description
  of posterior samples from random effects and/or zero-inflation
  component.

- More user-friendly warning for non-supported models in
  [`bayesfactor()`](https://easystats.github.io/bayestestR/reference/bayesfactor.md)-methods

### Bug fixes

- Fixed bug in
  [`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
  where the same interaction sometimes appeared more than once
  ([\#223](https://github.com/easystats/bayestestR/issues/223))

- Fixed bug in
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  for *stanreg* models fitted with fullrank-algorithm

## bayestestR 0.2.5

CRAN release: 2019-08-06

### Breaking changes

- [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
  for binomial model has now a different default (-.18; .18 ; instead of
  -.055; .055)

- [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md):
  returns a proportion (between 0 and 1) instead of a value between 0
  and 100

- [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md):
  returns a proportion (between 0.5 and 1) instead of a value between 50
  and 100 ([\#168](https://github.com/easystats/bayestestR/issues/168))

- `bayesfactor_savagedickey()`: `hypothesis` argument replaced by `null`
  as part of the new
  [`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  function

### New functions / features

- [`density_at()`](https://easystats.github.io/bayestestR/reference/density_at.md),
  [`p_map()`](https://easystats.github.io/bayestestR/reference/p_map.md)
  and
  [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md):
  `method` argument added

- [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md):
  `ci_method` argument added

- [`eti()`](https://easystats.github.io/bayestestR/reference/eti.md):
  Computes equal-tailed intervals

- [`reshape_ci()`](https://easystats.github.io/datawizard/reference/reshape_ci.html):
  Reshape CIs between wide/long

- [`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md):
  New function, replacing `bayesfactor_savagedickey()`, allows for
  computing Bayes factors against a *point-null* or an *interval-null*

- [`bayesfactor_restricted()`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md):
  Function for computing Bayes factors for order restricted models

### Minor changes

### Bug fixes

- [`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
  now works with `R < 3.6`.

## bayestestR 0.2.2

CRAN release: 2019-06-20

### Breaking changes

- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md):
  returns capitalized output (e.g., `Rejected` instead of `rejected`)

- [`describe_posterior.numeric()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md):
  `dispersion` defaults to `FALSE` for consistency with the other
  methods

### New functions / features

- [`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  and
  [`p_to_pd()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md):
  Functions to convert between probability of direction (pd) and p-value

- Support of `emmGrid` objects:
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md),
  `bayesfactor_savagedickey()`,
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md),
  …

### Minor changes

- Improved tutorial 2

### Bug fixes

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md):
  Fixed column order restoration

- [`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md):
  Inclusion BFs for matched models are more inline with JASP results.

## bayestestR 0.2.0

CRAN release: 2019-05-29

### Breaking changes

- plotting functions now require the installation of the `see` package

- `estimate` argument name in
  [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  and
  [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md)
  changed to `centrality`

- [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md)
  and
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)
  default `ci` to `0.89`

- `rnorm_perfect()` deprecated in favour of
  [`distribution_normal()`](https://easystats.github.io/bayestestR/reference/distribution.md)

- [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md)
  now returns a single value instead of a dataframe and the `density`
  parameter has been removed. The MAP density value is now accessible
  via `attributes(map_output)$MAP_density`

### New functions / features

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md),
  [`describe_prior()`](https://easystats.github.io/bayestestR/reference/describe_prior.md),
  [`diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.md):
  added wrapper function

- [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md)
  added function to compute point estimates

- [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md):
  new argument `method` to compute pd based on AUC

- [`area_under_curve()`](https://easystats.github.io/bayestestR/reference/area_under_curve.md):
  compute AUC

- [`distribution()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  functions have been added

- `bayesfactor_savagedickey()`,
  [`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  and
  [`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
  functions has been added

- Started adding plotting methods (currently in the
  [`see`](https://github.com/easystats/see) package) for
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
  and [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md)

- `probability_at()` as alias for
  [`density_at()`](https://easystats.github.io/bayestestR/reference/density_at.md)

- [`effective_sample()`](https://easystats.github.io/bayestestR/reference/effective_sample.md)
  to return the effective sample size of Stan-models

- [`mcse()`](https://easystats.github.io/bayestestR/reference/mcse.md)
  to return the Monte Carlo standard error of Stan-models

### Minor changes

- Improved documentation

- Improved testing

- [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md):
  improved printing

- [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md)
  for model-objects now returns the HDI values for all parameters as
  attribute in a consistent way

- Changes legend-labels in `plot.equivalence_test()` to align plots with
  the output of the
  [`print()`](https://rdrr.io/r/base/print.html)-method
  ([\#78](https://github.com/easystats/bayestestR/issues/78))

### Bug fixes

- [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md)
  returned multiple class attributes
  ([\#72](https://github.com/easystats/bayestestR/issues/72))

- Printing results from
  [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md)
  failed when `ci`-argument had fractional parts for percentage values
  (e.g. `ci = 0.995`).

- `plot.equivalence_test()` did not work properly for *brms*-models
  ([\#76](https://github.com/easystats/bayestestR/issues/76)).

## bayestestR 0.1.0

CRAN release: 2019-04-08

- CRAN initial publication and [0.1.0
  release](https://github.com/easystats/bayestestR/releases/tag/v0.1.0)

- Added a `NEWS.md` file to track changes to the package
