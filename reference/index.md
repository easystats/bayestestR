# Package index

## Posterior Description

- [`describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.md)
  : Describe Posterior Distributions
- [`describe_prior()`](https://easystats.github.io/bayestestR/reference/describe_prior.md)
  : Describe Priors
- [`sexit()`](https://easystats.github.io/bayestestR/reference/sexit.md)
  : Sequential Effect eXistence and sIgnificance Testing (SEXIT)

## Centrality and Uncertainty

- [`as.numeric(`*`<map_estimate>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  [`as.numeric(`*`<p_direction>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  [`as.numeric(`*`<p_map>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  [`as.numeric(`*`<p_significance>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  : Convert to Numeric
- [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md)
  : Maximum A Posteriori probability estimate (MAP)
- [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md)
  : Point-estimates of posterior distributions
- [`bci()`](https://easystats.github.io/bayestestR/reference/bci.md)
  [`bcai()`](https://easystats.github.io/bayestestR/reference/bci.md) :
  Bias Corrected and Accelerated Interval (BCa)
- [`eti()`](https://easystats.github.io/bayestestR/reference/eti.md) :
  Equal-Tailed Interval (ETI)
- [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md) :
  Highest Density Interval (HDI)
- [`spi()`](https://easystats.github.io/bayestestR/reference/spi.md) :
  Shortest Probability Interval (SPI)
- [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md) :
  Confidence/Credible/Compatibility Interval (CI)

## Effect Existence and Significance

Functions for Bayesian Inference

### Posterior Based Methods

- [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
  [`pd()`](https://easystats.github.io/bayestestR/reference/p_direction.md)
  : Probability of Direction (pd)
- [`p_map()`](https://easystats.github.io/bayestestR/reference/p_map.md)
  [`p_pointnull()`](https://easystats.github.io/bayestestR/reference/p_map.md)
  : Bayesian p-value based on the density at the Maximum A Posteriori
  (MAP)
- [`p_rope()`](https://easystats.github.io/bayestestR/reference/p_rope.md)
  : Probability of being in the ROPE
- [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md)
  : Practical Significance (ps)
- [`p_to_bf()`](https://easystats.github.io/bayestestR/reference/p_to_bf.md)
  : Convert p-values to (pseudo) Bayes Factors
- [`pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  [`p_to_pd()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  [`convert_p_to_pd()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  [`convert_pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.md)
  : Convert between Probability of Direction (pd) and p-value.
- [`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bayesfactor_pointnull()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bayesfactor_rope()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bf_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bf_pointnull()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bf_rope()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  : Bayes Factors (BF) for a Single Parameter
- [`rope()`](https://easystats.github.io/bayestestR/reference/rope.md) :
  Region of Practical Equivalence (ROPE)
- [`rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.md)
  : Find Default Equivalence (ROPE) Region Bounds
- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.md)
  : Test for Practical Equivalence

### Bayes factors

- [`bayesfactor()`](https://easystats.github.io/bayestestR/reference/bayesfactor.md)
  : Bayes Factors (BF)
- [`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
  [`bf_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md)
  : Inclusion Bayes Factors for testing predictors across Bayesian
  models
- [`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  [`bf_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  [`update(`*`<bayesfactor_models>`*`)`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  [`as.matrix(`*`<bayesfactor_models>`*`)`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  : Bayes Factors (BF) for model comparison
- [`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bayesfactor_pointnull()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bayesfactor_rope()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bf_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bf_pointnull()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  [`bf_rope()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md)
  : Bayes Factors (BF) for a Single Parameter
- [`bayesfactor_restricted()`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md)
  [`bf_restricted()`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md)
  [`as.logical(`*`<bayesfactor_restricted>`*`)`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md)
  [`as.matrix(`*`<bayesfactor_restricted>`*`)`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md)
  : Bayes Factors (BF) for Order Restricted Models
- [`si()`](https://easystats.github.io/bayestestR/reference/si.md) :
  Compute Support Intervals
- [`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
  : Generate posterior distributions weighted across models
- [`bic_to_bf()`](https://easystats.github.io/bayestestR/reference/bic_to_bf.md)
  : Convert BIC indices to Bayes Factors via the BIC-approximation
  method.
- [`p_to_bf()`](https://easystats.github.io/bayestestR/reference/p_to_bf.md)
  : Convert p-values to (pseudo) Bayes Factors

## Model Diagnostics

- [`diagnostic_posterior()`](https://easystats.github.io/bayestestR/reference/diagnostic_posterior.md)
  : Posteriors Sampling Diagnostic
- [`sensitivity_to_prior()`](https://easystats.github.io/bayestestR/reference/sensitivity_to_prior.md)
  : Sensitivity to Prior
- [`check_prior()`](https://easystats.github.io/bayestestR/reference/check_prior.md)
  : Check if Prior is Informative
- [`simulate_correlation()`](https://easystats.github.io/bayestestR/reference/simulate_correlation.md)
  [`simulate_ttest()`](https://easystats.github.io/bayestestR/reference/simulate_correlation.md)
  [`simulate_difference()`](https://easystats.github.io/bayestestR/reference/simulate_correlation.md)
  : Data Simulation
- [`simulate_prior()`](https://easystats.github.io/bayestestR/reference/simulate_prior.md)
  : Returns Priors of a Model as Empirical Distributions
- [`simulate_simpson()`](https://easystats.github.io/bayestestR/reference/simulate_simpson.md)
  : Simpson's paradox dataset simulation
- [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)
  : Un-update Bayesian models to their prior-to-data state
- [`effective_sample()`](https://easystats.github.io/bayestestR/reference/effective_sample.md)
  : Effective Sample Size (ESS)
- [`mcse()`](https://easystats.github.io/bayestestR/reference/mcse.md) :
  Monte-Carlo Standard Error (MCSE)

## Density Estimation

- [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.md)
  : Density Estimation
- [`density_at()`](https://easystats.github.io/bayestestR/reference/density_at.md)
  : Density Probability at a Given Value
- [`area_under_curve()`](https://easystats.github.io/bayestestR/reference/area_under_curve.md)
  [`auc()`](https://easystats.github.io/bayestestR/reference/area_under_curve.md)
  : Area under the Curve (AUC)
- [`overlap()`](https://easystats.github.io/bayestestR/reference/overlap.md)
  : Overlap Coefficient

## Distributions

- [`distribution()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_custom()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_beta()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_binomial()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_binom()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_cauchy()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_chisquared()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_chisq()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_gamma()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_mixture_normal()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_normal()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_gaussian()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_nbinom()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_poisson()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_student()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_t()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_student_t()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_tweedie()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  [`distribution_uniform()`](https://easystats.github.io/bayestestR/reference/distribution.md)
  : Empirical Distributions

## Utilities

- [`display(`*`<describe_posterior>`*`)`](https://easystats.github.io/bayestestR/reference/display.describe_posterior.md)
  [`print(`*`<describe_posterior>`*`)`](https://easystats.github.io/bayestestR/reference/display.describe_posterior.md)
  [`print_html(`*`<describe_posterior>`*`)`](https://easystats.github.io/bayestestR/reference/display.describe_posterior.md)
  [`print_md(`*`<describe_posterior>`*`)`](https://easystats.github.io/bayestestR/reference/display.describe_posterior.md)
  : Print tables in different output formats
- [`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)
  : Summary of Bayesian multivariate-response mediation-models
- [`convert_bayesian_as_frequentist()`](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.md)
  [`bayesian_as_frequentist()`](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.md)
  : Convert (refit) a Bayesian model to frequentist
- [`contr.equalprior()`](https://easystats.github.io/bayestestR/reference/contr.equalprior.md)
  [`contr.equalprior_pairs()`](https://easystats.github.io/bayestestR/reference/contr.equalprior.md)
  [`contr.equalprior_deviations()`](https://easystats.github.io/bayestestR/reference/contr.equalprior.md)
  : Contrast Matrices for Equal Marginal Priors in Bayesian Estimation
- [`as.numeric(`*`<map_estimate>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  [`as.numeric(`*`<p_direction>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  [`as.numeric(`*`<p_map>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  [`as.numeric(`*`<p_significance>`*`)`](https://easystats.github.io/bayestestR/reference/as.numeric.p_direction.md)
  : Convert to Numeric
- [`as.data.frame(`*`<density>`*`)`](https://easystats.github.io/bayestestR/reference/as.data.frame.density.md)
  : Coerce to a Data Frame
- [`sexit_thresholds()`](https://easystats.github.io/bayestestR/reference/sexit_thresholds.md)
  : Find Effect Size Thresholds
- [`reshape_iterations()`](https://easystats.github.io/bayestestR/reference/reshape_iterations.md)
  [`reshape_draws()`](https://easystats.github.io/bayestestR/reference/reshape_iterations.md)
  : Reshape estimations with multiple iterations (draws) to long format
- [`diagnostic_draws()`](https://easystats.github.io/bayestestR/reference/diagnostic_draws.md)
  : Diagnostic values for each iteration
- [`model_to_priors()`](https://easystats.github.io/bayestestR/reference/model_to_priors.md)
  : Convert model's posteriors to priors (EXPERIMENTAL)
- [`disgust`](https://easystats.github.io/bayestestR/reference/disgust.md)
  : Moral Disgust Judgment
