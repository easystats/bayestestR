# Bayes Factors (BF)

This function compte the Bayes factors (BFs) that are appropriate to the
input. For vectors or single models, it will compute
[`BFs for single parameters`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md),
or is `hypothesis` is specified,
[`BFs for restricted models`](https://easystats.github.io/bayestestR/reference/bayesfactor_restricted.md).
For multiple models, it will return the BF corresponding to
[`comparison between models`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
and if a model comparison is passed, it will compute the
[`inclusion BF`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md).  
  
For a complete overview of these functions, read the [Bayes factor
vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).

## Usage

``` r
bayesfactor(
  ...,
  prior = NULL,
  direction = "two-sided",
  null = 0,
  hypothesis = NULL,
  effects = "fixed",
  verbose = TRUE,
  denominator = 1,
  match_models = FALSE,
  prior_odds = NULL
)
```

## Arguments

- ...:

  A numeric vector, model object(s), or the output from
  `bayesfactor_models`.

- prior:

  An object representing a prior distribution (see 'Details').

- direction:

  Test type (see 'Details'). One of `0`, `"two-sided"` (default, two
  tailed), `-1`, `"left"` (left tailed) or `1`, `"right"` (right
  tailed).

- null:

  Value of the null, either a scalar (for point-null) or a range (for a
  interval-null).

- hypothesis:

  A character vector specifying the restrictions as logical conditions
  (see examples below).

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

- verbose:

  Toggle off warnings.

- denominator:

  Either an integer indicating which of the models to use as the
  denominator, or a model to be used as a denominator. Ignored for
  `BFBayesFactor`.

- match_models:

  See details.

- prior_odds:

  Optional vector of prior odds for the models. See
  `BayesFactor::priorOdds<-`.

## Value

Some type of Bayes factor, depending on the input. See
[`bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.md),
[`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
or
[`bayesfactor_inclusion()`](https://easystats.github.io/bayestestR/reference/bayesfactor_inclusion.md).

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Examples

``` r
# \dontrun{
library(bayestestR)

prior <- distribution_normal(1000, mean = 0, sd = 1)
posterior <- distribution_normal(1000, mean = 0.5, sd = 0.3)

bayesfactor(posterior, prior = prior, verbose = FALSE)
#> Bayes Factor (Savage-Dickey density ratio)
#> 
#> BF  
#> ----
#> 1.21
#> 
#> * Evidence Against The Null: 0
#> 

# rstanarm models
# ---------------
model <- suppressWarnings(rstanarm::stan_lmer(extra ~ group + (1 | ID), data = sleep))
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 5.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.53 seconds.
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
#> Chain 1:  Elapsed Time: 0.218 seconds (Warm-up)
#> Chain 1:                0.171 seconds (Sampling)
#> Chain 1:                0.389 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.6e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.16 seconds.
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
#> Chain 2:  Elapsed Time: 0.205 seconds (Warm-up)
#> Chain 2:                0.199 seconds (Sampling)
#> Chain 2:                0.404 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.19 seconds (Warm-up)
#> Chain 3:                0.149 seconds (Sampling)
#> Chain 3:                0.339 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.7e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.17 seconds.
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
#> Chain 4:  Elapsed Time: 0.222 seconds (Warm-up)
#> Chain 4:                0.183 seconds (Sampling)
#> Chain 4:                0.405 seconds (Total)
#> Chain 4: 
bayesfactor(model, verbose = FALSE)
#> Bayes Factor (Savage-Dickey density ratio) 
#> 
#> Parameter   |    BF
#> -------------------
#> (Intercept) | 0.184
#> group2      |  2.64
#> 
#> * Evidence Against The Null: 0
#> 

# Frequentist models
# ---------------
m0 <- lm(extra ~ 1, data = sleep)
m1 <- lm(extra ~ group, data = sleep)
m2 <- lm(extra ~ group + ID, data = sleep)

comparison <- bayesfactor(m0, m1, m2)
comparison
#> Bayes Factors for Model Comparison
#> 
#>       Model            BF
#> [..2] group          1.30
#> [..3] group + ID 1.12e+04
#> 
#> * Against Denominator: [..1] (Intercept only)
#> *   Bayes Factor Type: BIC approximation

bayesfactor(comparison)
#> Inclusion Bayes Factors (Model Averaged)
#> 
#>       P(prior) P(posterior) Inclusion BF
#> group     0.67         1.00     5.61e+03
#> ID        0.33         1.00     9.77e+03
#> 
#> * Compared among: all models
#> *    Priors odds: uniform-equal
# }
```
