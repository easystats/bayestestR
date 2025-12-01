# Bayes Factors (BF) for model comparison

This function computes or extracts Bayes factors from fitted models. The
`bf_*` function is an alias of the main function.

## Usage

``` r
bayesfactor_models(..., denominator = 1, verbose = TRUE)

bf_models(..., denominator = 1, verbose = TRUE)

# Default S3 method
bayesfactor_models(..., denominator = 1, verbose = TRUE)

# S3 method for class 'bayesfactor_models'
update(object, subset = NULL, reference = NULL, ...)

# S3 method for class 'bayesfactor_models'
as.matrix(x, ...)
```

## Arguments

- ...:

  Fitted models (see details), all fit on the same data, or a single
  `BFBayesFactor` object (see 'Details'). Ignored in
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html),
  [`update()`](https://rdrr.io/r/stats/update.html). If the following
  named arguments are present, they are passed to
  [`insight::get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.html)
  (see details):

  - `estimator` (defaults to `"ML"`)

  - `check_response` (defaults to `FALSE`)

- denominator:

  Either an integer indicating which of the models to use as the
  denominator, or a model to be used as a denominator. Ignored for
  `BFBayesFactor`.

- verbose:

  Toggle off warnings.

- object, x:

  A `bayesfactor_models()` object.

- subset:

  Vector of model indices to keep or remove.

- reference:

  Index of model to reference to, or `"top"` to reference to the best
  model, or `"bottom"` to reference to the worst model.

## Value

A data frame containing the models' formulas (reconstructed fixed and
random effects) and their `log(BF)`s (Use
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) to extract the
non-log Bayes factors; see examples), that prints nicely.  
  
For [`as.matrix()`](https://rdrr.io/r/base/matrix.html) a square matrix
of (log) Bayes factors, with rows as denominators and columns as
numerators.

## Details

If the passed models are supported by **insight** the DV of all models
will be tested for equality (else this is assumed to be true), and the
models' terms will be extracted (allowing for follow-up analysis with
`bayesfactor_inclusion`).

- For `brmsfit` or `stanreg` models, Bayes factors are computed using
  the
  [bridgesampling](https://CRAN.R-project.org/package=bridgesampling)
  package.

  - `brmsfit` models must have been fitted with
    `save_pars = save_pars(all = TRUE)`.

  - `stanreg` models must have been fitted with a defined
    `diagnostic_file`.

- For `BFBayesFactor`, `bayesfactor_models()` is mostly a wraparound
  [`BayesFactor::extractBF()`](https://rdrr.io/pkg/BayesFactor/man/extractBF-methods.html).

- For all other model types, Bayes factors are computed using the BIC
  approximation. Note that BICs are extracted from using
  [insight::get_loglikelihood](https://easystats.github.io/insight/reference/get_loglikelihood.html),
  see documentation there for options for dealing with transformed
  responses and REML estimation.

In order to correctly and precisely estimate Bayes factors, a rule of
thumb are the 4 P's: **P**roper **P**riors and **P**lentiful
**P**osteriors. How many? The number of posterior samples needed for
testing is substantially larger than for estimation (the default of 4000
samples may not be enough in many cases). A conservative rule of thumb
is to obtain 10 times more samples than would be required for estimation
(*Gronau, Singmann, & Wagenmakers, 2017*). If less than 40,000 samples
are detected, `bayesfactor_models()` gives a warning.

See also [the Bayes factors
vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Transitivity of Bayes factors

For multiple inputs (models or hypotheses), the function will return
multiple Bayes factors between each model and *the same* reference model
(the `denominator` or un-restricted model). However, we can take
advantage of the transitivity of Bayes factors - where if we have two
Bayes factors for Model *A* and model *B* against the *same reference
model C*, we can obtain a Bayes factor for comparing model *A* to model
*B* by dividing them:  
  
\$\$BF\_{AB} = \frac{BF\_{AC}}{BF\_{BC}} =
\frac{\frac{ML\_{A}}{ML\_{C}}}{\frac{ML\_{B}}{ML\_{C}}} =
\frac{ML\_{A}}{ML\_{B}}\$\$  
  
A full matrix comparing all models can be obtained with
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) (see examples).

## Interpreting Bayes Factors

A Bayes factor greater than 1 can be interpreted as evidence against the
null, at which one convention is that a Bayes factor greater than 3 can
be considered as "substantial" evidence against the null (and vice
versa, a Bayes factor smaller than 1/3 indicates substantial evidence in
favor of the null-model) (Wetzels et al. 2011).

## References

- Gronau, Q. F., Singmann, H., & Wagenmakers, E. J. (2017).
  Bridgesampling: An R package for estimating normalizing constants.
  arXiv preprint arXiv:1710.08162.

- Kass, R. E., and Raftery, A. E. (1995). Bayes Factors. Journal of the
  American Statistical Association, 90(430), 773-795.

- Robert, C. P. (2016). The expected demise of the Bayes factor. Journal
  of Mathematical Psychology, 72, 33–37.

- Wagenmakers, E. J. (2007). A practical solution to the pervasive
  problems of p values. Psychonomic bulletin & review, 14(5), 779-804.

- Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J.,
  and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental
  Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on
  Psychological Science, 6(3), 291–298.
  [doi:10.1177/1745691611406923](https://doi.org/10.1177/1745691611406923)

## Author

Mattan S. Ben-Shachar

## Examples

``` r
# With lm objects:
# ----------------
lm1 <- lm(mpg ~ 1, data = mtcars)
lm2 <- lm(mpg ~ hp, data = mtcars)
lm3 <- lm(mpg ~ hp + drat, data = mtcars)
lm4 <- lm(mpg ~ hp * drat, data = mtcars)
(BFM <- bayesfactor_models(lm1, lm2, lm3, lm4, denominator = 1))
#> Bayes Factors for Model Comparison
#> 
#>       Model           BF
#> [lm2] hp        4.54e+05
#> [lm3] hp + drat 7.70e+07
#> [lm4] hp * drat 1.59e+07
#> 
#> * Against Denominator: [lm1] (Intercept only)
#> *   Bayes Factor Type: BIC approximation
# bayesfactor_models(lm2, lm3, lm4, denominator = lm1) # same result
# bayesfactor_models(lm1, lm2, lm3, lm4, denominator = lm1) # same result

update(BFM, reference = "bottom")
#> Bayes Factors for Model Comparison
#> 
#>       Model           BF
#> [lm2] hp        4.54e+05
#> [lm3] hp + drat 7.70e+07
#> [lm4] hp * drat 1.59e+07
#> 
#> * Against Denominator: [lm1] (Intercept only)
#> *   Bayes Factor Type: BIC approximation
as.matrix(BFM)
#> # Bayes Factors for Model Comparison
#> 
#>  Denominator\Numerator |      [1] |      [2] |      [3] |      [4]
#> -----------------------------------------------------------------
#> [1] (Intercept only)   |        1 | 4.54e+05 | 7.70e+07 | 1.59e+07
#> [2] hp                 | 2.20e-06 |        1 |   169.72 |    35.09
#> [3] hp + drat          | 1.30e-08 |    0.006 |        1 |    0.207
#> [4] hp * drat          | 6.28e-08 |    0.028 |     4.84 |        1
as.numeric(BFM)
#> [1]        1.0   453874.3 77029881.3 15925712.4

lm2b <- lm(sqrt(mpg) ~ hp, data = mtcars)
# Set check_response = TRUE for transformed responses
bayesfactor_models(lm2b, denominator = lm2, check_response = TRUE)
#> Bayes Factors for Model Comparison
#> 
#>        Model   BF
#> [lm2b] hp    6.94
#> 
#> * Against Denominator: [lm2] hp
#> *   Bayes Factor Type: BIC approximation

# \donttest{
# With lmerMod objects:
# ---------------------
lmer1 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
lmer2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
#> boundary (singular) fit: see help('isSingular')
lmer3 <- lme4::lmer(
  Sepal.Length ~ Petal.Length + (Petal.Length | Species) + (1 | Petal.Width),
  data = iris
)
#> boundary (singular) fit: see help('isSingular')
bayesfactor_models(lmer1, lmer2, lmer3,
  denominator = 1,
  estimator = "REML"
)
#> Bayes Factors for Model Comparison
#> 
#>         Model                                                          BF
#> [lmer2] Petal.Length + (Petal.Length | Species)                     0.058
#> [lmer3] Petal.Length + (Petal.Length | Species) + (1 | Petal.Width) 0.005
#> 
#> * Against Denominator: [lmer1] Petal.Length + (1 | Species)
#> *   Bayes Factor Type: BIC approximation

# rstanarm models
# ---------------------
# (note that a unique diagnostic_file MUST be specified in order to work)
stan_m0 <- suppressWarnings(rstanarm::stan_glm(Sepal.Length ~ 1,
  data = iris,
  family = gaussian(),
  diagnostic_file = file.path(tempdir(), "df0.csv")
))
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.9e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.19 seconds.
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
#> Chain 1:  Elapsed Time: 0.018 seconds (Warm-up)
#> Chain 1:                0.036 seconds (Sampling)
#> Chain 1:                0.054 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 8e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.08 seconds.
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
#> Chain 2:  Elapsed Time: 0.017 seconds (Warm-up)
#> Chain 2:                0.036 seconds (Sampling)
#> Chain 2:                0.053 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 8e-06 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.08 seconds.
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
#> Chain 3:  Elapsed Time: 0.018 seconds (Warm-up)
#> Chain 3:                0.035 seconds (Sampling)
#> Chain 3:                0.053 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 8e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.08 seconds.
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
#> Chain 4:                0.037 seconds (Sampling)
#> Chain 4:                0.055 seconds (Total)
#> Chain 4: 
stan_m1 <- suppressWarnings(rstanarm::stan_glm(Sepal.Length ~ Species,
  data = iris,
  family = gaussian(),
  diagnostic_file = file.path(tempdir(), "df1.csv")
))
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.9e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.19 seconds.
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
#> Chain 1:  Elapsed Time: 0.028 seconds (Warm-up)
#> Chain 1:                0.046 seconds (Sampling)
#> Chain 1:                0.074 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
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
#> Chain 2:  Elapsed Time: 0.03 seconds (Warm-up)
#> Chain 2:                0.047 seconds (Sampling)
#> Chain 2:                0.077 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
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
#> Chain 3:  Elapsed Time: 0.029 seconds (Warm-up)
#> Chain 3:                0.047 seconds (Sampling)
#> Chain 3:                0.076 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 9e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
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
#> Chain 4:  Elapsed Time: 0.029 seconds (Warm-up)
#> Chain 4:                0.048 seconds (Sampling)
#> Chain 4:                0.077 seconds (Total)
#> Chain 4: 
stan_m2 <- suppressWarnings(rstanarm::stan_glm(Sepal.Length ~ Species + Petal.Length,
  data = iris,
  family = gaussian(),
  diagnostic_file = file.path(tempdir(), "df2.csv")
))
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.2 seconds.
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
#> Chain 1:  Elapsed Time: 0.098 seconds (Warm-up)
#> Chain 1:                0.111 seconds (Sampling)
#> Chain 1:                0.209 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
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
#> Chain 2:  Elapsed Time: 0.087 seconds (Warm-up)
#> Chain 2:                0.108 seconds (Sampling)
#> Chain 2:                0.195 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 9e-06 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
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
#> Chain 3:  Elapsed Time: 0.089 seconds (Warm-up)
#> Chain 3:                0.111 seconds (Sampling)
#> Chain 3:                0.2 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 9e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
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
#> Chain 4:  Elapsed Time: 0.083 seconds (Warm-up)
#> Chain 4:                0.1 seconds (Sampling)
#> Chain 4:                0.183 seconds (Total)
#> Chain 4: 
bayesfactor_models(stan_m1, stan_m2, denominator = stan_m0, verbose = FALSE)
#> Bayes Factors for Model Comparison
#> 
#>     Model                        BF
#> [1] Species                6.27e+27
#> [2] Species + Petal.Length 2.25e+53
#> 
#> * Against Denominator: [3] (Intercept only)
#> *   Bayes Factor Type: marginal likelihoods (bridgesampling)


# brms models
# --------------------
# (note the save_pars MUST be set to save_pars(all = TRUE) in order to work)
brm1 <- brms::brm(Sepal.Length ~ 1, data = iris, save_pars = save_pars(all = TRUE))
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.23 seconds.
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
#> Chain 1:  Elapsed Time: 0.031 seconds (Warm-up)
#> Chain 1:                0.03 seconds (Sampling)
#> Chain 1:                0.061 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 7e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.07 seconds.
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
#> Chain 2:  Elapsed Time: 0.03 seconds (Warm-up)
#> Chain 2:                0.026 seconds (Sampling)
#> Chain 2:                0.056 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 7e-06 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.07 seconds.
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
#> Chain 3:  Elapsed Time: 0.031 seconds (Warm-up)
#> Chain 3:                0.034 seconds (Sampling)
#> Chain 3:                0.065 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 7e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.07 seconds.
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
#> Chain 4:  Elapsed Time: 0.033 seconds (Warm-up)
#> Chain 4:                0.035 seconds (Sampling)
#> Chain 4:                0.068 seconds (Total)
#> Chain 4: 
brm2 <- brms::brm(Sepal.Length ~ Species, data = iris, save_pars = save_pars(all = TRUE))
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 6e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.06 seconds.
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
#> Chain 1:                0.016 seconds (Sampling)
#> Chain 1:                0.033 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 2.7e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.27 seconds.
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
#> Chain 2:  Elapsed Time: 0.018 seconds (Warm-up)
#> Chain 2:                0.017 seconds (Sampling)
#> Chain 2:                0.035 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.017 seconds (Warm-up)
#> Chain 3:                0.015 seconds (Sampling)
#> Chain 3:                0.032 seconds (Total)
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
#> Chain 4:                0.016 seconds (Sampling)
#> Chain 4:                0.034 seconds (Total)
#> Chain 4: 
brm3 <- brms::brm(
  Sepal.Length ~ Species + Petal.Length,
  data = iris,
  save_pars = save_pars(all = TRUE)
)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
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
#> Chain 1:  Elapsed Time: 0.049 seconds (Warm-up)
#> Chain 1:                0.059 seconds (Sampling)
#> Chain 1:                0.108 seconds (Total)
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
#> Chain 2:  Elapsed Time: 0.051 seconds (Warm-up)
#> Chain 2:                0.054 seconds (Sampling)
#> Chain 2:                0.105 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.048 seconds (Warm-up)
#> Chain 3:                0.053 seconds (Sampling)
#> Chain 3:                0.101 seconds (Total)
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
#> Chain 4:  Elapsed Time: 0.052 seconds (Warm-up)
#> Chain 4:                0.053 seconds (Sampling)
#> Chain 4:                0.105 seconds (Total)
#> Chain 4: 

bayesfactor_models(brm1, brm2, brm3, denominator = 1, verbose = FALSE)
#> Bayes Factors for Model Comparison
#> 
#>     Model                        BF
#> [2] Species                5.86e+29
#> [3] Species + Petal.Length 7.50e+55
#> 
#> * Against Denominator: [1] (Intercept only)
#> *   Bayes Factor Type: marginal likelihoods (bridgesampling)


# BayesFactor
# ---------------------------
data(puzzles)
BF <- BayesFactor::anovaBF(RT ~ shape * color + ID,
  data = puzzles,
  whichRandom = "ID", progress = FALSE
)
BF
#> Bayes factor analysis
#> --------------
#> [1] shape + ID                       : 2.841658 ±0.92%
#> [2] color + ID                       : 2.830879 ±0.86%
#> [3] shape + color + ID               : 11.75567 ±1.98%
#> [4] shape + color + shape:color + ID : 4.371906 ±1.99%
#> 
#> Against denominator:
#>   RT ~ ID 
#> ---
#> Bayes factor type: BFlinearModel, JZS
#> 
bayesfactor_models(BF) # basically the same
#> Bayes Factors for Model Comparison
#> 
#>     Model                               BF
#> [2] shape + ID                        2.84
#> [3] color + ID                        2.83
#> [4] shape + color + ID               11.76
#> [5] shape + color + shape:color + ID  4.37
#> 
#> * Against Denominator: [1] ID
#> *   Bayes Factor Type: JZS (BayesFactor)
# }
```
