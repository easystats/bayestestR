# Summary of Bayesian multivariate-response mediation-models

`mediation()` is a short summary for multivariate-response
mediation-models, i.e. this function computes average direct and average
causal mediation effects of multivariate response models.

## Usage

``` r
mediation(model, ...)

# S3 method for class 'brmsfit'
mediation(
  model,
  treatment,
  mediator,
  response = NULL,
  centrality = "median",
  ci = 0.95,
  method = "ETI",
  ...
)
```

## Arguments

- model:

  A `brmsfit` or `stanmvreg` object.

- ...:

  Not used.

- treatment:

  Character, name of the treatment variable (or direct effect) in a
  (multivariate response) mediator-model. If missing, `mediation()`
  tries to find the treatment variable automatically, however, this may
  fail.

- mediator:

  Character, name of the mediator variable in a (multivariate response)
  mediator-model. If missing, `mediation()` tries to find the treatment
  variable automatically, however, this may fail.

- response:

  A named character vector, indicating the names of the response
  variables to be used for the mediation analysis. Usually can be
  `NULL`, in which case these variables are retrieved automatically. If
  not `NULL`, names should match the names of the model formulas,
  `names(insight::find_response(model, combine = TRUE))`. This can be
  useful if, for instance, the mediator variable used as predictor has a
  different name from the mediator variable used as response. This might
  occur when the mediator is transformed in one model, but used "as is"
  as response variable in the other model. Example: The mediator `m` is
  used as response variable, but the centered version `m_center` is used
  as mediator variable. The second response variable (for the treatment
  model, with the mediator as additional predictor), `y`, is not
  transformed. Then we could use `response` like this:
  `mediation(model, response = c(m = "m_center", y = "y"))`.

- centrality:

  The point-estimates (centrality indices) to compute. Character
  (vector) or list with one or more of these options: `"median"`,
  `"mean"`, `"MAP"` (see
  [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.md)),
  `"trimmed"` (which is just `mean(x, trim = threshold)`), `"mode"` or
  `"all"`.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default to `0.95` (`95%`).

- method:

  Can be
  ["ETI"](https://easystats.github.io/bayestestR/reference/eti.md)
  (default),
  ["HDI"](https://easystats.github.io/bayestestR/reference/hdi.md),
  ["BCI"](https://easystats.github.io/bayestestR/reference/bci.md),
  ["SPI"](https://easystats.github.io/bayestestR/reference/spi.md) or
  ["SI"](https://easystats.github.io/bayestestR/reference/si.md).

## Value

A data frame with direct, indirect, mediator and total effect of a
multivariate-response mediation-model, as well as the proportion
mediated. The effect sizes are median values of the posterior samples
(use `centrality` for other centrality indices).

## Details

`mediation()` returns a data frame with information on the *direct
effect* (mean value of posterior samples from `treatment` of the outcome
model), *mediator effect* (mean value of posterior samples from
`mediator` of the outcome model), *indirect effect* (mean value of the
multiplication of the posterior samples from `mediator` of the outcome
model and the posterior samples from `treatment` of the mediation model)
and the total effect (mean value of sums of posterior samples used for
the direct and indirect effect). The *proportion mediated* is the
indirect effect divided by the total effect.

For all values, the `89%` credible intervals are calculated by default.
Use `ci` to calculate a different interval.

The arguments `treatment` and `mediator` do not necessarily need to be
specified. If missing, `mediation()` tries to find the treatment and
mediator variable automatically. If this does not work, specify these
variables.

The direct effect is also called *average direct effect* (ADE), the
indirect effect is also called *average causal mediation effects*
(ACME). See also *Tingley et al. 2014* and *Imai et al. 2010*.

## Note

There is an
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
that returns the posterior samples of the effects, which can be used for
further processing in the **bayestestR** package.

## References

- Imai, K., Keele, L. and Tingley, D. (2010) A General Approach to
  Causal Mediation Analysis, Psychological Methods, Vol. 15, No. 4
  (December), pp. 309-334.

- Tingley, D., Yamamoto, T., Hirose, K., Imai, K. and Keele, L. (2014).
  mediation: R package for Causal Mediation Analysis, Journal of
  Statistical Software, Vol. 59, No. 5, pp. 1-38.

## See also

The mediation package for a causal mediation analysis in the frequentist
framework.

## Examples

``` r
# \donttest{
library(mediation)
library(brms)
library(rstanarm)

# load sample data
data(jobs)
set.seed(123)

# linear models, for mediation analysis
b1 <- lm(job_seek ~ treat + econ_hard + sex + age, data = jobs)
b2 <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data = jobs)
# mediation analysis, for comparison with Stan models
m1 <- mediate(b1, b2, sims = 1000, treat = "treat", mediator = "job_seek")

# Fit Bayesian mediation model in brms
f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)
m2 <- brm(f1 + f2 + set_rescor(FALSE), data = jobs, refresh = 0)
#> Compiling Stan program...
#> Start sampling

# Fit Bayesian mediation model in rstanarm
m3 <- suppressWarnings(stan_mvmer(
  list(
    job_seek ~ treat + econ_hard + sex + age + (1 | occp),
    depress2 ~ treat + job_seek + econ_hard + sex + age + (1 | occp)
  ),
  data = jobs,
  refresh = 0
))
#> Fitting a multivariate glmer model.
#> 
#> Please note the warmup may be much slower than later iterations!

summary(m1)
#> 
#> Causal Mediation Analysis 
#> 
#> Quasi-Bayesian Confidence Intervals
#> 
#>                  Estimate 95% CI Lower 95% CI Upper p-value
#> ACME           -0.0157168   -0.0387462    0.0075162   0.192
#> ADE            -0.0438229   -0.1315386    0.0384077   0.348
#> Total Effect   -0.0595397   -0.1530295    0.0235004   0.206
#> Prop. Mediated  0.2136537   -2.0276750    2.6953662   0.322
#> 
#> Sample Size Used: 899 
#> 
#> 
#> Simulations: 1000 
#> 
mediation(m2, centrality = "mean", ci = 0.95)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.040 | [-0.125,  0.045]
#> Indirect Effect (ACME) |   -0.016 | [-0.041,  0.009]
#> Mediator Effect        |   -0.240 | [-0.295, -0.183]
#> Total Effect           |   -0.056 | [-0.139,  0.032]
#> 
#> Proportion mediated: 27.87% [-169.41%, 225.15%]
#> 
mediation(m3, centrality = "mean", ci = 0.95)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.041 | [-0.124,  0.041]
#> Indirect Effect (ACME) |   -0.018 | [-0.043,  0.006]
#> Mediator Effect        |   -0.241 | [-0.298, -0.183]
#> Total Effect           |   -0.059 | [-0.145,  0.029]
#> 
#> Proportion mediated: 30.30% [-216.03%, 276.63%]
#> 
# }
```
