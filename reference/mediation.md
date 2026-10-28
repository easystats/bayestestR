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
  fail. For *brms* models with more than two model formulas, `treatment`
  must be specified.

- mediator:

  Character, name of the mediator variable in a (multivariate response)
  mediator-model. If missing, `mediation()` tries to find the mediator
  variable automatically, however, this may fail. For *brms* models with
  more than two model formulas, `mediator` must be specified and can
  name several mediators (see 'Details').

- response:

  A named character vector, indicating the names of the response
  variables to be used for the mediation analysis. Usually can be
  `NULL`, in which case these variables are retrieved automatically. If
  not `NULL`, names must match the names of the model formulas. For
  *brms* models, these are `names(insight::find_formula(model))`, one
  name per formula, and the values are the left side of each formula
  without addition terms such as `cens()`. For other models, they are
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

  The point-estimate (centrality index) of the effects. One of the
  options of
  [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md),
  such as `"median"` (default), `"mean"` or `"MAP"`. Only one index is
  allowed.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default to `0.95` (`95%`).

- method:

  Can be
  ["ETI"](https://easystats.github.io/bayestestR/reference/eti.md)
  (default),
  ["HDI"](https://easystats.github.io/bayestestR/reference/hdi.md),
  ["BCI"](https://easystats.github.io/bayestestR/reference/bci.md),
  `"SPI"` (the
  [HDI](https://easystats.github.io/bayestestR/reference/hdi.md) with
  `method = "density"`) or
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

For all values, the 95% credible intervals are calculated by default.
Use `ci` to calculate a different interval.

For models with two model formulas, the arguments `treatment` and
`mediator` do not necessarily need to be specified. If missing,
`mediation()` tries to find the treatment and mediator variable
automatically. If this does not work, specify these variables.

The direct effect is also called *average direct effect* (ADE), the
indirect effect is also called *average causal mediation effects*
(ACME). See also *Tingley et al. 2014* and *Imai et al. 2010*.

For *brms* models, there can be more than one mediator, in parallel or
in series. For models with more than two model formulas, `treatment` and
`mediator` must be specified. Each mediator must be a predictor in the
outcome model, which is the one model whose response is not a predictor
in another model. `treatment` can also be a coefficient name, such as
`xb` for level `b` of a factor `x`. The indirect effect of a path from
the treatment through one or more mediators to the outcome is the
product of the coefficients along that path. With several mediators,
`mediation()` returns the direct effect, one indirect effect per path
(e.g. `Indirect Effect (ACME): m1 -> m2`), the total indirect effect,
one mediator effect per mediator, the total effect and the proportion
mediated. If `mediator` names only some of the mediators, only the paths
through these mediators are used.

## Note

There is an
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
that returns the posterior samples of the effects, which can be used for
further processing in the **bayestestR** package. With several
mediators, the columns are named after the effects.

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
#> Direct Effect (ADE)    |   -0.040 | [-0.127,  0.041]
#> Indirect Effect (ACME) |   -0.016 | [-0.040,  0.008]
#> Mediator Effect        |   -0.240 | [-0.299, -0.182]
#> Total Effect           |   -0.056 | [-0.144,  0.030]
#> 
#> Proportion mediated: 28.06% [-196.42%, 252.53%]
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
# \donttest{
library(brms)

# two mediators in series: m1 is also a predictor of m2
set.seed(123)
d <- data.frame(x = rnorm(200))
d$m1 <- 0.5 * d$x + rnorm(200)
d$m2 <- 0.4 * d$x + 0.5 * d$m1 + rnorm(200)
d$y <- 0.3 * d$x + 0.6 * d$m1 + 0.4 * d$m2 + rnorm(200)

f <- bf(y ~ x + m1 + m2) + bf(m1 ~ x) + bf(m2 ~ x + m1) + set_rescor(FALSE)
m4 <- brm(f, data = d, refresh = 0)
#> Compiling Stan program...
#> Start sampling

# with more than two model formulas, specify treatment and mediators
mediation(m4, treatment = "x", mediator = c("m1", "m2"))
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: x
#>   Mediator : m1, m2
#>   Response : y
#> 
#> Effect                           | Estimate |        95% ETI
#> ------------------------------------------------------------
#> Direct Effect (ADE)              |    0.198 | [0.025, 0.369]
#> Indirect Effect (ACME): m1       |    0.310 | [0.199, 0.448]
#> Indirect Effect (ACME): m2       |    0.170 | [0.088, 0.269]
#> Indirect Effect (ACME): m1 -> m2 |    0.090 | [0.048, 0.149]
#> Total Indirect Effect            |    0.575 | [0.424, 0.748]
#> Mediator Effect: m1              |    0.666 | [0.507, 0.830]
#> Mediator Effect: m2              |    0.436 | [0.287, 0.589]
#> Total Effect                     |    0.774 | [0.566, 0.986]
#> 
#> Proportion mediated: 74.28% [54.88%, 93.67%]
#> 
# }
```
