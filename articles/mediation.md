# Mediation Analysis using Bayesian Regression Models

This vignettes demonstrates the
[`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)-function.
Before we start, we fit some models, including a mediation-object from
the *mediation*-package and a structural equation modelling approach
with the *lavaan*-package, both of which we use for comparison with
*brms* and *rstanarm*.

## Mediation Analysis in brms and rstanarm

``` r

library(bayestestR)
library(mediation)
library(brms)
library(rstanarm)

# load sample data
data(jobs)

set.seed(123)
# linear models, for mediation analysis
b1 <- lm(job_seek ~ treat + econ_hard + sex + age, data = jobs)
b2 <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data = jobs)

# mediation analysis, for comparison with brms
m1 <- mediate(b1, b2, sims = 1000, treat = "treat", mediator = "job_seek")
```

``` r

# Fit Bayesian mediation model in brms
f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)
m2 <- brm(f1 + f2 + set_rescor(FALSE), data = jobs, refresh = 0)
```

``` r

# Fit Bayesian mediation model in rstanarm
m3 <- stan_mvmer(
  list(
    job_seek ~ treat + econ_hard + sex + age + (1 | occp),
    depress2 ~ treat + job_seek + econ_hard + sex + age + (1 | occp)
  ),
  data = jobs,
  refresh = 0
)
```

[`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)
is a summary function, especially for mediation analysis, i.e. for
multivariate response models with casual mediation effects.

In the models `m2` and `m3`, `treat` is the treatment effect and
`job_seek` is the mediator effect. For the *brms* model (`m2`), `f1`
describes the mediator model and `f2` describes the outcome model. This
is similar for the *rstanarm* model.

[`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)
returns a data frame with information on the *direct effect* (median
value of posterior samples from treatment of the outcome model),
*mediator effect* (median value of posterior samples from mediator of
the outcome model), *indirect effect* (median value of the
multiplication of the posterior samples from mediator of the outcome
model and the posterior samples from treatment of the mediation model)
and the *total effect* (median value of sums of posterior samples used
for the direct and indirect effect). The *proportion mediated* is the
indirect effect divided by the total effect.

The simplest call just needs the model-object.

``` r

# for brms
mediation(m2)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.040 | [-0.124,  0.046]
#> Indirect Effect (ACME) |   -0.015 | [-0.041,  0.008]
#> Mediator Effect        |   -0.240 | [-0.294, -0.185]
#> Total Effect           |   -0.055 | [-0.145,  0.034]
#> 
#> Proportion mediated: 28.14% [-181.46%, 237.75%]

# for rstanarm
mediation(m3)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.040 | [-0.129,  0.048]
#> Indirect Effect (ACME) |   -0.018 | [-0.042,  0.006]
#> Mediator Effect        |   -0.241 | [-0.296, -0.187]
#> Total Effect           |   -0.057 | [-0.151,  0.033]
#> 
#> Proportion mediated: 30.59% [-221.09%, 282.26%]
```

Typically,
[`mediation()`](https://easystats.github.io/bayestestR/reference/mediation.md)
finds the treatment and mediator variables automatically. If this does
not work, use the `treatment` and `mediator` arguments to specify the
related variable names. For all values, the 89% credible intervals are
calculated by default. Use `ci` to calculate a different interval.

## Comparison to the mediation package

Here is a comparison with the *mediation* package. Note that the
[`summary()`](https://rdrr.io/r/base/summary.html)-output of the
*mediation* package shows the indirect effect first, followed by the
direct effect.

``` r

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

mediation(m2, ci = 0.95)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.040 | [-0.124,  0.046]
#> Indirect Effect (ACME) |   -0.015 | [-0.041,  0.008]
#> Mediator Effect        |   -0.240 | [-0.294, -0.185]
#> Total Effect           |   -0.055 | [-0.145,  0.034]
#> 
#> Proportion mediated: 28.14% [-181.46%, 237.75%]

mediation(m3, ci = 0.95)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.040 | [-0.129,  0.048]
#> Indirect Effect (ACME) |   -0.018 | [-0.042,  0.006]
#> Mediator Effect        |   -0.241 | [-0.296, -0.187]
#> Total Effect           |   -0.057 | [-0.151,  0.033]
#> 
#> Proportion mediated: 30.59% [-221.09%, 282.26%]
```

If you want to calculate mean instead of median values from the
posterior samples, use the `centrality`-argument. Furthermore, there is
a [`print()`](https://rdrr.io/r/base/print.html)-method, which allows to
print more digits.

``` r

m <- mediation(m2, centrality = "mean", ci = 0.95)
print(m, digits = 4)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |            95% ETI
#> ------------------------------------------------------
#> Direct Effect (ADE)    |  -0.0395 | [-0.1237,  0.0456]
#> Indirect Effect (ACME) |  -0.0158 | [-0.0405,  0.0083]
#> Mediator Effect        |  -0.2401 | [-0.2944, -0.1846]
#> Total Effect           |  -0.0553 | [-0.1454,  0.0341]
#> 
#> Proportion mediated: 28.60% [-181.01%, 238.20%]
```

As you can see, the results are similar to what the *mediation* package
produces for non-Bayesian models.

## Comparison to SEM from the lavaan package

Finally, we also compare the results to a SEM model, using *lavaan*.
This example should demonstrate how to “translate” the same model in
different packages or modeling approached.

``` r

library(lavaan)
data(jobs)
set.seed(1234)

model <- " # direct effects
             depress2 ~ c1*treat + c2*econ_hard + c3*sex + c4*age + b*job_seek

           # mediation
             job_seek ~ a1*treat + a2*econ_hard + a3*sex + a4*age

           # indirect effects (a*b)
             indirect_treat := a1*b
             indirect_econ_hard := a2*b
             indirect_sex := a3*b
             indirect_age := a4*b

           # total effects
             total_treat := c1 + (a1*b)
             total_econ_hard := c2 + (a2*b)
             total_sex := c3 + (a3*b)
             total_age := c4 + (a4*b)
         "
m4 <- sem(model, data = jobs)
summary(m4)
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        11
#> 
#>   Number of observations                           899
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   depress2 ~                                          
#>     treat     (c1)   -0.040    0.043   -0.929    0.353
#>     econ_hard (c2)    0.149    0.021    7.156    0.000
#>     sex       (c3)    0.107    0.041    2.604    0.009
#>     age       (c4)    0.001    0.002    0.332    0.740
#>     job_seek   (b)   -0.240    0.028   -8.524    0.000
#>   job_seek ~                                          
#>     treat     (a1)    0.066    0.051    1.278    0.201
#>     econ_hard (a2)    0.053    0.025    2.167    0.030
#>     sex       (a3)   -0.008    0.049   -0.157    0.875
#>     age       (a4)    0.005    0.002    1.983    0.047
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .depress2          0.373    0.018   21.201    0.000
#>    .job_seek          0.524    0.025   21.201    0.000
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     indirect_treat   -0.016    0.012   -1.264    0.206
#>     indirct_cn_hrd   -0.013    0.006   -2.100    0.036
#>     indirect_sex      0.002    0.012    0.157    0.875
#>     indirect_age     -0.001    0.001   -1.932    0.053
#>     total_treat      -0.056    0.045   -1.244    0.214
#>     total_econ_hrd    0.136    0.022    6.309    0.000
#>     total_sex         0.109    0.043    2.548    0.011
#>     total_age        -0.000    0.002   -0.223    0.824

# just to have the numbers right at hand and you don't need to scroll up
mediation(m2, ci = 0.95)
#> # Causal Mediation Analysis for Stan Model
#> 
#>   Treatment: treat
#>   Mediator : job_seek
#>   Response : depress2
#> 
#> Effect                 | Estimate |          95% ETI
#> ----------------------------------------------------
#> Direct Effect (ADE)    |   -0.040 | [-0.124,  0.046]
#> Indirect Effect (ACME) |   -0.015 | [-0.041,  0.008]
#> Mediator Effect        |   -0.240 | [-0.294, -0.185]
#> Total Effect           |   -0.055 | [-0.145,  0.034]
#> 
#> Proportion mediated: 28.14% [-181.46%, 237.75%]
```

The summary output from *lavaan* is longer, but we can find the related
numbers quite easily:

- the *direct effect* of treatment is `treat (c1)`, which is `-0.040`
- the *indirect effect* of treatment is `indirect_treat`, which is
  `-0.016`
- the *mediator effect* of job_seek is `job_seek (b)`, which is `-0.240`
- the *total effect* is `total_treat`, which is `-0.056`
