# Bayes Factors (BF) for Order Restricted Models

This method computes Bayes factors for comparing a model with an order
restrictions on its parameters with the fully unrestricted model. *Note
that this method should only be used for confirmatory analyses*.  
  
The `bf_*` function is an alias of the main function.  
  
**For more info, in particular on specifying correct priors for factors
with more than 2 levels, see [the Bayes factors
vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).**

## Usage

``` r
bayesfactor_restricted(posterior, ...)

bf_restricted(posterior, ...)

# S3 method for class 'stanreg'
bayesfactor_restricted(
  posterior,
  hypothesis,
  prior = NULL,
  verbose = TRUE,
  effects = "fixed",
  component = "conditional",
  ...
)

# S3 method for class 'brmsfit'
bayesfactor_restricted(
  posterior,
  hypothesis,
  prior = NULL,
  verbose = TRUE,
  effects = "fixed",
  component = "conditional",
  ...
)

# S3 method for class 'blavaan'
bayesfactor_restricted(
  posterior,
  hypothesis,
  prior = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'emmGrid'
bayesfactor_restricted(
  posterior,
  hypothesis,
  prior = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
bayesfactor_restricted(
  posterior,
  hypothesis,
  prior = NULL,
  rvar_col = NULL,
  ...
)

# S3 method for class 'bayesfactor_restricted'
as.logical(x, which = c("posterior", "prior"), ...)

# S3 method for class 'bayesfactor_restricted'
as.matrix(x, ...)
```

## Arguments

- posterior:

  A `stanreg` / `brmsfit` object, `emmGrid` or a data frame -
  representing a posterior distribution(s) from (see Details).

- ...:

  Currently not used.

- hypothesis:

  A character vector specifying the restrictions as logical conditions
  (see examples below).

- prior:

  An object representing a prior distribution (see Details).

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

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

- x:

  An object of class `bayesfactor_restricted`

- which:

  Should the logical matrix be of the posterior or prior
  distribution(s)?

## Value

A data frame containing the (log) Bayes factor representing evidence
*against* the un-restricted model (Use
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) to extract the
non-log Bayes factors; see examples). (A `bool_results` attribute
contains the results for each sample, indicating if they are included or
not in the hypothesized restriction.)  
  
For [`as.matrix()`](https://rdrr.io/r/base/matrix.html) a square matrix
of (log) Bayes factors, with rows as denominators and columns as
numerators.

## Details

This method is used to compute Bayes factors for order-restricted models
vs un-restricted models by setting an order restriction on the prior and
posterior distributions (Morey & Wagenmakers, 2013).  
  
(Though it is possible to use `bayesfactor_restricted()` to test
interval restrictions, it is more suitable for testing order
restrictions; see examples).

## Setting the correct `prior`

For the computation of Bayes factors, the model priors must be proper
priors (at the very least they should be *not flat*, and it is
preferable that they be *informative*); As the priors for the
alternative get wider, the likelihood of the null value(s) increases, to
the extreme that for completely flat priors the null is infinitely more
favorable than the alternative (this is called *the
Jeffreys-Lindley-Bartlett paradox*). Thus, you should only ever try (or
want) to compute a Bayes factor when you have an informed prior.  
  
(Note that by default,
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) uses
flat priors for fixed-effects; See example below.)  
  
It is important to provide the correct `prior` for meaningful results,
to match the `posterior`-type input:

- **A numeric vector** - `prior` should also be a *numeric vector*,
  representing the prior-estimate.

- **A data frame** - `prior` should also be a *data frame*, representing
  the prior-estimates, in matching column order.

  - If `rvar_col` is specified, `prior` should be *the name of an `rvar`
    column* that represents the prior-estimates.

- **Supported Bayesian model (`stanreg`, `brmsfit`, etc.)**

  - `prior` should be *a model an equivalent model with MCMC samples
    from the priors **only***. See
    [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md).

  - If `prior` is set to `NULL`,
    [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)
    is called internally (not supported for `brmsfit_multiple` model).

- **Output from a `{marginaleffects}` function** - `prior` should also
  be *an equivalent output* from a `{marginaleffects}` function based on
  a prior-model (See
  [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)).

- **Output from an `{emmeans}` function**

  - `prior` should also be *an equivalent output* from an `{emmeans}`
    function based on a prior-model (See
    [`unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.md)).

  - `prior` can also be *the original (posterior) model*, in which case
    the function will try to "unupdate" the estimates (not supported if
    the estimates have undergone any transformations – `"log"`,
    `"response"`, etc. – or any `regrid`ing).

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

- Morey, R. D., & Wagenmakers, E. J. (2014). Simple relation between
  Bayesian order-restricted and point-null hypothesis tests. Statistics
  & Probability Letters, 92, 121-124.

- Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for
  testing interval null hypotheses. Psychological methods, 16(4), 406.

- Morey, R. D. (Jan, 2015). Multiple Comparisons with BayesFactor, Part
  2 – order restrictions. Retrieved from
  https://richarddmorey.org/category/order-restrictions/.

## Examples

``` r
set.seed(444)
library(bayestestR)
prior <- data.frame(
  A = rnorm(500),
  B = rnorm(500),
  C = rnorm(500)
)

posterior <- data.frame(
  A = rnorm(500, .4, 0.7),
  B = rnorm(500, -.2, 0.4),
  C = rnorm(500, 0, 0.5)
)

hyps <- c(
  "A > B & B > C",
  "A > B & A > C",
  "C > A"
)


(b <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior))
#> Bayes Factor (Order-Restriction)
#> 
#> Hypothesis    P(Prior) P(Posterior)    BF
#> A > B & B > C     0.16         0.23  1.39
#> A > B & A > C     0.36         0.59  1.61
#> C > A             0.46         0.34 0.742
#> 
#> * Bayes factors for the restricted model vs. the un-restricted model.

# See the matrix of BFs
as.matrix(b)
#> # Bayes Factors for Restricted Models
#> 
#>  Denominator\Numerator |   [1] |   [2] |   [3] |   [4]
#> -----------------------------------------------------
#> [1] (Un-restricted)    |     1 |  1.39 |  1.61 | 0.742
#> [2] A > B & B > C      | 0.719 |     1 |  1.16 | 0.534
#> [3] A > B & A > C      | 0.621 | 0.864 |     1 | 0.461
#> [4] C > A              |  1.35 |  1.87 |  2.17 |     1

bool <- as.logical(b, which = "posterior")
head(bool)
#>      A > B & B > C A > B & A > C C > A
#> [1,]          TRUE          TRUE FALSE
#> [2,]          TRUE          TRUE FALSE
#> [3,]          TRUE          TRUE FALSE
#> [4,]         FALSE          TRUE FALSE
#> [5,]         FALSE         FALSE  TRUE
#> [6,]         FALSE          TRUE FALSE


see::plots(
  plot(estimate_density(posterior)),
  # distribution **conditional** on the restrictions
  plot(estimate_density(posterior[bool[, hyps[1]], ])) + ggplot2::ggtitle(hyps[1]),
  plot(estimate_density(posterior[bool[, hyps[2]], ])) + ggplot2::ggtitle(hyps[2]),
  plot(estimate_density(posterior[bool[, hyps[3]], ])) + ggplot2::ggtitle(hyps[3]),
  guides = "collect"
)

# \donttest{
# rstanarm models
# ---------------
data("mtcars")

fit_stan <- rstanarm::stan_glm(mpg ~ wt + cyl + am,
  data = mtcars, refresh = 0
)
hyps <- c(
  "am > 0 & cyl < 0",
  "cyl < 0",
  "wt - cyl > 0"
)

bayesfactor_restricted(fit_stan, hypothesis = hyps)
#> Sampling priors, please wait...
#> Bayes Factor (Order-Restriction)
#> 
#> Hypothesis       P(Prior) P(Posterior)    BF
#> am > 0 & cyl < 0     0.25         0.56  2.25
#> cyl < 0              0.50         1.00  1.99
#> wt - cyl > 0         0.50         0.10 0.197
#> 
#> * Bayes factors for the restricted model vs. the un-restricted model.
# }
# \donttest{
# emmGrid objects
# ---------------
# replicating http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
data("disgust")
contrasts(disgust$condition) <- contr.equalprior_pairs # see vignette
fit_model <- rstanarm::stan_glm(score ~ condition, data = disgust, family = gaussian())
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.22 seconds.
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
#> Chain 1:  Elapsed Time: 0.03 seconds (Warm-up)
#> Chain 1:                0.038 seconds (Sampling)
#> Chain 1:                0.068 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 9e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
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
#> Chain 2:  Elapsed Time: 0.031 seconds (Warm-up)
#> Chain 2:                0.039 seconds (Sampling)
#> Chain 2:                0.07 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.03 seconds (Warm-up)
#> Chain 3:                0.038 seconds (Sampling)
#> Chain 3:                0.068 seconds (Total)
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
#> Chain 4:                0.039 seconds (Sampling)
#> Chain 4:                0.068 seconds (Total)
#> Chain 4: 

em_condition <- emmeans::emmeans(fit_model, ~condition, data = disgust)
hyps <- c("lemon < control & control < sulfur")

bayesfactor_restricted(em_condition, prior = fit_model, hypothesis = hyps)
#> Sampling priors, please wait...
#> Bayes Factor (Order-Restriction)
#> 
#> Hypothesis                         P(Prior) P(Posterior)   BF
#> lemon < control & control < sulfur     0.17         0.75 4.28
#> 
#> * Bayes factors for the restricted model vs. the un-restricted model.
# > # Bayes Factor (Order-Restriction)
# >
# >                          Hypothesis P(Prior) P(Posterior)   BF
# >  lemon < control & control < sulfur     0.17         0.75 4.49
# > ---
# > Bayes factors for the restricted model vs. the un-restricted model.
# }
```
