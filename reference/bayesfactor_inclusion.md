# Inclusion Bayes Factors for testing predictors across Bayesian models

The `bf_*` function is an alias of the main function. For more info, see
[the Bayes factors
vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).

## Usage

``` r
bayesfactor_inclusion(models, match_models = FALSE, prior_odds = NULL, ...)

bf_inclusion(models, match_models = FALSE, prior_odds = NULL, ...)
```

## Arguments

- models:

  An object of class
  [`bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.md)
  or `BFBayesFactor`.

- match_models:

  See details.

- prior_odds:

  Optional vector of prior odds for the models. See
  `BayesFactor::priorOdds<-`.

- ...:

  Arguments passed to or from other methods.

## Value

a data frame containing the prior and posterior probabilities, and
log(BF) for each effect (Use
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) to extract the
non-log Bayes factors; see examples).

## Details

Inclusion Bayes factors answer the question: Are the observed data more
probable under models with a particular effect, than they are under
models without that particular effect? In other words, on average - are
models with effect \\X\\ more likely to have produced the observed data
than models without effect \\X\\?

### Match Models

If `match_models=FALSE` (default), Inclusion BFs are computed by
comparing all models with a term against all models without that term.
If `TRUE`, comparison is restricted to models that (1) do not include
any interactions with the term of interest; (2) for interaction terms,
averaging is done only across models that containe the main effect terms
from which the interaction term is comprised.

## Note

Random effects in the `lmer` style are converted to interaction terms:
i.e., `(X|G)` will become the terms `1:G` and `X:G`.

## Interpreting Bayes Factors

A Bayes factor greater than 1 can be interpreted as evidence against the
null, at which one convention is that a Bayes factor greater than 3 can
be considered as "substantial" evidence against the null (and vice
versa, a Bayes factor smaller than 1/3 indicates substantial evidence in
favor of the null-model) (Wetzels et al. 2011).

## References

- Hinne, M., Gronau, Q. F., van den Bergh, D., and Wagenmakers, E.
  (2019, March 25). A conceptual introduction to Bayesian Model
  Averaging.
  [doi:10.31234/osf.io/wgb64](https://doi.org/10.31234/osf.io/wgb64)

- Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive
  sampling for variable selection and model averaging. Journal of
  Computational and Graphical Statistics, 20(1), 80-101.

- Mathot, S. (2017). Bayes like a Baws: Interpreting Bayesian Repeated
  Measures in JASP. [Blog
  post](https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp).

## See also

[`weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.md)
for Bayesian parameter averaging.

## Author

Mattan S. Ben-Shachar

## Examples

``` r
library(bayestestR)

# Using bayesfactor_models:
# ------------------------------
mo0 <- lm(Sepal.Length ~ 1, data = iris)
mo1 <- lm(Sepal.Length ~ Species, data = iris)
mo2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
mo3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)

BFmodels <- bayesfactor_models(mo1, mo2, mo3, denominator = mo0)
(bf_inc <- bayesfactor_inclusion(BFmodels))
#> Inclusion Bayes Factors (Model Averaged)
#> 
#>                      P(prior) P(posterior) Inclusion BF
#> Species                  0.75         1.00     2.02e+55
#> Petal.Length             0.50         1.00     3.58e+26
#> Petal.Length:Species     0.25         0.04        0.113
#> 
#> * Compared among: all models
#> *    Priors odds: uniform-equal

as.numeric(bf_inc)
#> [1] 2.021143e+55 3.575448e+26 1.131202e-01

# \donttest{
# BayesFactor
# -------------------------------
BF <- BayesFactor::generalTestBF(len ~ supp * dose, ToothGrowth, progress = FALSE)
bayesfactor_inclusion(BF)
#> Inclusion Bayes Factors (Model Averaged)
#> 
#>           P(prior) P(posterior) Inclusion BF
#> supp          0.60         0.98        37.31
#> dose          0.60         1.00     6.11e+12
#> dose:supp     0.20         0.58         5.63
#> 
#> * Compared among: all models
#> *    Priors odds: uniform-equal

# compare only matched models:
bayesfactor_inclusion(BF, match_models = TRUE)
#> Inclusion Bayes Factors (Model Averaged)
#> 
#>           P(prior) P(posterior) Inclusion BF
#> supp          0.40         0.40        22.65
#> dose          0.40         0.42     3.81e+12
#> dose:supp     0.20         0.58         1.47
#> 
#> * Compared among: matched models only
#> *    Priors odds: uniform-equal
# }
```
