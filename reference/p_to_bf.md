# Convert p-values to (pseudo) Bayes Factors

Convert p-values to (pseudo) Bayes Factors. This transformation has been
suggested by Aust, Pawel, and Wagenmakers (2026), but is based on a vast
amount of assumptions. It might therefore be not reliable. Use at your
own risks.

## Usage

``` r
p_to_bf(x, ...)

# S3 method for class 'numeric'
p_to_bf(x, n_obs = NULL, log = FALSE, ...)

# Default S3 method
p_to_bf(x, n_obs = NULL, log = FALSE, ...)
```

## Arguments

- x:

  A (frequentist) model object, or a (numeric) vector of p-values.
  p-values must come from a *two-tailed* *z*- or *t*-test or from an
  *F*- or a \\\chi^2\\-test with a single degree of freedom. p-values
  must not be corrected for multiple testing.

- ...:

  Arguments passed to
  [`parameters::p_value()`](https://easystats.github.io/parameters/reference/p_value.html)
  if `x` is a model object.

- n_obs:

  (Effective) number of observations. For mixed models this must be
  supplied and generally corresponds to the number of clusters. Either
  length 1, or same length as `p`.

- log:

  Whether to return log Bayes Factors. **Note:** The
  [`print()`](https://rdrr.io/r/base/print.html) method always shows
  `BF` - the `"log_BF"` column is only accessible from the returned data
  frame.

## Value

A data frame with the p-values and pseudo-Bayes factors (against the
null).

## References

- Aust, F., Pawel, S., & Wagenmakers, E.J. (2026). Extracting Bayesian
  Evidence from Frequentist p-Values. Preprint available on ArXiv:
  https://arxiv.org/abs/2607.12132

## See also

[`bic_to_bf()`](https://easystats.github.io/bayestestR/reference/bic_to_bf.md)
for approximate Bayes factors based on BICs.

## Examples

``` r

# Compare to BIC-approximated and pseudo BF
# --------------------------------------------
m0 <- lm(mpg ~ 1, data = mtcars)
m1 <- lm(mpg ~ am, data = mtcars)
m2 <- lm(mpg ~ factor(cyl), data = mtcars)

# BIC-approximated BF, m1 against null model
bayesfactor_models(m1, denominator = m0)
#> Bayes Factors for Model Comparison
#> 
#>     Model     BF
#> [1] am    222.01
#> 
#> * Against Denominator: [2] (Intercept only)
#> *   Bayes Factor Type: BIC approximation
# bic_to_bf(BIC(m1), denominator = BIC(m0)) # equivalent

# pseudo-BF based on p-values
p_to_bf(m1)[-1, ] # dropping intercept
#> Pseudo-BF (against NULL)
#> 
#> Parameter |      p |     BF
#> ---------------------------
#> am        | < .001 | 206.74

# When using a p-value from an F/chisq-test with more than one degree of
# freedom, the pseudo-BF is not reliable:
bayesfactor_models(m2, denominator = m0)
#> Bayes Factors for Model Comparison
#> 
#>     Model             BF
#> [1] factor(cyl) 4.54e+07
#> 
#> * Against Denominator: [2] (Intercept only)
#> *   Bayes Factor Type: BIC approximation
p_to_bf(anova(m2), n_obs = nrow(mtcars))
#> Pseudo-BF (against NULL)
#> 
#> Parameter   |      p |       BF
#> -------------------------------
#> factor(cyl) | < .001 | 1.18e+07

# Mixed models
# ------------------

data("sleepstudy", package = "lme4")
mixed0 <- lmerTest::lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
mixed1 <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

bayesfactor_models(mixed1, denominator = mixed0)
#> Bayes Factors for Model Comparison
#> 
#>     Model                         BF
#> [1] Days + (Days | Subject) 9.52e+03
#> 
#> * Against Denominator: [2] 1 + (Days | Subject)
#> *   Bayes Factor Type: BIC approximation

p_to_bf(
  mixed1,
  n_obs = nlevels(lme4::sleepstudy$Subject), # *effective* sample size here
  method = "S" # make sure to get the correct p-values for the fixed effects
)[-1, ] # dropping intercept
#> Pseudo-BF (against NULL)
#> 
#> Parameter |      p |       BF
#> -----------------------------
#> Days      | < .001 | 2.41e+04
```
