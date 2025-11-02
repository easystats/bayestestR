# Convert p-values to (pseudo) Bayes Factors

Convert p-values to (pseudo) Bayes Factors. This transformation has been
suggested by Wagenmakers (2022), but is based on a vast amount of
assumptions. It might therefore be not reliable. Use at your own risks.
For more accurate approximate Bayes factors, use
[`bic_to_bf()`](https://easystats.github.io/bayestestR/reference/bic_to_bf.md)
instead.

## Usage

``` r
p_to_bf(x, ...)

# S3 method for class 'numeric'
p_to_bf(x, log = FALSE, n_obs = NULL, ...)

# Default S3 method
p_to_bf(x, log = FALSE, ...)
```

## Arguments

- x:

  A (frequentist) model object, or a (numeric) vector of p-values.

- ...:

  Other arguments to be passed (not used for now).

- log:

  Wether to return log Bayes Factors. **Note:** The
  [`print()`](https://rdrr.io/r/base/print.html) method always shows
  `BF` - the `"log_BF"` column is only accessible from the returned data
  frame.

- n_obs:

  Number of observations. Either length 1, or same length as `p`.

## Value

A data frame with the p-values and pseudo-Bayes factors (against the
null).

## References

- Wagenmakers, E.J. (2022). Approximate objective Bayes factors from
  p-values and sample size: The 3p(sqrt(n)) rule. Preprint available on
  ArXiv: https://psyarxiv.com/egydq

## See also

[`bic_to_bf()`](https://easystats.github.io/bayestestR/reference/bic_to_bf.md)
for more accurate approximate Bayes factors.

## Examples

``` r
data(iris)
model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
p_to_bf(model)
#> Pseudo-BF (against NULL)
#> 
#> Parameter         |      p |       BF
#> -------------------------------------
#> (Intercept)       | < .001 | 2.71e+09
#> Sepal.Length      | < .001 | 2.43e+26
#> Speciesversicolor | < .001 | 2.82e+64
#> Speciesvirginica  | < .001 | 5.53e+68

# Examples that demonstrate comparison between
# BIC-approximated and pseudo BF
# --------------------------------------------
m0 <- lm(mpg ~ 1, mtcars)
m1 <- lm(mpg ~ am, mtcars)
m2 <- lm(mpg ~ factor(cyl), mtcars)

# In this first example, BIC-approximated BF and
# pseudo-BF based on p-values are close...

# BIC-approximated BF, m1 against null model
bic_to_bf(BIC(m1), denominator = BIC(m0))
#> [1] 222.005

# pseudo-BF based on p-values - dropping intercept
p_to_bf(m1)[-1, ]
#> Pseudo-BF (against NULL)
#> 
#> Parameter |      p |     BF
#> ---------------------------
#> am        | < .001 | 206.74

# The second example shows that results from pseudo-BF are less accurate
# and should be handled wit caution!
bic_to_bf(BIC(m2), denominator = BIC(m0))
#> [1] 45355714
p_to_bf(anova(m2), n_obs = nrow(mtcars))
#> Pseudo-BF (against NULL)
#> 
#> Parameter   |      p |       BF
#> -------------------------------
#> factor(cyl) | < .001 | 1.18e+07
```
