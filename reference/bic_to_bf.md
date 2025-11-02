# Convert BIC indices to Bayes Factors via the BIC-approximation method.

The difference between two Bayesian information criterion (BIC) indices
of two models can be used to approximate Bayes factors via:  
\$\$BF\_{10} = e^{(BIC_0 - BIC_1)/2}\$\$

## Usage

``` r
bic_to_bf(bic, denominator, log = FALSE)
```

## Arguments

- bic:

  A vector of BIC values.

- denominator:

  The BIC value to use as a denominator (to test against).

- log:

  If `TRUE`, return the `log(BF)`.

## Value

The Bayes Factors corresponding to the BIC values against the
denominator.

## References

Wagenmakers, E. J. (2007). A practical solution to the pervasive
problems of p values. Psychonomic bulletin & review, 14(5), 779-804

## Examples

``` r
bic1 <- BIC(lm(Sepal.Length ~ 1, data = iris))
bic2 <- BIC(lm(Sepal.Length ~ Species, data = iris))
bic3 <- BIC(lm(Sepal.Length ~ Species + Petal.Length, data = iris))
bic4 <- BIC(lm(Sepal.Length ~ Species * Petal.Length, data = iris))

bic_to_bf(c(bic1, bic2, bic3, bic4), denominator = bic1)
#> [1] 1.000000e+00 1.695852e+29 5.843105e+55 2.203243e+54
```
