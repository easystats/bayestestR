# Sequential Effect eXistence and sIgnificance Testing (SEXIT)

The SEXIT is a new framework to describe Bayesian effects, guiding which
indices to use. Accordingly, the `sexit()` function returns the minimal
(and optimal) required information to describe models' parameters under
a Bayesian framework. It includes the following indices:

- Centrality: the median of the posterior distribution. In probabilistic
  terms, there is `50%` of probability that the effect is higher and
  lower. See
  [`point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.md).

- Uncertainty: the `95%` Highest Density Interval (HDI). In
  probabilistic terms, there is `95%` of probability that the effect is
  within this confidence interval. See
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.md).

- Existence: The probability of direction allows to quantify the
  certainty by which an effect is positive or negative. It is a critical
  index to show that an effect of some manipulation is not harmful (for
  instance in clinical studies) or to assess the direction of a link.
  See
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

- Significance: Once existence is demonstrated with high certainty, we
  can assess whether the effect is of sufficient size to be considered
  as significant (i.e., not negligible). This is a useful index to
  determine which effects are actually important and worthy of
  discussion in a given process. See
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.md).

- Size: Finally, this index gives an idea about the strength of an
  effect. However, beware, as studies have shown that a big effect size
  can be also suggestive of low statistical power (see details section).

## Usage

``` r
sexit(x, significant = "default", large = "default", ci = 0.95, ...)
```

## Arguments

- x:

  A vector representing a posterior distribution, a data frame of
  posterior draws (samples be parameter). Can also be a Bayesian model.

- significant, large:

  The threshold values to use for significant and large probabilities.
  If left to 'default', will be selected through
  [`sexit_thresholds()`](https://easystats.github.io/bayestestR/reference/sexit_thresholds.md).
  See the details section below.

- ci:

  Value or vector of probability of the (credible) interval - CI
  (between 0 and 1) to be estimated. Default to `.95` (`95%`).

- ...:

  Currently not used.

## Value

A dataframe and text as attribute.

## Details

### Rationale

The assessment of "significance" (in its broadest meaning) is a
pervasive issue in science, and its historical index, the p-value, has
been strongly criticized and deemed to have played an important role in
the replicability crisis. In reaction, more and more scientists have
tuned to Bayesian methods, offering an alternative set of tools to
answer their questions. However, the Bayesian framework offers a wide
variety of possible indices related to "significance", and the debate
has been raging about which index is the best, and which one to report.

This situation can lead to the mindless reporting of all possible
indices (with the hopes that with that the reader will be satisfied),
but often without having the writer understanding and interpreting them.
It is indeed complicated to juggle between many indices with complicated
definitions and subtle differences.

SEXIT aims at offering a practical framework for Bayesian effects
reporting, in which the focus is put on intuitiveness, explicitness and
usefulness of the indices' interpretation. To that end, we suggest a
system of description of parameters that would be intuitive, easy to
learn and apply, mathematically accurate and useful for taking decision.

Once the thresholds for significance (i.e., the ROPE) and the one for a
"large" effect are explicitly defined, the SEXIT framework does not make
any interpretation, i.e., it does not label the effects, but just
sequentially gives 3 probabilities (of direction, of significance and of
being large, respectively) as-is on top of the characteristics of the
posterior (using the median and HDI for centrality and uncertainty
description). Thus, it provides a lot of information about the posterior
distribution (through the mass of different 'sections' of the posterior)
in a clear and meaningful way.

### Threshold selection

One of the most important thing about the SEXIT framework is that it
relies on two "arbitrary" thresholds (i.e., that have no absolute
meaning). They are the ones related to effect size (an inherently
subjective notion), namely the thresholds for significant and large
effects. They are set, by default, to `0.05` and `0.3` of the standard
deviation of the outcome variable (tiny and large effect sizes for
correlations according to Funder and Ozer, 2019). However, these
defaults were chosen by lack of a better option, and might not be
adapted to your case. Thus, they are to be handled with care, and the
chosen thresholds should always be explicitly reported and justified.

- For **linear models (lm)**, this can be generalised to 0.05 \* SD_(y)
  and 0.3 \* SD_(y) for significant and large effects, respectively.

- For **logistic models**, the parameters expressed in log odds ratio
  can be converted to standardized difference through the formula
  π/√(3), resulting a threshold of `0.09` and `0.54`.

- For other models with **binary outcome**, it is strongly recommended
  to manually specify the rope argument. Currently, the same default is
  applied that for logistic models.

- For models from **count data**, the residual variance is used. This is
  a rather experimental threshold and is probably often similar to
  `0.05` and `0.3`, but should be used with care!

- For **t-tests**, the standard deviation of the response is used,
  similarly to linear models (see above).

- For **correlations**,`0.05` and `0.3` are used.

- For all other models, `0.05` and `0.3` are used, but it is strongly
  advised to specify it manually.

### Examples

The three values for existence, significance and size provide a useful
description of the posterior distribution of the effects. Some possible
scenarios include:

- The probability of existence is low, but the probability of being
  large is high: it suggests that the posterior is very wide (covering
  large territories on both side of 0). The statistical power might be
  too low, which should warrant any confident conclusion.

- The probability of existence and significance is high, but the
  probability of being large is very small: it suggests that the effect
  is, with high confidence, not large (the posterior is mostly contained
  between the significance and the large thresholds).

- The 3 indices are very low: this suggests that the effect is null with
  high confidence (the posterior is closely centred around 0).

## References

- Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019). bayestestR:
  Describing Effects and their Uncertainty, Existence and Significance
  within the Bayesian Framework. Journal of Open Source Software,
  4(40), 1541.
  [doi:10.21105/joss.01541](https://doi.org/10.21105/joss.01541)

- Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of
  Effect Existence and Significance in the Bayesian Framework. Frontiers
  in Psychology 2019;10:2767.
  [doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

## Examples

``` r
# \donttest{
library(bayestestR)

s <- sexit(rnorm(1000, -1, 1))
s
#> # Following the Sequential Effect eXistence and sIgnificance Testing (SEXIT) framework, we report the median of the posterior distribution and its 95% CI (Highest Density Interval), along the probability of direction (pd), the probability of significance and the probability of being large. The thresholds beyond which the effect is considered as significant (i.e., non-negligible) and large are |0.05| and |0.30|.
#> 
#> The effect (Median = -0.96, 95% CI [-2.80, 1.08]) has a 84.70% probability of being negative (< 0), 83.50% of being significant (< -0.05), and 76.70% of being large (< -0.30)
#> 
#> Median |        95% CI | Direction | Significance (> |0.05|) | Large (> |0.30|)
#> -------------------------------------------------------------------------------
#>  -0.96 | [-2.80, 1.08] |      0.85 |                    0.83 |             0.77
#> 
print(s, summary = TRUE)
#> # The thresholds beyond which the effect is considered as significant (i.e., non-negligible) and large are |0.05| and |0.30|.
#> 
#> The effect (Median = -0.96, 95% CI [-2.80, 1.08]) has 84.70%, 83.50% and 76.70% probability of being negative (< 0), significant (< -0.05) and large (< -0.30)

s <- sexit(iris)
s
#> # Following the Sequential Effect eXistence and sIgnificance Testing (SEXIT) framework, we report the median of the posterior distribution and its 95% CI (Highest Density Interval), along the probability of direction (pd), the probability of significance and the probability of being large. The thresholds beyond which the effect is considered as significant (i.e., non-negligible) and large are |0.05| and |0.30|.
#> 
#> - Sepal.Length (Median = 5.80, 95% CI [4.47, 7.70]) has a 100.00% probability of being positive (> 0), 100.00% of being significant (> 0.05), and 100.00% of being large (> 0.30)
#> - Sepal.Width (Median = 3.00, 95% CI [2.27, 3.93]) has a 100.00% probability of being positive (> 0), 100.00% of being significant (> 0.05), and 100.00% of being large (> 0.30)
#> - Petal.Length (Median = 4.35, 95% CI [1.27, 6.46]) has a 100.00% probability of being positive (> 0), 100.00% of being significant (> 0.05), and 100.00% of being large (> 0.30)
#> - Petal.Width (Median = 1.30, 95% CI [0.10, 2.40]) has a 100.00% probability of being positive (> 0), 100.00% of being significant (> 0.05), and 72.67% of being large (> 0.30)
#> 
#> Parameter    | Median |       95% CI | Direction | Significance (> |0.05|) | Large (> |0.30|)
#> ---------------------------------------------------------------------------------------------
#> Sepal.Length |   5.80 | [4.47, 7.70] |         1 |                       1 |             1.00
#> Sepal.Width  |   3.00 | [2.27, 3.93] |         1 |                       1 |             1.00
#> Petal.Length |   4.35 | [1.27, 6.46] |         1 |                       1 |             1.00
#> Petal.Width  |   1.30 | [0.10, 2.40] |         1 |                       1 |             0.73
#> 
print(s, summary = TRUE)
#> # The thresholds beyond which the effect is considered as significant (i.e., non-negligible) and large are |0.05| and |0.30|.
#> 
#> - Sepal.Length (Median = 5.80, 95% CI [4.47, 7.70]) has 100.00%, 100.00% and 100.00% probability of being positive (> 0), significant (> 0.05) and large (> 0.30)
#> - Sepal.Width (Median = 3.00, 95% CI [2.27, 3.93]) has 100.00%, 100.00% and 100.00% probability of being positive (> 0), significant (> 0.05) and large (> 0.30)
#> - Petal.Length (Median = 4.35, 95% CI [1.27, 6.46]) has 100.00%, 100.00% and 100.00% probability of being positive (> 0), significant (> 0.05) and large (> 0.30)
#> - Petal.Width (Median = 1.30, 95% CI [0.10, 2.40]) has 100.00%, 100.00% and 72.67% probability of being positive (> 0), significant (> 0.05) and large (> 0.30)

if (require("rstanarm")) {
  model <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt * cyl,
    data = mtcars,
    iter = 400, refresh = 0
  ))
  s <- sexit(model)
  s
  print(s, summary = TRUE)
}
#> # The thresholds beyond which the effect is considered as significant (i.e., non-negligible) and large are |0.30| and |1.81| (corresponding respectively to 0.05 and 0.30 of the outcome's SD).
#> 
#> - (Intercept) (Median = 53.30, 95% CI [40.76, 65.33]) has 100.00%, 100.00% and 100.00% probability of being positive (> 0), significant (> 0.30) and large (> 1.81)
#> - wt (Median = -8.24, 95% CI [-12.74, -3.47]) has 100.00%, 100.00% and 99.62% probability of being negative (< 0), significant (< -0.30) and large (< -1.81)
#> - cyl (Median = -3.62, 95% CI [-5.50, -1.57]) has 99.88%, 99.62% and 96.25% probability of being negative (< 0), significant (< -0.30) and large (< -1.81)
#> - wt:cyl (Median = 0.75, 95% CI [0.09, 1.38]) has 98.25%, 91.25% and 0.12% probability of being positive (> 0), significant (> 0.30) and large (> 1.81)
# }
```
