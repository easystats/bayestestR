# Shortest Probability Interval (SPI)

Compute the **Shortest Probability Interval (SPI)** of posterior
distributions. The SPI is a more computationally stable HDI. The
implementation is based on the algorithm from the **SPIn** package.

## Usage

``` r
spi(x, ...)

# S3 method for class 'numeric'
spi(x, ci = 0.95, verbose = TRUE, ...)

# S3 method for class 'data.frame'
spi(x, ci = 0.95, rvar_col = NULL, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
spi(
  x,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'get_predicted'
spi(x, ci = 0.95, use_iterations = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  Vector representing a posterior distribution, or a data frame of such
  vectors. Can also be a Bayesian model. **bayestestR** supports a wide
  range of models (see, for example, `methods("hdi")`) and not all of
  those are documented in the 'Usage' section, because methods for other
  classes mostly resemble the arguments of the `.numeric` or
  `.data.frame`methods.

- ...:

  Currently not used.

- ci:

  Value or vector of probability of the (credible) interval - CI
  (between 0 and 1) to be estimated. Default to `.95` (`95%`).

- verbose:

  Toggle off warnings.

- rvar_col:

  A single character - the name of an `rvar` column in the data frame to
  be processed. See example in
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.md).

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

- parameters:

  Regular expression pattern that describes the parameters that should
  be returned. Meta-parameters (like `lp__` or `prior_`) are filtered by
  default, so only parameters that typically appear in the
  [`summary()`](https://rdrr.io/r/base/summary.html) are returned. Use
  `parameters` to select specific parameters for the output.

- use_iterations:

  Logical, if `TRUE` and `x` is a `get_predicted` object, (returned by
  [`insight::get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html)),
  the function is applied to the iterations instead of the predictions.
  This only applies to models that return iterations for predicted
  values (e.g., `brmsfit` models).

## Value

A data frame with following columns:

- `Parameter` The model parameter(s), if `x` is a model-object. If `x`
  is a vector, this column is missing.

- `CI` The probability of the credible interval.

- `CI_low`, `CI_high` The lower and upper credible interval limits for
  the parameters.

## Details

The SPI is an alternative method to the HDI
([`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md)) to
quantify uncertainty of (posterior) distributions. The SPI is said to be
more stable than the HDI, because, the *"HDI can be noisy (that is, have
a high Monte Carlo error)"* (Liu et al. 2015). Furthermore, the HDI is
sensitive to additional assumptions, in particular assumptions related
to the different estimation methods, which can make the HDI less
accurate or reliable.

## Note

The code to compute the SPI was adapted from the **SPIn** package, and
slightly modified to be more robust for Stan models. Thus, credits go to
Ying Liu for the original SPI algorithm and R implementation.

## References

Liu, Y., Gelman, A., & Zheng, T. (2015). Simulation-efficient shortest
probability intervals. Statistics and Computing, 25(4), 809–819.
https://doi.org/10.1007/s11222-015-9563-8

## See also

Other ci:
[`bci()`](https://easystats.github.io/bayestestR/reference/bci.md),
[`ci()`](https://easystats.github.io/bayestestR/reference/ci.md),
[`eti()`](https://easystats.github.io/bayestestR/reference/eti.md),
[`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.md),
[`si()`](https://easystats.github.io/bayestestR/reference/si.md)

## Examples

``` r
library(bayestestR)

posterior <- rnorm(1000)
spi(posterior)
#> 95% SPI: [-1.96, 1.96]
spi(posterior, ci = c(0.80, 0.89, 0.95))
#> Shortest Probability Interval
#> 
#> 80% SPI       |       89% SPI |       95% SPI
#> ---------------------------------------------
#> [-1.17, 1.34] | [-1.50, 1.70] | [-1.96, 1.96]

df <- data.frame(replicate(4, rnorm(100)))
spi(df)
#> Shortest Probability Interval
#> 
#> Parameter |       95% SPI
#> -------------------------
#> X1        | [-2.04, 1.89]
#> X2        | [-1.64, 1.96]
#> X3        | [-2.10, 1.73]
#> X4        | [-1.89, 2.38]
spi(df, ci = c(0.80, 0.89, 0.95))
#> Shortest Probability Interval
#> 
#> Parameter |       80% SPI |       89% SPI |       95% SPI
#> ---------------------------------------------------------
#> X1        | [-1.05, 1.44] | [-1.43, 1.76] | [-2.04, 1.89]
#> X2        | [-0.90, 1.17] | [-1.37, 1.41] | [-1.64, 1.96]
#> X3        | [-1.41, 1.42] | [-1.98, 1.42] | [-2.10, 1.73]
#> X4        | [-1.27, 1.37] | [-2.00, 1.37] | [-1.89, 2.38]
# \donttest{
library(rstanarm)
model <- suppressWarnings(
  stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
)
spi(model)
#> Shortest Probability Interval 
#> 
#> Parameter   |        95% SPI
#> ----------------------------
#> (Intercept) | [28.02, 47.24]
#> wt          | [-6.77, -4.16]
#> gear        | [-2.30,  1.58]
# }
```
