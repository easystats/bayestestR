# Convert model's posteriors to priors (EXPERIMENTAL)

Convert model's posteriors to (normal) priors.

## Usage

``` r
model_to_priors(model, scale_multiply = 3, ...)
```

## Arguments

- model:

  A Bayesian model.

- scale_multiply:

  The SD of the posterior will be multiplied by this amount before being
  set as a prior to avoid overly narrow priors.

- ...:

  Other arguments for `insight::get_prior()` or
  [`describe_posterior`](https://easystats.github.io/bayestestR/reference/describe_posterior.md).

## Examples

``` r
# \donttest{
# brms models
# -----------------------------------------------
if (require("brms")) {
  formula <- brms::brmsformula(mpg ~ wt + cyl, center = FALSE)

  model <- brms::brm(formula, data = mtcars, refresh = 0)
  priors <- model_to_priors(model)
  priors <- brms::validate_prior(priors, formula, data = mtcars)
  priors

  model2 <- brms::brm(formula, data = mtcars, prior = priors, refresh = 0)
}
#> Compiling Stan program...
#> Start sampling
#> Compiling Stan program...
#> Start sampling
# }
```
