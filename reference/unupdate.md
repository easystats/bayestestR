# Un-update Bayesian models to their prior-to-data state

As posteriors are priors that have been updated after observing some
data, the goal of this function is to un-update the posteriors to obtain
models representing the priors. These models can then be used to examine
the prior predictive distribution, or to compare priors with posteriors.

## Usage

``` r
unupdate(model, verbose = TRUE, ...)

# S3 method for class 'brmsfit'
unupdate(model, verbose = TRUE, ...)

# S3 method for class 'brmsfit_multiple'
unupdate(model, verbose = TRUE, newdata = NULL, ...)
```

## Arguments

- model:

  A fitted Bayesian model.

- verbose:

  Toggle warnings.

- ...:

  Not used

- newdata:

  List of `data.frames` to update the model with new data. Required even
  if the original data should be used.

## Value

A model un-fitted to the data, representing the prior model.

## Details

This function in used internally to compute Bayes factors.
