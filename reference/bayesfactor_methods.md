# Methods for Bayes factors

Methods for Bayes factors

## Usage

``` r
# S3 method for class 'bayestestRBF'
as.matrix(x, log = TRUE, ...)

# S3 method for class 'bayesfactor_models'
update(object, subset = NULL, reference = NULL, ...)

# S3 method for class 'bayestestRBF'
as.numeric(x, log = FALSE, ...)

# S3 method for class 'bayesfactor_restricted'
as.logical(x, which = c("posterior", "prior"), ...)
```

## Arguments

- x, object:

  Bayes factor object

- log:

  Return log(BF) (default), or BF values.

- ...:

  Additional arguments (currently not used).

- subset:

  Vector of model indices to keep or remove.

- reference:

  Index of model to reference to, or `"top"` to reference to the best
  model, or `"bottom"` to reference to the worst model.

- which:

  Should the logical matrix be of the posterior or prior
  distribution(s)?

## Value

- [`as.numeric()`](https://rdrr.io/r/base/numeric.html) /
  [`as.double()`](https://rdrr.io/r/base/double.html): a numeric vector
  of (log) Bayes factors.

- [`as.logical()`](https://rdrr.io/r/base/logical.html): a logical data
  frame with a column for each order-restricted hypothesis.

- [`as.matrix()`](https://rdrr.io/r/base/matrix.html): a square matrix
  of (log) Bayes factors, with rows as denominators and columns as
  numerators.

- [`update()`](https://rdrr.io/r/stats/update.html): an updated
  `bayesfactor_models` object.

## Interpreting Bayes Factors

A Bayes factor greater than 1 can be interpreted as evidence against the
null, at which one convention is that a Bayes factor greater than 3 can
be considered as "substantial" evidence against the null (and vice
versa, a Bayes factor smaller than 1/3 indicates substantial evidence in
favor of the null-model). See also
[`effectsize::interpret_bf()`](https://easystats.github.io/effectsize/reference/interpret_bf.html).

## Transitivity of Bayes factors

For multiple inputs (models or hypotheses), the function will return
multiple Bayes factors between each model and *the same* reference model
(the `denominator` or un-restricted model). However, we can take
advantage of the transitivity of Bayes factors - where if we have two
Bayes factors for Model *A* and model *B* against the *same reference
model C*, we can obtain a Bayes factor for comparing model *A* to model
*B* by dividing them:\
\
\$\$BF\_{AB} = \frac{BF\_{AC}}{BF\_{BC}} =
\frac{\frac{ML\_{A}}{ML\_{C}}}{\frac{ML\_{B}}{ML\_{C}}} =
\frac{ML\_{A}}{ML\_{B}}\$\$\
\
(Where *ML* is the *marginal likelihood*.)\
\
A full matrix comparing all models can be obtained with
[`as.matrix()`](https://rdrr.io/r/base/matrix.html).

## Prior and posterior considerations

In order to correctly and precisely estimate Bayes factors, a rule of
thumb are the 4 P's: **P**roper **P**riors and **P**lentiful
**P**osteriors.\
\
For the computation of Bayes factors, the model priors must be proper
priors (at the very least they should be *not flat*, and it is
preferable that they be *informative*) (Note that by default,
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) uses
flat priors for fixed-effects); Wide priors result in smaller marginal
likelihoods, and thus models with wider priors are trivially less likely
than models with narrower priors - where, at the extreme, that a model
with completely flat priors is infinitely less favorable than a point
null model (this is called *the Jeffreys-Lindley-Bartlett paradox*).
Thus, you should only ever try (or want) to compute a Bayes factor when
you have an informed prior.\
\
Additionally, for models using MCMC estimation the number of posterior
samples needed for testing is substantially larger than for estimation
(the default of 4000 samples may not be enough in many cases). A
conservative rule of thumb is to obtain 10 times more samples than would
be required for estimation (*Gronau, Singmann, & Wagenmakers, 2017*). If
less than 40,000 samples are detected, a warning is issued.
