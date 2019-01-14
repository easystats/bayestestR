# bayestestR

[![HitCount](http://hits.dwyl.io/DominiqueMakowski/bayestestR.svg)](http://hits.dwyl.io/DominiqueMakowski/bayestestR)

Describe your Posterior Distributions


---


## Goal

The goal of `bayestestR` is to be a lightweight package containing utilities to describe and analyze posterior distributions and models under the Bayesian framework.


## Installation


- Run the following:

```R
install.packages("devtools")
library("devtools")
install_github("DominiqueMakowski/bayestestR")
library("bayestestR")
```


## Functions

- **`rnorm_perfect()`**: Generates a sample of size n with a near-perfect normal distribution.

```R
x <- rnorm_perfect(n = 10)
plot(density(x))
```