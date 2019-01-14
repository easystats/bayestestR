# bayestestR

[![HitCount](http://hits.dwyl.io/DominiqueMakowski/bayestestR.svg)](http://hits.dwyl.io/DominiqueMakowski/bayestestR)

Describe your Posterior Distributions


---


## Goal

`bayestestR` is a lightweight package providing utilities to describe posterior distributions and Bayesian models.


## Installation


Run the following:

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