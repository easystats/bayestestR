# Density Estimation

This function is a wrapper over different methods of density estimation.
By default, it uses the base R `density` with by default uses a
different smoothing bandwidth (`"SJ"`) from the legacy default
implemented the base R `density` function (`"nrd0"`). However, Deng and
Wickham suggest that `method = "KernSmooth"` is the fastest and the most
accurate.

## Usage

``` r
estimate_density(x, ...)

# S3 method for class 'data.frame'
estimate_density(
  x,
  method = "kernel",
  precision = 2^10,
  extend = FALSE,
  extend_scale = 0.1,
  bw = "SJ",
  ci = NULL,
  select = NULL,
  by = NULL,
  rvar_col = NULL,
  ...
)

# S3 method for class 'brmsfit'
estimate_density(
  x,
  method = "kernel",
  precision = 2^10,
  extend = FALSE,
  extend_scale = 0.1,
  bw = "SJ",
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  ...
)
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

- method:

  Density estimation method. Can be `"kernel"` (default), `"logspline"`
  or `"KernSmooth"`.

- precision:

  Number of points of density data. See the `n` parameter in `density`.

- extend:

  Extend the range of the x axis by a factor of `extend_scale`.

- extend_scale:

  Ratio of range by which to extend the x axis. A value of `0.1` means
  that the x axis will be extended by `1/10` of the range of the data.

- bw:

  See the eponymous argument in `density`. Here, the default has been
  changed for `"SJ"`, which is recommended.

- ci:

  The confidence interval threshold. Only used when `method = "kernel"`.
  This feature is experimental, use with caution.

- select:

  Character vector of column names. If `NULL` (the default), all numeric
  variables will be selected. Other arguments from
  [`datawizard::extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.html)
  (such as `exclude`) can also be used.

- by:

  Optional character vector. If not `NULL` and input is a data frame,
  density estimation is performed for each group (subsets) indicated by
  `by`. See examples.

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

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/bayestestR.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Model components

Possible values for the `component` argument depend on the model class.
Following are valid options:

- `"all"`: returns all model components, applies to all models, but will
  only have an effect for models with more than just the conditional
  model component.

- `"conditional"`: only returns the conditional component, i.e. "fixed
  effects" terms from the model. Will only have an effect for models
  with more than just the conditional model component.

- `"smooth_terms"`: returns smooth terms, only applies to GAMs (or
  similar models that may contain smooth terms).

- `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.

- `"location"`: returns location parameters such as `conditional`,
  `zero_inflated`, or `smooth_terms` (everything that are fixed or
  random effects - depending on the `effects` argument - but no
  auxiliary parameters).

- `"distributional"` (or `"auxiliary"`): components like `sigma`,
  `dispersion`, `beta` or `precision` (and other auxiliary parameters)
  are returned.

For models of class `brmsfit` (package **brms**), even more options are
possible for the `component` argument, which are not all documented in
detail here. See also
[[`?insight::find_parameters`](https://easystats.github.io/insight/reference/find_parameters.html)](https://easystats.github.io/insight/reference/find_parameters.BGGM.html).

## References

Deng, H., & Wickham, H. (2011). Density estimation in R. Electronic
publication.

## Examples

``` r
library(bayestestR)

set.seed(1)
x <- rnorm(250, mean = 1)

# Basic usage
density_kernel <- estimate_density(x) # default method is "kernel"

hist(x, prob = TRUE)
lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2)
lines(density_kernel$x, density_kernel$CI_low, col = "gray", lty = 2)
lines(density_kernel$x, density_kernel$CI_high, col = "gray", lty = 2)
legend("topright",
  legend = c("Estimate", "95% CI"),
  col = c("black", "gray"), lwd = 2, lty = c(1, 2)
)


# Other Methods
density_logspline <- estimate_density(x, method = "logspline")
density_KernSmooth <- estimate_density(x, method = "KernSmooth")
density_mixture <- estimate_density(x, method = "mixture")


hist(x, prob = TRUE)
lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2)
lines(density_logspline$x, density_logspline$y, col = "red", lwd = 2)
lines(density_KernSmooth$x, density_KernSmooth$y, col = "blue", lwd = 2)
lines(density_mixture$x, density_mixture$y, col = "green", lwd = 2)


# Extension
density_extended <- estimate_density(x, extend = TRUE)
density_default <- estimate_density(x, extend = FALSE)

hist(x, prob = TRUE)
lines(density_extended$x, density_extended$y, col = "red", lwd = 3)
lines(density_default$x, density_default$y, col = "black", lwd = 3)


# Multiple columns
head(estimate_density(iris))
#>      Parameter        x          y
#> 1 Sepal.Length 4.300000 0.09643086
#> 2 Sepal.Length 4.303519 0.09759152
#> 3 Sepal.Length 4.307038 0.09875679
#> 4 Sepal.Length 4.310557 0.09993469
#> 5 Sepal.Length 4.314076 0.10111692
#> 6 Sepal.Length 4.317595 0.10230788
head(estimate_density(iris, select = "Sepal.Width"))
#>     Parameter        x          y
#> 1 Sepal.Width 2.000000 0.04647877
#> 2 Sepal.Width 2.002346 0.04729167
#> 3 Sepal.Width 2.004692 0.04811925
#> 4 Sepal.Width 2.007038 0.04895638
#> 5 Sepal.Width 2.009384 0.04980346
#> 6 Sepal.Width 2.011730 0.05066768

# Grouped data
head(estimate_density(iris, by = "Species"))
#>      Parameter        x         y Species
#> 1 Sepal.Length 4.300000 0.2354858  setosa
#> 2 Sepal.Length 4.301466 0.2374750  setosa
#> 3 Sepal.Length 4.302933 0.2394634  setosa
#> 4 Sepal.Length 4.304399 0.2414506  setosa
#> 5 Sepal.Length 4.305865 0.2434373  setosa
#> 6 Sepal.Length 4.307331 0.2454216  setosa
head(estimate_density(iris$Petal.Width, by = iris$Species))
#>           x        y  Group
#> 1 0.1000000 9.011849 setosa
#> 2 0.1004888 8.955321 setosa
#> 3 0.1009775 8.792006 setosa
#> 4 0.1014663 8.527789 setosa
#> 5 0.1019550 8.171922 setosa
#> 6 0.1024438 7.736494 setosa
# \donttest{
# rstanarm models
# -----------------------------------------------
library(rstanarm)
model <- suppressWarnings(
  stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
)
head(estimate_density(model))
#>     Parameter        x           y
#> 1 (Intercept) 24.19242 0.002028542
#> 2 (Intercept) 24.22067 0.002051099
#> 3 (Intercept) 24.24892 0.002073742
#> 4 (Intercept) 24.27717 0.002096471
#> 5 (Intercept) 24.30542 0.002119371
#> 6 (Intercept) 24.33367 0.002142358

library(emmeans)
head(estimate_density(emtrends(model, ~1, "wt", data = mtcars)))
#>        X1         x          y
#> 1 overall -7.810281 0.01753665
#> 2 overall -7.806283 0.01762645
#> 3 overall -7.802285 0.01771463
#> 4 overall -7.798287 0.01780030
#> 5 overall -7.794289 0.01788463
#> 6 overall -7.790292 0.01796733

# brms models
# -----------------------------------------------
library(brms)
model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 5e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.05 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.019 seconds (Warm-up)
#> Chain 1:                0.014 seconds (Sampling)
#> Chain 1:                0.033 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 3e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.019 seconds (Warm-up)
#> Chain 2:                0.013 seconds (Sampling)
#> Chain 2:                0.032 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 3e-06 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.02 seconds (Warm-up)
#> Chain 3:                0.017 seconds (Sampling)
#> Chain 3:                0.037 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 3e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.02 seconds (Warm-up)
#> Chain 4:                0.017 seconds (Sampling)
#> Chain 4:                0.037 seconds (Total)
#> Chain 4: 
estimate_density(model)
#>        Parameter            x            y
#> 1    b_Intercept 31.677511642 2.750315e-04
#> 2    b_Intercept 31.692604903 2.747610e-04
#> 3    b_Intercept 31.707698163 2.740329e-04
#> 4    b_Intercept 31.722791424 2.728507e-04
#> 5    b_Intercept 31.737884685 2.712198e-04
#> 6    b_Intercept 31.752977946 2.691474e-04
#> 7    b_Intercept 31.768071206 2.666050e-04
#> 8    b_Intercept 31.783164467 2.635872e-04
#> 9    b_Intercept 31.798257728 2.601691e-04
#> 10   b_Intercept 31.813350988 2.563662e-04
#> 11   b_Intercept 31.828444249 2.521953e-04
#> 12   b_Intercept 31.843537510 2.476744e-04
#> 13   b_Intercept 31.858630770 2.428182e-04
#> 14   b_Intercept 31.873724031 2.375927e-04
#> 15   b_Intercept 31.888817292 2.320916e-04
#> 16   b_Intercept 31.903910552 2.263376e-04
#> 17   b_Intercept 31.919003813 2.203543e-04
#> 18   b_Intercept 31.934097074 2.141651e-04
#> 19   b_Intercept 31.949190334 2.077943e-04
#> 20   b_Intercept 31.964283595 2.012455e-04
#> 21   b_Intercept 31.979376856 1.945691e-04
#> 22   b_Intercept 31.994470116 1.877983e-04
#> 23   b_Intercept 32.009563377 1.809569e-04
#> 24   b_Intercept 32.024656638 1.740682e-04
#> 25   b_Intercept 32.039749899 1.671553e-04
#> 26   b_Intercept 32.054843159 1.602432e-04
#> 27   b_Intercept 32.069936420 1.533636e-04
#> 28   b_Intercept 32.085029681 1.465364e-04
#> 29   b_Intercept 32.100122941 1.397808e-04
#> 30   b_Intercept 32.115216202 1.331152e-04
#> 31   b_Intercept 32.130309463 1.265571e-04
#> 32   b_Intercept 32.145402723 1.201261e-04
#> 33   b_Intercept 32.160495984 1.138693e-04
#> 34   b_Intercept 32.175589245 1.077739e-04
#> 35   b_Intercept 32.190682505 1.018525e-04
#> 36   b_Intercept 32.205775766 9.611626e-05
#> 37   b_Intercept 32.220869027 9.057580e-05
#> 38   b_Intercept 32.235962287 8.524068e-05
#> 39   b_Intercept 32.251055548 8.015963e-05
#> 40   b_Intercept 32.266148809 7.531581e-05
#> 41   b_Intercept 32.281242069 7.070384e-05
#> 42   b_Intercept 32.296335330 6.632925e-05
#> 43   b_Intercept 32.311428591 6.219697e-05
#> 44   b_Intercept 32.326521851 5.831140e-05
#> 45   b_Intercept 32.341615112 5.470275e-05
#> 46   b_Intercept 32.356708373 5.138225e-05
#> 47   b_Intercept 32.371801634 4.832055e-05
#> 48   b_Intercept 32.386894894 4.552042e-05
#> 49   b_Intercept 32.401988155 4.298453e-05
#> 50   b_Intercept 32.417081416 4.071555e-05
#> 51   b_Intercept 32.432174676 3.872390e-05
#> 52   b_Intercept 32.447267937 3.706140e-05
#> 53   b_Intercept 32.462361198 3.567666e-05
#> 54   b_Intercept 32.477454458 3.457348e-05
#> 55   b_Intercept 32.492547719 3.375609e-05
#> 56   b_Intercept 32.507640980 3.322920e-05
#> 57   b_Intercept 32.522734240 3.299803e-05
#> 58   b_Intercept 32.537827501 3.312604e-05
#> 59   b_Intercept 32.552920762 3.358088e-05
#> 60   b_Intercept 32.568014022 3.435644e-05
#> 61   b_Intercept 32.583107283 3.546153e-05
#> 62   b_Intercept 32.598200544 3.690572e-05
#> 63   b_Intercept 32.613293804 3.869939e-05
#> 64   b_Intercept 32.628387065 4.089532e-05
#> 65   b_Intercept 32.643480326 4.351605e-05
#> 66   b_Intercept 32.658573587 4.653145e-05
#> 67   b_Intercept 32.673666847 4.995626e-05
#> 68   b_Intercept 32.688760108 5.380601e-05
#> 69   b_Intercept 32.703853369 5.809703e-05
#> 70   b_Intercept 32.718946629 6.286326e-05
#> 71   b_Intercept 32.734039890 6.820712e-05
#> 72   b_Intercept 32.749133151 7.405780e-05
#> 73   b_Intercept 32.764226411 8.043528e-05
#> 74   b_Intercept 32.779319672 8.736004e-05
#> 75   b_Intercept 32.794412933 9.485309e-05
#> 76   b_Intercept 32.809506193 1.029358e-04
#> 77   b_Intercept 32.824599454 1.117538e-04
#> 78   b_Intercept 32.839692715 1.212438e-04
#> 79   b_Intercept 32.854785975 1.314036e-04
#> 80   b_Intercept 32.869879236 1.422560e-04
#> 81   b_Intercept 32.884972497 1.538237e-04
#> 82   b_Intercept 32.900065757 1.661291e-04
#> 83   b_Intercept 32.915159018 1.792873e-04
#> 84   b_Intercept 32.930252279 1.933288e-04
#> 85   b_Intercept 32.945345539 2.081875e-04
#> 86   b_Intercept 32.960438800 2.238846e-04
#> 87   b_Intercept 32.975532061 2.404404e-04
#> 88   b_Intercept 32.990625322 2.578750e-04
#> 89   b_Intercept 33.005718582 2.762463e-04
#> 90   b_Intercept 33.020811843 2.957153e-04
#> 91   b_Intercept 33.035905104 3.161285e-04
#> 92   b_Intercept 33.050998364 3.375019e-04
#> 93   b_Intercept 33.066091625 3.598505e-04
#> 94   b_Intercept 33.081184886 3.831885e-04
#> 95   b_Intercept 33.096278146 4.075289e-04
#> 96   b_Intercept 33.111371407 4.330883e-04
#> 97   b_Intercept 33.126464668 4.597125e-04
#> 98   b_Intercept 33.141557928 4.873768e-04
#> 99   b_Intercept 33.156651189 5.160887e-04
#> 100  b_Intercept 33.171744450 5.458550e-04
#> 101  b_Intercept 33.186837710 5.766811e-04
#> 102  b_Intercept 33.201930971 6.087035e-04
#> 103  b_Intercept 33.217024232 6.419097e-04
#> 104  b_Intercept 33.232117492 6.761860e-04
#> 105  b_Intercept 33.247210753 7.115323e-04
#> 106  b_Intercept 33.262304014 7.479478e-04
#> 107  b_Intercept 33.277397275 7.854306e-04
#> 108  b_Intercept 33.292490535 8.240289e-04
#> 109  b_Intercept 33.307583796 8.638791e-04
#> 110  b_Intercept 33.322677057 9.047803e-04
#> 111  b_Intercept 33.337770317 9.467256e-04
#> 112  b_Intercept 33.352863578 9.897067e-04
#> 113  b_Intercept 33.367956839 1.033715e-03
#> 114  b_Intercept 33.383050099 1.078740e-03
#> 115  b_Intercept 33.398143360 1.124972e-03
#> 116  b_Intercept 33.413236621 1.172216e-03
#> 117  b_Intercept 33.428329881 1.220431e-03
#> 118  b_Intercept 33.443423142 1.269600e-03
#> 119  b_Intercept 33.458516403 1.319708e-03
#> 120  b_Intercept 33.473609663 1.370737e-03
#> 121  b_Intercept 33.488702924 1.422780e-03
#> 122  b_Intercept 33.503796185 1.475782e-03
#> 123  b_Intercept 33.518889445 1.529629e-03
#> 124  b_Intercept 33.533982706 1.584296e-03
#> 125  b_Intercept 33.549075967 1.639758e-03
#> 126  b_Intercept 33.564169228 1.695987e-03
#> 127  b_Intercept 33.579262488 1.752994e-03
#> 128  b_Intercept 33.594355749 1.810821e-03
#> 129  b_Intercept 33.609449010 1.869305e-03
#> 130  b_Intercept 33.624542270 1.928412e-03
#> 131  b_Intercept 33.639635531 1.988108e-03
#> 132  b_Intercept 33.654728792 2.048355e-03
#> 133  b_Intercept 33.669822052 2.109115e-03
#> 134  b_Intercept 33.684915313 2.170437e-03
#> 135  b_Intercept 33.700008574 2.232178e-03
#> 136  b_Intercept 33.715101834 2.294287e-03
#> 137  b_Intercept 33.730195095 2.356721e-03
#> 138  b_Intercept 33.745288356 2.419439e-03
#> 139  b_Intercept 33.760381616 2.482397e-03
#> 140  b_Intercept 33.775474877 2.545570e-03
#> 141  b_Intercept 33.790568138 2.608890e-03
#> 142  b_Intercept 33.805661398 2.672296e-03
#> 143  b_Intercept 33.820754659 2.735747e-03
#> 144  b_Intercept 33.835847920 2.799202e-03
#> 145  b_Intercept 33.850941180 2.862624e-03
#> 146  b_Intercept 33.866034441 2.925967e-03
#> 147  b_Intercept 33.881127702 2.989169e-03
#> 148  b_Intercept 33.896220963 3.052212e-03
#> 149  b_Intercept 33.911314223 3.115067e-03
#> 150  b_Intercept 33.926407484 3.177709e-03
#> 151  b_Intercept 33.941500745 3.240114e-03
#> 152  b_Intercept 33.956594005 3.302262e-03
#> 153  b_Intercept 33.971687266 3.364072e-03
#> 154  b_Intercept 33.986780527 3.425588e-03
#> 155  b_Intercept 34.001873787 3.486807e-03
#> 156  b_Intercept 34.016967048 3.547730e-03
#> 157  b_Intercept 34.032060309 3.608357e-03
#> 158  b_Intercept 34.047153569 3.668696e-03
#> 159  b_Intercept 34.062246830 3.728721e-03
#> 160  b_Intercept 34.077340091 3.788472e-03
#> 161  b_Intercept 34.092433351 3.847997e-03
#> 162  b_Intercept 34.107526612 3.907324e-03
#> 163  b_Intercept 34.122619873 3.966483e-03
#> 164  b_Intercept 34.137713133 4.025511e-03
#> 165  b_Intercept 34.152806394 4.084443e-03
#> 166  b_Intercept 34.167899655 4.143338e-03
#> 167  b_Intercept 34.182992916 4.202260e-03
#> 168  b_Intercept 34.198086176 4.261261e-03
#> 169  b_Intercept 34.213179437 4.320396e-03
#> 170  b_Intercept 34.228272698 4.379720e-03
#> 171  b_Intercept 34.243365958 4.439295e-03
#> 172  b_Intercept 34.258459219 4.499272e-03
#> 173  b_Intercept 34.273552480 4.559662e-03
#> 174  b_Intercept 34.288645740 4.620529e-03
#> 175  b_Intercept 34.303739001 4.681938e-03
#> 176  b_Intercept 34.318832262 4.743952e-03
#> 177  b_Intercept 34.333925522 4.806639e-03
#> 178  b_Intercept 34.349018783 4.870188e-03
#> 179  b_Intercept 34.364112044 4.934637e-03
#> 180  b_Intercept 34.379205304 4.999988e-03
#> 181  b_Intercept 34.394298565 5.066300e-03
#> 182  b_Intercept 34.409391826 5.133633e-03
#> 183  b_Intercept 34.424485086 5.202044e-03
#> 184  b_Intercept 34.439578347 5.271678e-03
#> 185  b_Intercept 34.454671608 5.342704e-03
#> 186  b_Intercept 34.469764868 5.414991e-03
#> 187  b_Intercept 34.484858129 5.488583e-03
#> 188  b_Intercept 34.499951390 5.563520e-03
#> 189  b_Intercept 34.515044651 5.639842e-03
#> 190  b_Intercept 34.530137911 5.717583e-03
#> 191  b_Intercept 34.545231172 5.797118e-03
#> 192  b_Intercept 34.560324433 5.878145e-03
#> 193  b_Intercept 34.575417693 5.960685e-03
#> 194  b_Intercept 34.590510954 6.044757e-03
#> 195  b_Intercept 34.605604215 6.130376e-03
#> 196  b_Intercept 34.620697475 6.217555e-03
#> 197  b_Intercept 34.635790736 6.306553e-03
#> 198  b_Intercept 34.650883997 6.397246e-03
#> 199  b_Intercept 34.665977257 6.489521e-03
#> 200  b_Intercept 34.681070518 6.583378e-03
#> 201  b_Intercept 34.696163779 6.678818e-03
#> 202  b_Intercept 34.711257039 6.775837e-03
#> 203  b_Intercept 34.726350300 6.874562e-03
#> 204  b_Intercept 34.741443561 6.975093e-03
#> 205  b_Intercept 34.756536821 7.077190e-03
#> 206  b_Intercept 34.771630082 7.180851e-03
#> 207  b_Intercept 34.786723343 7.286073e-03
#> 208  b_Intercept 34.801816604 7.392856e-03
#> 209  b_Intercept 34.816909864 7.501213e-03
#> 210  b_Intercept 34.832003125 7.611488e-03
#> 211  b_Intercept 34.847096386 7.723342e-03
#> 212  b_Intercept 34.862189646 7.836789e-03
#> 213  b_Intercept 34.877282907 7.951845e-03
#> 214  b_Intercept 34.892376168 8.068534e-03
#> 215  b_Intercept 34.907469428 8.186883e-03
#> 216  b_Intercept 34.922562689 8.307209e-03
#> 217  b_Intercept 34.937655950 8.429409e-03
#> 218  b_Intercept 34.952749210 8.553418e-03
#> 219  b_Intercept 34.967842471 8.679294e-03
#> 220  b_Intercept 34.982935732 8.807102e-03
#> 221  b_Intercept 34.998028992 8.936912e-03
#> 222  b_Intercept 35.013122253 9.069000e-03
#> 223  b_Intercept 35.028215514 9.203608e-03
#> 224  b_Intercept 35.043308774 9.340541e-03
#> 225  b_Intercept 35.058402035 9.479906e-03
#> 226  b_Intercept 35.073495296 9.621818e-03
#> 227  b_Intercept 35.088588556 9.766397e-03
#> 228  b_Intercept 35.103681817 9.913813e-03
#> 229  b_Intercept 35.118775078 1.006485e-02
#> 230  b_Intercept 35.133868339 1.021905e-02
#> 231  b_Intercept 35.148961599 1.037655e-02
#> 232  b_Intercept 35.164054860 1.053752e-02
#> 233  b_Intercept 35.179148121 1.070212e-02
#> 234  b_Intercept 35.194241381 1.087049e-02
#> 235  b_Intercept 35.209334642 1.104354e-02
#> 236  b_Intercept 35.224427903 1.122104e-02
#> 237  b_Intercept 35.239521163 1.140293e-02
#> 238  b_Intercept 35.254614424 1.158938e-02
#> 239  b_Intercept 35.269707685 1.178055e-02
#> 240  b_Intercept 35.284800945 1.197662e-02
#> 241  b_Intercept 35.299894206 1.217825e-02
#> 242  b_Intercept 35.314987467 1.238588e-02
#> 243  b_Intercept 35.330080727 1.259898e-02
#> 244  b_Intercept 35.345173988 1.281770e-02
#> 245  b_Intercept 35.360267249 1.304220e-02
#> 246  b_Intercept 35.375360509 1.327260e-02
#> 247  b_Intercept 35.390453770 1.350918e-02
#> 248  b_Intercept 35.405547031 1.375328e-02
#> 249  b_Intercept 35.420640292 1.400375e-02
#> 250  b_Intercept 35.435733552 1.426069e-02
#> 251  b_Intercept 35.450826813 1.452421e-02
#> 252  b_Intercept 35.465920074 1.479440e-02
#> 253  b_Intercept 35.481013334 1.507136e-02
#> 254  b_Intercept 35.496106595 1.535638e-02
#> 255  b_Intercept 35.511199856 1.564873e-02
#> 256  b_Intercept 35.526293116 1.594809e-02
#> 257  b_Intercept 35.541386377 1.625451e-02
#> 258  b_Intercept 35.556479638 1.656803e-02
#> 259  b_Intercept 35.571572898 1.688870e-02
#> 260  b_Intercept 35.586666159 1.721728e-02
#> 261  b_Intercept 35.601759420 1.755399e-02
#> 262  b_Intercept 35.616852680 1.789793e-02
#> 263  b_Intercept 35.631945941 1.824908e-02
#> 264  b_Intercept 35.647039202 1.860746e-02
#> 265  b_Intercept 35.662132462 1.897305e-02
#> 266  b_Intercept 35.677225723 1.934604e-02
#> 267  b_Intercept 35.692318984 1.972766e-02
#> 268  b_Intercept 35.707412244 2.011641e-02
#> 269  b_Intercept 35.722505505 2.051225e-02
#> 270  b_Intercept 35.737598766 2.091514e-02
#> 271  b_Intercept 35.752692027 2.132501e-02
#> 272  b_Intercept 35.767785287 2.174182e-02
#> 273  b_Intercept 35.782878548 2.216673e-02
#> 274  b_Intercept 35.797971809 2.259869e-02
#> 275  b_Intercept 35.813065069 2.303729e-02
#> 276  b_Intercept 35.828158330 2.348243e-02
#> 277  b_Intercept 35.843251591 2.393400e-02
#> 278  b_Intercept 35.858344851 2.439187e-02
#> 279  b_Intercept 35.873438112 2.485657e-02
#> 280  b_Intercept 35.888531373 2.532796e-02
#> 281  b_Intercept 35.903624633 2.580513e-02
#> 282  b_Intercept 35.918717894 2.628790e-02
#> 283  b_Intercept 35.933811155 2.677609e-02
#> 284  b_Intercept 35.948904415 2.726949e-02
#> 285  b_Intercept 35.963997676 2.776808e-02
#> 286  b_Intercept 35.979090937 2.827230e-02
#> 287  b_Intercept 35.994184197 2.878094e-02
#> 288  b_Intercept 36.009277458 2.929374e-02
#> 289  b_Intercept 36.024370719 2.981045e-02
#> 290  b_Intercept 36.039463980 3.033079e-02
#> 291  b_Intercept 36.054557240 3.085448e-02
#> 292  b_Intercept 36.069650501 3.138174e-02
#> 293  b_Intercept 36.084743762 3.191173e-02
#> 294  b_Intercept 36.099837022 3.244402e-02
#> 295  b_Intercept 36.114930283 3.297830e-02
#> 296  b_Intercept 36.130023544 3.351428e-02
#> 297  b_Intercept 36.145116804 3.405166e-02
#> 298  b_Intercept 36.160210065 3.459021e-02
#> 299  b_Intercept 36.175303326 3.512950e-02
#> 300  b_Intercept 36.190396586 3.566915e-02
#> 301  b_Intercept 36.205489847 3.620888e-02
#> 302  b_Intercept 36.220583108 3.674844e-02
#> 303  b_Intercept 36.235676368 3.728758e-02
#> 304  b_Intercept 36.250769629 3.782604e-02
#> 305  b_Intercept 36.265862890 3.836341e-02
#> 306  b_Intercept 36.280956150 3.889965e-02
#> 307  b_Intercept 36.296049411 3.943463e-02
#> 308  b_Intercept 36.311142672 3.996823e-02
#> 309  b_Intercept 36.326235933 4.050037e-02
#> 310  b_Intercept 36.341329193 4.103100e-02
#> 311  b_Intercept 36.356422454 4.155977e-02
#> 312  b_Intercept 36.371515715 4.208699e-02
#> 313  b_Intercept 36.386608975 4.261279e-02
#> 314  b_Intercept 36.401702236 4.313727e-02
#> 315  b_Intercept 36.416795497 4.366059e-02
#> 316  b_Intercept 36.431888757 4.418292e-02
#> 317  b_Intercept 36.446982018 4.470443e-02
#> 318  b_Intercept 36.462075279 4.522553e-02
#> 319  b_Intercept 36.477168539 4.574662e-02
#> 320  b_Intercept 36.492261800 4.626808e-02
#> 321  b_Intercept 36.507355061 4.679028e-02
#> 322  b_Intercept 36.522448321 4.731367e-02
#> 323  b_Intercept 36.537541582 4.783881e-02
#> 324  b_Intercept 36.552634843 4.836669e-02
#> 325  b_Intercept 36.567728103 4.889756e-02
#> 326  b_Intercept 36.582821364 4.943199e-02
#> 327  b_Intercept 36.597914625 4.997057e-02
#> 328  b_Intercept 36.613007885 5.051391e-02
#> 329  b_Intercept 36.628101146 5.106262e-02
#> 330  b_Intercept 36.643194407 5.161881e-02
#> 331  b_Intercept 36.658287668 5.218224e-02
#> 332  b_Intercept 36.673380928 5.275342e-02
#> 333  b_Intercept 36.688474189 5.333302e-02
#> 334  b_Intercept 36.703567450 5.392170e-02
#> 335  b_Intercept 36.718660710 5.452013e-02
#> 336  b_Intercept 36.733753971 5.513044e-02
#> 337  b_Intercept 36.748847232 5.575327e-02
#> 338  b_Intercept 36.763940492 5.638816e-02
#> 339  b_Intercept 36.779033753 5.703572e-02
#> 340  b_Intercept 36.794127014 5.769652e-02
#> 341  b_Intercept 36.809220274 5.837113e-02
#> 342  b_Intercept 36.824313535 5.906091e-02
#> 343  b_Intercept 36.839406796 5.976834e-02
#> 344  b_Intercept 36.854500056 6.049133e-02
#> 345  b_Intercept 36.869593317 6.123028e-02
#> 346  b_Intercept 36.884686578 6.198556e-02
#> 347  b_Intercept 36.899779838 6.275749e-02
#> 348  b_Intercept 36.914873099 6.354635e-02
#> 349  b_Intercept 36.929966360 6.435609e-02
#> 350  b_Intercept 36.945059621 6.518363e-02
#> 351  b_Intercept 36.960152881 6.602876e-02
#> 352  b_Intercept 36.975246142 6.689152e-02
#> 353  b_Intercept 36.990339403 6.777195e-02
#> 354  b_Intercept 37.005432663 6.867003e-02
#> 355  b_Intercept 37.020525924 6.958808e-02
#> 356  b_Intercept 37.035619185 7.052517e-02
#> 357  b_Intercept 37.050712445 7.147945e-02
#> 358  b_Intercept 37.065805706 7.245068e-02
#> 359  b_Intercept 37.080898967 7.343859e-02
#> 360  b_Intercept 37.095992227 7.444286e-02
#> 361  b_Intercept 37.111085488 7.546411e-02
#> 362  b_Intercept 37.126178749 7.650345e-02
#> 363  b_Intercept 37.141272009 7.755772e-02
#> 364  b_Intercept 37.156365270 7.862644e-02
#> 365  b_Intercept 37.171458531 7.970910e-02
#> 366  b_Intercept 37.186551791 8.080518e-02
#> 367  b_Intercept 37.201645052 8.191412e-02
#> 368  b_Intercept 37.216738313 8.303783e-02
#> 369  b_Intercept 37.231831573 8.417301e-02
#> 370  b_Intercept 37.246924834 8.531890e-02
#> 371  b_Intercept 37.262018095 8.647486e-02
#> 372  b_Intercept 37.277111356 8.764026e-02
#> 373  b_Intercept 37.292204616 8.881444e-02
#> 374  b_Intercept 37.307297877 8.999777e-02
#> 375  b_Intercept 37.322391138 9.118891e-02
#> 376  b_Intercept 37.337484398 9.238649e-02
#> 377  b_Intercept 37.352577659 9.358986e-02
#> 378  b_Intercept 37.367670920 9.479839e-02
#> 379  b_Intercept 37.382764180 9.601145e-02
#> 380  b_Intercept 37.397857441 9.722862e-02
#> 381  b_Intercept 37.412950702 9.844935e-02
#> 382  b_Intercept 37.428043962 9.967243e-02
#> 383  b_Intercept 37.443137223 1.008973e-01
#> 384  b_Intercept 37.458230484 1.021234e-01
#> 385  b_Intercept 37.473323744 1.033502e-01
#> 386  b_Intercept 37.488417005 1.045772e-01
#> 387  b_Intercept 37.503510266 1.058036e-01
#> 388  b_Intercept 37.518603526 1.070289e-01
#> 389  b_Intercept 37.533696787 1.082528e-01
#> 390  b_Intercept 37.548790048 1.094747e-01
#> 391  b_Intercept 37.563883309 1.106943e-01
#> 392  b_Intercept 37.578976569 1.119113e-01
#> 393  b_Intercept 37.594069830 1.131247e-01
#> 394  b_Intercept 37.609163091 1.143343e-01
#> 395  b_Intercept 37.624256351 1.155401e-01
#> 396  b_Intercept 37.639349612 1.167418e-01
#> 397  b_Intercept 37.654442873 1.179393e-01
#> 398  b_Intercept 37.669536133 1.191323e-01
#> 399  b_Intercept 37.684629394 1.203201e-01
#> 400  b_Intercept 37.699722655 1.215024e-01
#> 401  b_Intercept 37.714815915 1.226796e-01
#> 402  b_Intercept 37.729909176 1.238516e-01
#> 403  b_Intercept 37.745002437 1.250184e-01
#> 404  b_Intercept 37.760095697 1.261800e-01
#> 405  b_Intercept 37.775188958 1.273363e-01
#> 406  b_Intercept 37.790282219 1.284860e-01
#> 407  b_Intercept 37.805375479 1.296306e-01
#> 408  b_Intercept 37.820468740 1.307699e-01
#> 409  b_Intercept 37.835562001 1.319041e-01
#> 410  b_Intercept 37.850655261 1.330333e-01
#> 411  b_Intercept 37.865748522 1.341576e-01
#> 412  b_Intercept 37.880841783 1.352764e-01
#> 413  b_Intercept 37.895935044 1.363902e-01
#> 414  b_Intercept 37.911028304 1.374997e-01
#> 415  b_Intercept 37.926121565 1.386050e-01
#> 416  b_Intercept 37.941214826 1.397063e-01
#> 417  b_Intercept 37.956308086 1.408037e-01
#> 418  b_Intercept 37.971401347 1.418973e-01
#> 419  b_Intercept 37.986494608 1.429870e-01
#> 420  b_Intercept 38.001587868 1.440736e-01
#> 421  b_Intercept 38.016681129 1.451574e-01
#> 422  b_Intercept 38.031774390 1.462385e-01
#> 423  b_Intercept 38.046867650 1.473171e-01
#> 424  b_Intercept 38.061960911 1.483934e-01
#> 425  b_Intercept 38.077054172 1.494671e-01
#> 426  b_Intercept 38.092147432 1.505390e-01
#> 427  b_Intercept 38.107240693 1.516090e-01
#> 428  b_Intercept 38.122333954 1.526774e-01
#> 429  b_Intercept 38.137427214 1.537443e-01
#> 430  b_Intercept 38.152520475 1.548096e-01
#> 431  b_Intercept 38.167613736 1.558732e-01
#> 432  b_Intercept 38.182706997 1.569351e-01
#> 433  b_Intercept 38.197800257 1.579956e-01
#> 434  b_Intercept 38.212893518 1.590543e-01
#> 435  b_Intercept 38.227986779 1.601114e-01
#> 436  b_Intercept 38.243080039 1.611665e-01
#> 437  b_Intercept 38.258173300 1.622194e-01
#> 438  b_Intercept 38.273266561 1.632696e-01
#> 439  b_Intercept 38.288359821 1.643171e-01
#> 440  b_Intercept 38.303453082 1.653616e-01
#> 441  b_Intercept 38.318546343 1.664028e-01
#> 442  b_Intercept 38.333639603 1.674404e-01
#> 443  b_Intercept 38.348732864 1.684739e-01
#> 444  b_Intercept 38.363826125 1.695020e-01
#> 445  b_Intercept 38.378919385 1.705249e-01
#> 446  b_Intercept 38.394012646 1.715425e-01
#> 447  b_Intercept 38.409105907 1.725540e-01
#> 448  b_Intercept 38.424199167 1.735593e-01
#> 449  b_Intercept 38.439292428 1.745577e-01
#> 450  b_Intercept 38.454385689 1.755475e-01
#> 451  b_Intercept 38.469478949 1.765290e-01
#> 452  b_Intercept 38.484572210 1.775021e-01
#> 453  b_Intercept 38.499665471 1.784665e-01
#> 454  b_Intercept 38.514758732 1.794218e-01
#> 455  b_Intercept 38.529851992 1.803676e-01
#> 456  b_Intercept 38.544945253 1.813027e-01
#> 457  b_Intercept 38.560038514 1.822263e-01
#> 458  b_Intercept 38.575131774 1.831395e-01
#> 459  b_Intercept 38.590225035 1.840421e-01
#> 460  b_Intercept 38.605318296 1.849339e-01
#> 461  b_Intercept 38.620411556 1.858150e-01
#> 462  b_Intercept 38.635504817 1.866850e-01
#> 463  b_Intercept 38.650598078 1.875418e-01
#> 464  b_Intercept 38.665691338 1.883879e-01
#> 465  b_Intercept 38.680784599 1.892234e-01
#> 466  b_Intercept 38.695877860 1.900484e-01
#> 467  b_Intercept 38.710971120 1.908632e-01
#> 468  b_Intercept 38.726064381 1.916680e-01
#> 469  b_Intercept 38.741157642 1.924614e-01
#> 470  b_Intercept 38.756250902 1.932451e-01
#> 471  b_Intercept 38.771344163 1.940201e-01
#> 472  b_Intercept 38.786437424 1.947865e-01
#> 473  b_Intercept 38.801530685 1.955450e-01
#> 474  b_Intercept 38.816623945 1.962959e-01
#> 475  b_Intercept 38.831717206 1.970389e-01
#> 476  b_Intercept 38.846810467 1.977745e-01
#> 477  b_Intercept 38.861903727 1.985040e-01
#> 478  b_Intercept 38.876996988 1.992279e-01
#> 479  b_Intercept 38.892090249 1.999464e-01
#> 480  b_Intercept 38.907183509 2.006601e-01
#> 481  b_Intercept 38.922276770 2.013690e-01
#> 482  b_Intercept 38.937370031 2.020728e-01
#> 483  b_Intercept 38.952463291 2.027726e-01
#> 484  b_Intercept 38.967556552 2.034686e-01
#> 485  b_Intercept 38.982649813 2.041610e-01
#> 486  b_Intercept 38.997743073 2.048497e-01
#> 487  b_Intercept 39.012836334 2.055348e-01
#> 488  b_Intercept 39.027929595 2.062154e-01
#> 489  b_Intercept 39.043022855 2.068920e-01
#> 490  b_Intercept 39.058116116 2.075643e-01
#> 491  b_Intercept 39.073209377 2.082320e-01
#> 492  b_Intercept 39.088302638 2.088948e-01
#> 493  b_Intercept 39.103395898 2.095522e-01
#> 494  b_Intercept 39.118489159 2.102030e-01
#> 495  b_Intercept 39.133582420 2.108461e-01
#> 496  b_Intercept 39.148675680 2.114817e-01
#> 497  b_Intercept 39.163768941 2.121090e-01
#> 498  b_Intercept 39.178862202 2.127271e-01
#> 499  b_Intercept 39.193955462 2.133353e-01
#> 500  b_Intercept 39.209048723 2.139322e-01
#> 501  b_Intercept 39.224141984 2.145145e-01
#> 502  b_Intercept 39.239235244 2.150836e-01
#> 503  b_Intercept 39.254328505 2.156382e-01
#> 504  b_Intercept 39.269421766 2.161776e-01
#> 505  b_Intercept 39.284515026 2.167005e-01
#> 506  b_Intercept 39.299608287 2.172060e-01
#> 507  b_Intercept 39.314701548 2.176892e-01
#> 508  b_Intercept 39.329794808 2.181517e-01
#> 509  b_Intercept 39.344888069 2.185931e-01
#> 510  b_Intercept 39.359981330 2.190125e-01
#> 511  b_Intercept 39.375074590 2.194090e-01
#> 512  b_Intercept 39.390167851 2.197818e-01
#> 513  b_Intercept 39.405261112 2.201269e-01
#> 514  b_Intercept 39.420354373 2.204435e-01
#> 515  b_Intercept 39.435447633 2.207335e-01
#> 516  b_Intercept 39.450540894 2.209965e-01
#> 517  b_Intercept 39.465634155 2.212320e-01
#> 518  b_Intercept 39.480727415 2.214393e-01
#> 519  b_Intercept 39.495820676 2.216171e-01
#> 520  b_Intercept 39.510913937 2.217605e-01
#> 521  b_Intercept 39.526007197 2.218748e-01
#> 522  b_Intercept 39.541100458 2.219601e-01
#> 523  b_Intercept 39.556193719 2.220162e-01
#> 524  b_Intercept 39.571286979 2.220433e-01
#> 525  b_Intercept 39.586380240 2.220415e-01
#> 526  b_Intercept 39.601473501 2.220055e-01
#> 527  b_Intercept 39.616566761 2.219405e-01
#> 528  b_Intercept 39.631660022 2.218480e-01
#> 529  b_Intercept 39.646753283 2.217283e-01
#> 530  b_Intercept 39.661846543 2.215822e-01
#> 531  b_Intercept 39.676939804 2.214103e-01
#> 532  b_Intercept 39.692033065 2.212104e-01
#> 533  b_Intercept 39.707126326 2.209839e-01
#> 534  b_Intercept 39.722219586 2.207344e-01
#> 535  b_Intercept 39.737312847 2.204629e-01
#> 536  b_Intercept 39.752406108 2.201702e-01
#> 537  b_Intercept 39.767499368 2.198573e-01
#> 538  b_Intercept 39.782592629 2.195242e-01
#> 539  b_Intercept 39.797685890 2.191699e-01
#> 540  b_Intercept 39.812779150 2.187990e-01
#> 541  b_Intercept 39.827872411 2.184122e-01
#> 542  b_Intercept 39.842965672 2.180107e-01
#> 543  b_Intercept 39.858058932 2.175952e-01
#> 544  b_Intercept 39.873152193 2.171669e-01
#> 545  b_Intercept 39.888245454 2.167242e-01
#> 546  b_Intercept 39.903338714 2.162705e-01
#> 547  b_Intercept 39.918431975 2.158068e-01
#> 548  b_Intercept 39.933525236 2.153337e-01
#> 549  b_Intercept 39.948618496 2.148521e-01
#> 550  b_Intercept 39.963711757 2.143623e-01
#> 551  b_Intercept 39.978805018 2.138641e-01
#> 552  b_Intercept 39.993898278 2.133584e-01
#> 553  b_Intercept 40.008991539 2.128461e-01
#> 554  b_Intercept 40.024084800 2.123276e-01
#> 555  b_Intercept 40.039178061 2.118031e-01
#> 556  b_Intercept 40.054271321 2.112726e-01
#> 557  b_Intercept 40.069364582 2.107361e-01
#> 558  b_Intercept 40.084457843 2.101926e-01
#> 559  b_Intercept 40.099551103 2.096430e-01
#> 560  b_Intercept 40.114644364 2.090873e-01
#> 561  b_Intercept 40.129737625 2.085251e-01
#> 562  b_Intercept 40.144830885 2.079562e-01
#> 563  b_Intercept 40.159924146 2.073802e-01
#> 564  b_Intercept 40.175017407 2.067950e-01
#> 565  b_Intercept 40.190110667 2.062014e-01
#> 566  b_Intercept 40.205203928 2.055991e-01
#> 567  b_Intercept 40.220297189 2.049877e-01
#> 568  b_Intercept 40.235390449 2.043665e-01
#> 569  b_Intercept 40.250483710 2.037350e-01
#> 570  b_Intercept 40.265576971 2.030911e-01
#> 571  b_Intercept 40.280670231 2.024345e-01
#> 572  b_Intercept 40.295763492 2.017657e-01
#> 573  b_Intercept 40.310856753 2.010841e-01
#> 574  b_Intercept 40.325950014 2.003894e-01
#> 575  b_Intercept 40.341043274 1.996810e-01
#> 576  b_Intercept 40.356136535 1.989576e-01
#> 577  b_Intercept 40.371229796 1.982172e-01
#> 578  b_Intercept 40.386323056 1.974618e-01
#> 579  b_Intercept 40.401416317 1.966912e-01
#> 580  b_Intercept 40.416509578 1.959053e-01
#> 581  b_Intercept 40.431602838 1.951038e-01
#> 582  b_Intercept 40.446696099 1.942869e-01
#> 583  b_Intercept 40.461789360 1.934510e-01
#> 584  b_Intercept 40.476882620 1.925995e-01
#> 585  b_Intercept 40.491975881 1.917329e-01
#> 586  b_Intercept 40.507069142 1.908514e-01
#> 587  b_Intercept 40.522162402 1.899553e-01
#> 588  b_Intercept 40.537255663 1.890450e-01
#> 589  b_Intercept 40.552348924 1.881192e-01
#> 590  b_Intercept 40.567442184 1.871794e-01
#> 591  b_Intercept 40.582535445 1.862273e-01
#> 592  b_Intercept 40.597628706 1.852639e-01
#> 593  b_Intercept 40.612721966 1.842897e-01
#> 594  b_Intercept 40.627815227 1.833056e-01
#> 595  b_Intercept 40.642908488 1.823119e-01
#> 596  b_Intercept 40.658001749 1.813091e-01
#> 597  b_Intercept 40.673095009 1.802996e-01
#> 598  b_Intercept 40.688188270 1.792844e-01
#> 599  b_Intercept 40.703281531 1.782645e-01
#> 600  b_Intercept 40.718374791 1.772410e-01
#> 601  b_Intercept 40.733468052 1.762147e-01
#> 602  b_Intercept 40.748561313 1.751869e-01
#> 603  b_Intercept 40.763654573 1.741590e-01
#> 604  b_Intercept 40.778747834 1.731322e-01
#> 605  b_Intercept 40.793841095 1.721074e-01
#> 606  b_Intercept 40.808934355 1.710856e-01
#> 607  b_Intercept 40.824027616 1.700677e-01
#> 608  b_Intercept 40.839120877 1.690556e-01
#> 609  b_Intercept 40.854214137 1.680501e-01
#> 610  b_Intercept 40.869307398 1.670517e-01
#> 611  b_Intercept 40.884400659 1.660610e-01
#> 612  b_Intercept 40.899493919 1.650788e-01
#> 613  b_Intercept 40.914587180 1.641057e-01
#> 614  b_Intercept 40.929680441 1.631431e-01
#> 615  b_Intercept 40.944773702 1.621924e-01
#> 616  b_Intercept 40.959866962 1.612527e-01
#> 617  b_Intercept 40.974960223 1.603241e-01
#> 618  b_Intercept 40.990053484 1.594069e-01
#> 619  b_Intercept 41.005146744 1.585014e-01
#> 620  b_Intercept 41.020240005 1.576076e-01
#> 621  b_Intercept 41.035333266 1.567284e-01
#> 622  b_Intercept 41.050426526 1.558608e-01
#> 623  b_Intercept 41.065519787 1.550047e-01
#> 624  b_Intercept 41.080613048 1.541600e-01
#> 625  b_Intercept 41.095706308 1.533262e-01
#> 626  b_Intercept 41.110799569 1.525032e-01
#> 627  b_Intercept 41.125892830 1.516919e-01
#> 628  b_Intercept 41.140986090 1.508909e-01
#> 629  b_Intercept 41.156079351 1.500987e-01
#> 630  b_Intercept 41.171172612 1.493149e-01
#> 631  b_Intercept 41.186265872 1.485386e-01
#> 632  b_Intercept 41.201359133 1.477694e-01
#> 633  b_Intercept 41.216452394 1.470067e-01
#> 634  b_Intercept 41.231545654 1.462500e-01
#> 635  b_Intercept 41.246638915 1.454974e-01
#> 636  b_Intercept 41.261732176 1.447482e-01
#> 637  b_Intercept 41.276825437 1.440015e-01
#> 638  b_Intercept 41.291918697 1.432565e-01
#> 639  b_Intercept 41.307011958 1.425123e-01
#> 640  b_Intercept 41.322105219 1.417679e-01
#> 641  b_Intercept 41.337198479 1.410221e-01
#> 642  b_Intercept 41.352291740 1.402742e-01
#> 643  b_Intercept 41.367385001 1.395234e-01
#> 644  b_Intercept 41.382478261 1.387689e-01
#> 645  b_Intercept 41.397571522 1.380101e-01
#> 646  b_Intercept 41.412664783 1.372451e-01
#> 647  b_Intercept 41.427758043 1.364737e-01
#> 648  b_Intercept 41.442851304 1.356956e-01
#> 649  b_Intercept 41.457944565 1.349103e-01
#> 650  b_Intercept 41.473037825 1.341172e-01
#> 651  b_Intercept 41.488131086 1.333159e-01
#> 652  b_Intercept 41.503224347 1.325053e-01
#> 653  b_Intercept 41.518317607 1.316842e-01
#> 654  b_Intercept 41.533410868 1.308538e-01
#> 655  b_Intercept 41.548504129 1.300138e-01
#> 656  b_Intercept 41.563597390 1.291640e-01
#> 657  b_Intercept 41.578690650 1.283044e-01
#> 658  b_Intercept 41.593783911 1.274348e-01
#> 659  b_Intercept 41.608877172 1.265531e-01
#> 660  b_Intercept 41.623970432 1.256616e-01
#> 661  b_Intercept 41.639063693 1.247604e-01
#> 662  b_Intercept 41.654156954 1.238497e-01
#> 663  b_Intercept 41.669250214 1.229297e-01
#> 664  b_Intercept 41.684343475 1.220006e-01
#> 665  b_Intercept 41.699436736 1.210612e-01
#> 666  b_Intercept 41.714529996 1.201130e-01
#> 667  b_Intercept 41.729623257 1.191568e-01
#> 668  b_Intercept 41.744716518 1.181930e-01
#> 669  b_Intercept 41.759809778 1.172219e-01
#> 670  b_Intercept 41.774903039 1.162441e-01
#> 671  b_Intercept 41.789996300 1.152593e-01
#> 672  b_Intercept 41.805089560 1.142679e-01
#> 673  b_Intercept 41.820182821 1.132712e-01
#> 674  b_Intercept 41.835276082 1.122696e-01
#> 675  b_Intercept 41.850369343 1.112636e-01
#> 676  b_Intercept 41.865462603 1.102536e-01
#> 677  b_Intercept 41.880555864 1.092399e-01
#> 678  b_Intercept 41.895649125 1.082226e-01
#> 679  b_Intercept 41.910742385 1.072027e-01
#> 680  b_Intercept 41.925835646 1.061806e-01
#> 681  b_Intercept 41.940928907 1.051568e-01
#> 682  b_Intercept 41.956022167 1.041314e-01
#> 683  b_Intercept 41.971115428 1.031050e-01
#> 684  b_Intercept 41.986208689 1.020778e-01
#> 685  b_Intercept 42.001301949 1.010502e-01
#> 686  b_Intercept 42.016395210 1.000225e-01
#> 687  b_Intercept 42.031488471 9.899513e-02
#> 688  b_Intercept 42.046581731 9.796819e-02
#> 689  b_Intercept 42.061674992 9.694197e-02
#> 690  b_Intercept 42.076768253 9.591680e-02
#> 691  b_Intercept 42.091861513 9.489301e-02
#> 692  b_Intercept 42.106954774 9.387067e-02
#> 693  b_Intercept 42.122048035 9.284994e-02
#> 694  b_Intercept 42.137141295 9.183101e-02
#> 695  b_Intercept 42.152234556 9.081403e-02
#> 696  b_Intercept 42.167327817 8.979924e-02
#> 697  b_Intercept 42.182421078 8.878724e-02
#> 698  b_Intercept 42.197514338 8.777776e-02
#> 699  b_Intercept 42.212607599 8.677095e-02
#> 700  b_Intercept 42.227700860 8.576696e-02
#> 701  b_Intercept 42.242794120 8.476595e-02
#> 702  b_Intercept 42.257887381 8.376806e-02
#> 703  b_Intercept 42.272980642 8.277412e-02
#> 704  b_Intercept 42.288073902 8.178389e-02
#> 705  b_Intercept 42.303167163 8.079738e-02
#> 706  b_Intercept 42.318260424 7.981478e-02
#> 707  b_Intercept 42.333353684 7.883627e-02
#> 708  b_Intercept 42.348446945 7.786202e-02
#> 709  b_Intercept 42.363540206 7.689276e-02
#> 710  b_Intercept 42.378633466 7.592884e-02
#> 711  b_Intercept 42.393726727 7.496991e-02
#> 712  b_Intercept 42.408819988 7.401619e-02
#> 713  b_Intercept 42.423913248 7.306788e-02
#> 714  b_Intercept 42.439006509 7.212521e-02
#> 715  b_Intercept 42.454099770 7.118860e-02
#> 716  b_Intercept 42.469193031 7.025935e-02
#> 717  b_Intercept 42.484286291 6.933652e-02
#> 718  b_Intercept 42.499379552 6.842033e-02
#> 719  b_Intercept 42.514472813 6.751100e-02
#> 720  b_Intercept 42.529566073 6.660874e-02
#> 721  b_Intercept 42.544659334 6.571375e-02
#> 722  b_Intercept 42.559752595 6.482771e-02
#> 723  b_Intercept 42.574845855 6.394978e-02
#> 724  b_Intercept 42.589939116 6.307982e-02
#> 725  b_Intercept 42.605032377 6.221800e-02
#> 726  b_Intercept 42.620125637 6.136449e-02
#> 727  b_Intercept 42.635218898 6.051945e-02
#> 728  b_Intercept 42.650312159 5.968405e-02
#> 729  b_Intercept 42.665405419 5.885848e-02
#> 730  b_Intercept 42.680498680 5.804187e-02
#> 731  b_Intercept 42.695591941 5.723431e-02
#> 732  b_Intercept 42.710685201 5.643591e-02
#> 733  b_Intercept 42.725778462 5.564676e-02
#> 734  b_Intercept 42.740871723 5.486731e-02
#> 735  b_Intercept 42.755964983 5.409907e-02
#> 736  b_Intercept 42.771058244 5.334030e-02
#> 737  b_Intercept 42.786151505 5.259104e-02
#> 738  b_Intercept 42.801244766 5.185134e-02
#> 739  b_Intercept 42.816338026 5.112120e-02
#> 740  b_Intercept 42.831431287 5.040065e-02
#> 741  b_Intercept 42.846524548 4.969158e-02
#> 742  b_Intercept 42.861617808 4.899244e-02
#> 743  b_Intercept 42.876711069 4.830290e-02
#> 744  b_Intercept 42.891804330 4.762292e-02
#> 745  b_Intercept 42.906897590 4.695251e-02
#> 746  b_Intercept 42.921990851 4.629164e-02
#> 747  b_Intercept 42.937084112 4.564144e-02
#> 748  b_Intercept 42.952177372 4.500174e-02
#> 749  b_Intercept 42.967270633 4.437149e-02
#> 750  b_Intercept 42.982363894 4.375063e-02
#> 751  b_Intercept 42.997457154 4.313914e-02
#> 752  b_Intercept 43.012550415 4.253697e-02
#> 753  b_Intercept 43.027643676 4.194452e-02
#> 754  b_Intercept 43.042736936 4.136299e-02
#> 755  b_Intercept 43.057830197 4.079062e-02
#> 756  b_Intercept 43.072923458 4.022736e-02
#> 757  b_Intercept 43.088016719 3.967315e-02
#> 758  b_Intercept 43.103109979 3.912794e-02
#> 759  b_Intercept 43.118203240 3.859165e-02
#> 760  b_Intercept 43.133296501 3.806600e-02
#> 761  b_Intercept 43.148389761 3.754934e-02
#> 762  b_Intercept 43.163483022 3.704132e-02
#> 763  b_Intercept 43.178576283 3.654187e-02
#> 764  b_Intercept 43.193669543 3.605087e-02
#> 765  b_Intercept 43.208762804 3.556822e-02
#> 766  b_Intercept 43.223856065 3.509484e-02
#> 767  b_Intercept 43.238949325 3.463033e-02
#> 768  b_Intercept 43.254042586 3.417369e-02
#> 769  b_Intercept 43.269135847 3.372478e-02
#> 770  b_Intercept 43.284229107 3.328343e-02
#> 771  b_Intercept 43.299322368 3.284945e-02
#> 772  b_Intercept 43.314415629 3.242304e-02
#> 773  b_Intercept 43.329508889 3.200477e-02
#> 774  b_Intercept 43.344602150 3.159315e-02
#> 775  b_Intercept 43.359695411 3.118794e-02
#> 776  b_Intercept 43.374788671 3.078890e-02
#> 777  b_Intercept 43.389881932 3.039577e-02
#> 778  b_Intercept 43.404975193 3.000830e-02
#> 779  b_Intercept 43.420068454 2.962725e-02
#> 780  b_Intercept 43.435161714 2.925124e-02
#> 781  b_Intercept 43.450254975 2.887987e-02
#> 782  b_Intercept 43.465348236 2.851282e-02
#> 783  b_Intercept 43.480441496 2.814981e-02
#> 784  b_Intercept 43.495534757 2.779051e-02
#> 785  b_Intercept 43.510628018 2.743501e-02
#> 786  b_Intercept 43.525721278 2.708275e-02
#> 787  b_Intercept 43.540814539 2.673307e-02
#> 788  b_Intercept 43.555907800 2.638568e-02
#> 789  b_Intercept 43.571001060 2.604027e-02
#> 790  b_Intercept 43.586094321 2.569654e-02
#> 791  b_Intercept 43.601187582 2.535425e-02
#> 792  b_Intercept 43.616280842 2.501313e-02
#> 793  b_Intercept 43.631374103 2.467267e-02
#> 794  b_Intercept 43.646467364 2.433264e-02
#> 795  b_Intercept 43.661560624 2.399278e-02
#> 796  b_Intercept 43.676653885 2.365288e-02
#> 797  b_Intercept 43.691747146 2.331272e-02
#> 798  b_Intercept 43.706840407 2.297196e-02
#> 799  b_Intercept 43.721933667 2.263050e-02
#> 800  b_Intercept 43.737026928 2.228819e-02
#> 801  b_Intercept 43.752120189 2.194494e-02
#> 802  b_Intercept 43.767213449 2.160064e-02
#> 803  b_Intercept 43.782306710 2.125523e-02
#> 804  b_Intercept 43.797399971 2.090849e-02
#> 805  b_Intercept 43.812493231 2.056046e-02
#> 806  b_Intercept 43.827586492 2.021126e-02
#> 807  b_Intercept 43.842679753 1.986094e-02
#> 808  b_Intercept 43.857773013 1.950957e-02
#> 809  b_Intercept 43.872866274 1.915722e-02
#> 810  b_Intercept 43.887959535 1.880396e-02
#> 811  b_Intercept 43.903052795 1.844992e-02
#> 812  b_Intercept 43.918146056 1.809540e-02
#> 813  b_Intercept 43.933239317 1.774059e-02
#> 814  b_Intercept 43.948332577 1.738569e-02
#> 815  b_Intercept 43.963425838 1.703092e-02
#> 816  b_Intercept 43.978519099 1.667652e-02
#> 817  b_Intercept 43.993612360 1.632296e-02
#> 818  b_Intercept 44.008705620 1.597044e-02
#> 819  b_Intercept 44.023798881 1.561925e-02
#> 820  b_Intercept 44.038892142 1.526965e-02
#> 821  b_Intercept 44.053985402 1.492195e-02
#> 822  b_Intercept 44.069078663 1.457643e-02
#> 823  b_Intercept 44.084171924 1.423385e-02
#> 824  b_Intercept 44.099265184 1.389444e-02
#> 825  b_Intercept 44.114358445 1.355829e-02
#> 826  b_Intercept 44.129451706 1.322569e-02
#> 827  b_Intercept 44.144544966 1.289693e-02
#> 828  b_Intercept 44.159638227 1.257229e-02
#> 829  b_Intercept 44.174731488 1.225238e-02
#> 830  b_Intercept 44.189824748 1.193797e-02
#> 831  b_Intercept 44.204918009 1.162858e-02
#> 832  b_Intercept 44.220011270 1.132445e-02
#> 833  b_Intercept 44.235104530 1.102577e-02
#> 834  b_Intercept 44.250197791 1.073274e-02
#> 835  b_Intercept 44.265291052 1.044555e-02
#> 836  b_Intercept 44.280384312 1.016576e-02
#> 837  b_Intercept 44.295477573 9.892174e-03
#> 838  b_Intercept 44.310570834 9.624881e-03
#> 839  b_Intercept 44.325664095 9.363970e-03
#> 840  b_Intercept 44.340757355 9.109505e-03
#> 841  b_Intercept 44.355850616 8.861536e-03
#> 842  b_Intercept 44.370943877 8.621112e-03
#> 843  b_Intercept 44.386037137 8.387710e-03
#> 844  b_Intercept 44.401130398 8.160811e-03
#> 845  b_Intercept 44.416223659 7.940373e-03
#> 846  b_Intercept 44.431316919 7.726340e-03
#> 847  b_Intercept 44.446410180 7.518639e-03
#> 848  b_Intercept 44.461503441 7.317676e-03
#> 849  b_Intercept 44.476596701 7.123723e-03
#> 850  b_Intercept 44.491689962 6.935716e-03
#> 851  b_Intercept 44.506783223 6.753517e-03
#> 852  b_Intercept 44.521876483 6.576978e-03
#> 853  b_Intercept 44.536969744 6.405944e-03
#> 854  b_Intercept 44.552063005 6.240283e-03
#> 855  b_Intercept 44.567156265 6.080890e-03
#> 856  b_Intercept 44.582249526 5.926382e-03
#> 857  b_Intercept 44.597342787 5.776575e-03
#> 858  b_Intercept 44.612436048 5.631283e-03
#> 859  b_Intercept 44.627529308 5.490321e-03
#> 860  b_Intercept 44.642622569 5.353501e-03
#> 861  b_Intercept 44.657715830 5.221234e-03
#> 862  b_Intercept 44.672809090 5.092898e-03
#> 863  b_Intercept 44.687902351 4.968045e-03
#> 864  b_Intercept 44.702995612 4.846500e-03
#> 865  b_Intercept 44.718088872 4.728094e-03
#> 866  b_Intercept 44.733182133 4.612662e-03
#> 867  b_Intercept 44.748275394 4.500274e-03
#> 868  b_Intercept 44.763368654 4.390862e-03
#> 869  b_Intercept 44.778461915 4.283890e-03
#> 870  b_Intercept 44.793555176 4.179225e-03
#> 871  b_Intercept 44.808648436 4.076745e-03
#> 872  b_Intercept 44.823741697 3.976337e-03
#> 873  b_Intercept 44.838834958 3.877915e-03
#> 874  b_Intercept 44.853928218 3.781731e-03
#> 875  b_Intercept 44.869021479 3.687276e-03
#> 876  b_Intercept 44.884114740 3.594476e-03
#> 877  b_Intercept 44.899208000 3.503264e-03
#> 878  b_Intercept 44.914301261 3.413582e-03
#> 879  b_Intercept 44.929394522 3.325379e-03
#> 880  b_Intercept 44.944487783 3.238845e-03
#> 881  b_Intercept 44.959581043 3.153781e-03
#> 882  b_Intercept 44.974674304 3.070077e-03
#> 883  b_Intercept 44.989767565 2.987714e-03
#> 884  b_Intercept 45.004860825 2.906682e-03
#> 885  b_Intercept 45.019954086 2.826973e-03
#> 886  b_Intercept 45.035047347 2.748712e-03
#> 887  b_Intercept 45.050140607 2.671962e-03
#> 888  b_Intercept 45.065233868 2.596555e-03
#> 889  b_Intercept 45.080327129 2.522504e-03
#> 890  b_Intercept 45.095420389 2.449828e-03
#> 891  b_Intercept 45.110513650 2.378547e-03
#> 892  b_Intercept 45.125606911 2.308712e-03
#> 893  b_Intercept 45.140700171 2.240635e-03
#> 894  b_Intercept 45.155793432 2.174045e-03
#> 895  b_Intercept 45.170886693 2.108968e-03
#> 896  b_Intercept 45.185979953 2.045434e-03
#> 897  b_Intercept 45.201073214 1.983472e-03
#> 898  b_Intercept 45.216166475 1.923112e-03
#> 899  b_Intercept 45.231259736 1.864677e-03
#> 900  b_Intercept 45.246352996 1.808007e-03
#> 901  b_Intercept 45.261446257 1.753039e-03
#> 902  b_Intercept 45.276539518 1.699798e-03
#> 903  b_Intercept 45.291632778 1.648307e-03
#> 904  b_Intercept 45.306726039 1.598589e-03
#> 905  b_Intercept 45.321819300 1.550849e-03
#> 906  b_Intercept 45.336912560 1.505163e-03
#> 907  b_Intercept 45.352005821 1.461311e-03
#> 908  b_Intercept 45.367099082 1.419306e-03
#> 909  b_Intercept 45.382192342 1.379155e-03
#> 910  b_Intercept 45.397285603 1.340865e-03
#> 911  b_Intercept 45.412378864 1.304490e-03
#> 912  b_Intercept 45.427472124 1.270361e-03
#> 913  b_Intercept 45.442565385 1.238090e-03
#> 914  b_Intercept 45.457658646 1.207671e-03
#> 915  b_Intercept 45.472751906 1.179091e-03
#> 916  b_Intercept 45.487845167 1.152336e-03
#> 917  b_Intercept 45.502938428 1.127389e-03
#> 918  b_Intercept 45.518031688 1.104550e-03
#> 919  b_Intercept 45.533124949 1.083544e-03
#> 920  b_Intercept 45.548218210 1.064254e-03
#> 921  b_Intercept 45.563311471 1.046649e-03
#> 922  b_Intercept 45.578404731 1.030691e-03
#> 923  b_Intercept 45.593497992 1.016342e-03
#> 924  b_Intercept 45.608591253 1.003722e-03
#> 925  b_Intercept 45.623684513 9.927868e-04
#> 926  b_Intercept 45.638777774 9.832930e-04
#> 927  b_Intercept 45.653871035 9.751872e-04
#> 928  b_Intercept 45.668964295 9.684133e-04
#> 929  b_Intercept 45.684057556 9.629128e-04
#> 930  b_Intercept 45.699150817 9.586621e-04
#> 931  b_Intercept 45.714244077 9.557610e-04
#> 932  b_Intercept 45.729337338 9.539022e-04
#> 933  b_Intercept 45.744430599 9.530162e-04
#> 934  b_Intercept 45.759523859 9.530321e-04
#> 935  b_Intercept 45.774617120 9.538776e-04
#> 936  b_Intercept 45.789710381 9.554791e-04
#> 937  b_Intercept 45.804803641 9.578687e-04
#> 938  b_Intercept 45.819896902 9.608453e-04
#> 939  b_Intercept 45.834990163 9.643046e-04
#> 940  b_Intercept 45.850083424 9.681698e-04
#> 941  b_Intercept 45.865176684 9.723643e-04
#> 942  b_Intercept 45.880269945 9.768116e-04
#> 943  b_Intercept 45.895363206 9.814439e-04
#> 944  b_Intercept 45.910456466 9.861536e-04
#> 945  b_Intercept 45.925549727 9.908492e-04
#> 946  b_Intercept 45.940642988 9.954604e-04
#> 947  b_Intercept 45.955736248 9.999190e-04
#> 948  b_Intercept 45.970829509 1.004159e-03
#> 949  b_Intercept 45.985922770 1.008101e-03
#> 950  b_Intercept 46.001016030 1.011611e-03
#> 951  b_Intercept 46.016109291 1.014690e-03
#> 952  b_Intercept 46.031202552 1.017285e-03
#> 953  b_Intercept 46.046295812 1.019352e-03
#> 954  b_Intercept 46.061389073 1.020848e-03
#> 955  b_Intercept 46.076482334 1.021733e-03
#> 956  b_Intercept 46.091575594 1.021838e-03
#> 957  b_Intercept 46.106668855 1.021233e-03
#> 958  b_Intercept 46.121762116 1.019920e-03
#> 959  b_Intercept 46.136855376 1.017886e-03
#> 960  b_Intercept 46.151948637 1.015118e-03
#> 961  b_Intercept 46.167041898 1.011613e-03
#> 962  b_Intercept 46.182135159 1.007279e-03
#> 963  b_Intercept 46.197228419 1.002137e-03
#> 964  b_Intercept 46.212321680 9.962804e-04
#> 965  b_Intercept 46.227414941 9.897290e-04
#> 966  b_Intercept 46.242508201 9.825055e-04
#> 967  b_Intercept 46.257601462 9.746368e-04
#> 968  b_Intercept 46.272694723 9.661276e-04
#> 969  b_Intercept 46.287787983 9.569492e-04
#> 970  b_Intercept 46.302881244 9.472568e-04
#> 971  b_Intercept 46.317974505 9.370950e-04
#> 972  b_Intercept 46.333067765 9.265107e-04
#> 973  b_Intercept 46.348161026 9.155530e-04
#> 974  b_Intercept 46.363254287 9.042728e-04
#> 975  b_Intercept 46.378347547 8.926850e-04
#> 976  b_Intercept 46.393440808 8.809063e-04
#> 977  b_Intercept 46.408534069 8.689985e-04
#> 978  b_Intercept 46.423627329 8.570156e-04
#> 979  b_Intercept 46.438720590 8.450111e-04
#> 980  b_Intercept 46.453813851 8.330378e-04
#> 981  b_Intercept 46.468907112 8.211670e-04
#> 982  b_Intercept 46.484000372 8.094661e-04
#> 983  b_Intercept 46.499093633 7.979708e-04
#> 984  b_Intercept 46.514186894 7.867233e-04
#> 985  b_Intercept 46.529280154 7.757631e-04
#> 986  b_Intercept 46.544373415 7.651263e-04
#> 987  b_Intercept 46.559466676 7.548669e-04
#> 988  b_Intercept 46.574559936 7.450713e-04
#> 989  b_Intercept 46.589653197 7.356966e-04
#> 990  b_Intercept 46.604746458 7.267591e-04
#> 991  b_Intercept 46.619839718 7.182713e-04
#> 992  b_Intercept 46.634932979 7.102415e-04
#> 993  b_Intercept 46.650026240 7.026738e-04
#> 994  b_Intercept 46.665119500 6.956628e-04
#> 995  b_Intercept 46.680212761 6.891119e-04
#> 996  b_Intercept 46.695306022 6.829982e-04
#> 997  b_Intercept 46.710399282 6.773049e-04
#> 998  b_Intercept 46.725492543 6.720118e-04
#> 999  b_Intercept 46.740585804 6.670952e-04
#> 1000 b_Intercept 46.755679065 6.625690e-04
#> 1001 b_Intercept 46.770772325 6.583749e-04
#> 1002 b_Intercept 46.785865586 6.544444e-04
#> 1003 b_Intercept 46.800958847 6.507399e-04
#> 1004 b_Intercept 46.816052107 6.472224e-04
#> 1005 b_Intercept 46.831145368 6.438514e-04
#> 1006 b_Intercept 46.846238629 6.405879e-04
#> 1007 b_Intercept 46.861331889 6.373781e-04
#> 1008 b_Intercept 46.876425150 6.341615e-04
#> 1009 b_Intercept 46.891518411 6.308952e-04
#> 1010 b_Intercept 46.906611671 6.275372e-04
#> 1011 b_Intercept 46.921704932 6.240460e-04
#> 1012 b_Intercept 46.936798193 6.203815e-04
#> 1013 b_Intercept 46.951891453 6.164465e-04
#> 1014 b_Intercept 46.966984714 6.122388e-04
#> 1015 b_Intercept 46.982077975 6.077294e-04
#> 1016 b_Intercept 46.997171235 6.028879e-04
#> 1017 b_Intercept 47.012264496 5.976871e-04
#> 1018 b_Intercept 47.027357757 5.921020e-04
#> 1019 b_Intercept 47.042451017 5.860494e-04
#> 1020 b_Intercept 47.057544278 5.795267e-04
#> 1021 b_Intercept 47.072637539 5.725586e-04
#> 1022 b_Intercept 47.087730800 5.651363e-04
#> 1023 b_Intercept 47.102824060 5.572541e-04
#> 1024 b_Intercept 47.117917321 5.489094e-04
#> 1025        b_wt -6.882027746 7.178788e-04
#> 1026        b_wt -6.875683507 7.244908e-04
#> 1027        b_wt -6.869339268 7.304031e-04
#> 1028        b_wt -6.862995028 7.357482e-04
#> 1029        b_wt -6.856650789 7.405577e-04
#> 1030        b_wt -6.850306549 7.448527e-04
#> 1031        b_wt -6.843962310 7.486560e-04
#> 1032        b_wt -6.837618071 7.519925e-04
#> 1033        b_wt -6.831273831 7.548166e-04
#> 1034        b_wt -6.824929592 7.572221e-04
#> 1035        b_wt -6.818585352 7.592649e-04
#> 1036        b_wt -6.812241113 7.609759e-04
#> 1037        b_wt -6.805896873 7.623862e-04
#> 1038        b_wt -6.799552634 7.635272e-04
#> 1039        b_wt -6.793208395 7.644019e-04
#> 1040        b_wt -6.786864155 7.650659e-04
#> 1041        b_wt -6.780519916 7.655699e-04
#> 1042        b_wt -6.774175676 7.659411e-04
#> 1043        b_wt -6.767831437 7.662052e-04
#> 1044        b_wt -6.761487198 7.663864e-04
#> 1045        b_wt -6.755142958 7.665032e-04
#> 1046        b_wt -6.748798719 7.665820e-04
#> 1047        b_wt -6.742454479 7.666448e-04
#> 1048        b_wt -6.736110240 7.667035e-04
#> 1049        b_wt -6.729766000 7.667674e-04
#> 1050        b_wt -6.723421761 7.668429e-04
#> 1051        b_wt -6.717077522 7.669349e-04
#> 1052        b_wt -6.710733282 7.670438e-04
#> 1053        b_wt -6.704389043 7.671615e-04
#> 1054        b_wt -6.698044803 7.672799e-04
#> 1055        b_wt -6.691700564 7.673884e-04
#> 1056        b_wt -6.685356325 7.674739e-04
#> 1057        b_wt -6.679012085 7.675187e-04
#> 1058        b_wt -6.672667846 7.674871e-04
#> 1059        b_wt -6.666323606 7.673652e-04
#> 1060        b_wt -6.659979367 7.671300e-04
#> 1061        b_wt -6.653635128 7.667574e-04
#> 1062        b_wt -6.647290888 7.662221e-04
#> 1063        b_wt -6.640946649 7.654980e-04
#> 1064        b_wt -6.634602409 7.644974e-04
#> 1065        b_wt -6.628258170 7.632392e-04
#> 1066        b_wt -6.621913930 7.616979e-04
#> 1067        b_wt -6.615569691 7.598489e-04
#> 1068        b_wt -6.609225452 7.576688e-04
#> 1069        b_wt -6.602881212 7.551353e-04
#> 1070        b_wt -6.596536973 7.521447e-04
#> 1071        b_wt -6.590192733 7.487394e-04
#> 1072        b_wt -6.583848494 7.449190e-04
#> 1073        b_wt -6.577504255 7.406726e-04
#> 1074        b_wt -6.571160015 7.359922e-04
#> 1075        b_wt -6.564815776 7.308726e-04
#> 1076        b_wt -6.558471536 7.252373e-04
#> 1077        b_wt -6.552127297 7.191355e-04
#> 1078        b_wt -6.545783057 7.126072e-04
#> 1079        b_wt -6.539438818 7.056642e-04
#> 1080        b_wt -6.533094579 6.983223e-04
#> 1081        b_wt -6.526750339 6.906000e-04
#> 1082        b_wt -6.520406100 6.824768e-04
#> 1083        b_wt -6.514061860 6.740029e-04
#> 1084        b_wt -6.507717621 6.652481e-04
#> 1085        b_wt -6.501373382 6.562478e-04
#> 1086        b_wt -6.495029142 6.470397e-04
#> 1087        b_wt -6.488684903 6.376641e-04
#> 1088        b_wt -6.482340663 6.281575e-04
#> 1089        b_wt -6.475996424 6.185835e-04
#> 1090        b_wt -6.469652184 6.090074e-04
#> 1091        b_wt -6.463307945 5.994785e-04
#> 1092        b_wt -6.456963706 5.900470e-04
#> 1093        b_wt -6.450619466 5.807638e-04
#> 1094        b_wt -6.444275227 5.716970e-04
#> 1095        b_wt -6.437930987 5.629494e-04
#> 1096        b_wt -6.431586748 5.545349e-04
#> 1097        b_wt -6.425242509 5.465027e-04
#> 1098        b_wt -6.418898269 5.389005e-04
#> 1099        b_wt -6.412554030 5.317750e-04
#> 1100        b_wt -6.406209790 5.251862e-04
#> 1101        b_wt -6.399865551 5.192987e-04
#> 1102        b_wt -6.393521311 5.140377e-04
#> 1103        b_wt -6.387177072 5.094376e-04
#> 1104        b_wt -6.380832833 5.055297e-04
#> 1105        b_wt -6.374488593 5.023425e-04
#> 1106        b_wt -6.368144354 4.999012e-04
#> 1107        b_wt -6.361800114 4.984114e-04
#> 1108        b_wt -6.355455875 4.977214e-04
#> 1109        b_wt -6.349111636 4.978340e-04
#> 1110        b_wt -6.342767396 4.987557e-04
#> 1111        b_wt -6.336423157 5.004892e-04
#> 1112        b_wt -6.330078917 5.030336e-04
#> 1113        b_wt -6.323734678 5.065392e-04
#> 1114        b_wt -6.317390438 5.108699e-04
#> 1115        b_wt -6.311046199 5.159736e-04
#> 1116        b_wt -6.304701960 5.218314e-04
#> 1117        b_wt -6.298357720 5.284212e-04
#> 1118        b_wt -6.292013481 5.357184e-04
#> 1119        b_wt -6.285669241 5.437965e-04
#> 1120        b_wt -6.279325002 5.525577e-04
#> 1121        b_wt -6.272980763 5.619127e-04
#> 1122        b_wt -6.266636523 5.718253e-04
#> 1123        b_wt -6.260292284 5.822579e-04
#> 1124        b_wt -6.253948044 5.931720e-04
#> 1125        b_wt -6.247603805 6.045753e-04
#> 1126        b_wt -6.241259565 6.164057e-04
#> 1127        b_wt -6.234915326 6.285720e-04
#> 1128        b_wt -6.228571087 6.410343e-04
#> 1129        b_wt -6.222226847 6.537537e-04
#> 1130        b_wt -6.215882608 6.666919e-04
#> 1131        b_wt -6.209538368 6.798236e-04
#> 1132        b_wt -6.203194129 6.931079e-04
#> 1133        b_wt -6.196849890 7.064864e-04
#> 1134        b_wt -6.190505650 7.199302e-04
#> 1135        b_wt -6.184161411 7.334126e-04
#> 1136        b_wt -6.177817171 7.469098e-04
#> 1137        b_wt -6.171472932 7.603989e-04
#> 1138        b_wt -6.165128692 7.738524e-04
#> 1139        b_wt -6.158784453 7.872606e-04
#> 1140        b_wt -6.152440214 8.006152e-04
#> 1141        b_wt -6.146095974 8.139115e-04
#> 1142        b_wt -6.139751735 8.271476e-04
#> 1143        b_wt -6.133407495 8.403243e-04
#> 1144        b_wt -6.127063256 8.534391e-04
#> 1145        b_wt -6.120719017 8.665173e-04
#> 1146        b_wt -6.114374777 8.795738e-04
#> 1147        b_wt -6.108030538 8.926262e-04
#> 1148        b_wt -6.101686298 9.056948e-04
#> 1149        b_wt -6.095342059 9.188027e-04
#> 1150        b_wt -6.088997819 9.319999e-04
#> 1151        b_wt -6.082653580 9.453112e-04
#> 1152        b_wt -6.076309341 9.587672e-04
#> 1153        b_wt -6.069965101 9.724016e-04
#> 1154        b_wt -6.063620862 9.862495e-04
#> 1155        b_wt -6.057276622 1.000347e-03
#> 1156        b_wt -6.050932383 1.014796e-03
#> 1157        b_wt -6.044588144 1.029612e-03
#> 1158        b_wt -6.038243904 1.044817e-03
#> 1159        b_wt -6.031899665 1.060452e-03
#> 1160        b_wt -6.025555425 1.076555e-03
#> 1161        b_wt -6.019211186 1.093168e-03
#> 1162        b_wt -6.012866947 1.110418e-03
#> 1163        b_wt -6.006522707 1.128331e-03
#> 1164        b_wt -6.000178468 1.146895e-03
#> 1165        b_wt -5.993834228 1.166147e-03
#> 1166        b_wt -5.987489989 1.186125e-03
#> 1167        b_wt -5.981145749 1.206867e-03
#> 1168        b_wt -5.974801510 1.228505e-03
#> 1169        b_wt -5.968457271 1.251108e-03
#> 1170        b_wt -5.962113031 1.274608e-03
#> 1171        b_wt -5.955768792 1.299042e-03
#> 1172        b_wt -5.949424552 1.324450e-03
#> 1173        b_wt -5.943080313 1.350868e-03
#> 1174        b_wt -5.936736074 1.378422e-03
#> 1175        b_wt -5.930391834 1.407268e-03
#> 1176        b_wt -5.924047595 1.437277e-03
#> 1177        b_wt -5.917703355 1.468496e-03
#> 1178        b_wt -5.911359116 1.500974e-03
#> 1179        b_wt -5.905014876 1.534765e-03
#> 1180        b_wt -5.898670637 1.569982e-03
#> 1181        b_wt -5.892326398 1.606942e-03
#> 1182        b_wt -5.885982158 1.645439e-03
#> 1183        b_wt -5.879637919 1.685547e-03
#> 1184        b_wt -5.873293679 1.727345e-03
#> 1185        b_wt -5.866949440 1.770917e-03
#> 1186        b_wt -5.860605201 1.816362e-03
#> 1187        b_wt -5.854260961 1.864271e-03
#> 1188        b_wt -5.847916722 1.914316e-03
#> 1189        b_wt -5.841572482 1.966611e-03
#> 1190        b_wt -5.835228243 2.021281e-03
#> 1191        b_wt -5.828884003 2.078455e-03
#> 1192        b_wt -5.822539764 2.138270e-03
#> 1193        b_wt -5.816195525 2.201504e-03
#> 1194        b_wt -5.809851285 2.267863e-03
#> 1195        b_wt -5.803507046 2.337424e-03
#> 1196        b_wt -5.797162806 2.410358e-03
#> 1197        b_wt -5.790818567 2.486839e-03
#> 1198        b_wt -5.784474328 2.567049e-03
#> 1199        b_wt -5.778130088 2.651906e-03
#> 1200        b_wt -5.771785849 2.741262e-03
#> 1201        b_wt -5.765441609 2.835047e-03
#> 1202        b_wt -5.759097370 2.933463e-03
#> 1203        b_wt -5.752753130 3.036712e-03
#> 1204        b_wt -5.746408891 3.144997e-03
#> 1205        b_wt -5.740064652 3.259284e-03
#> 1206        b_wt -5.733720412 3.379684e-03
#> 1207        b_wt -5.727376173 3.505850e-03
#> 1208        b_wt -5.721031933 3.637974e-03
#> 1209        b_wt -5.714687694 3.776243e-03
#> 1210        b_wt -5.708343455 3.920840e-03
#> 1211        b_wt -5.701999215 4.072622e-03
#> 1212        b_wt -5.695654976 4.232079e-03
#> 1213        b_wt -5.689310736 4.398445e-03
#> 1214        b_wt -5.682966497 4.571851e-03
#> 1215        b_wt -5.676622257 4.752415e-03
#> 1216        b_wt -5.670278018 4.940244e-03
#> 1217        b_wt -5.663933779 5.135915e-03
#> 1218        b_wt -5.657589539 5.340342e-03
#> 1219        b_wt -5.651245300 5.552267e-03
#> 1220        b_wt -5.644901060 5.771709e-03
#> 1221        b_wt -5.638556821 5.998665e-03
#> 1222        b_wt -5.632212582 6.233116e-03
#> 1223        b_wt -5.625868342 6.475238e-03
#> 1224        b_wt -5.619524103 6.726273e-03
#> 1225        b_wt -5.613179863 6.984538e-03
#> 1226        b_wt -5.606835624 7.249901e-03
#> 1227        b_wt -5.600491384 7.522212e-03
#> 1228        b_wt -5.594147145 7.801302e-03
#> 1229        b_wt -5.587802906 8.086977e-03
#> 1230        b_wt -5.581458666 8.380429e-03
#> 1231        b_wt -5.575114427 8.679895e-03
#> 1232        b_wt -5.568770187 8.985060e-03
#> 1233        b_wt -5.562425948 9.295636e-03
#> 1234        b_wt -5.556081709 9.611319e-03
#> 1235        b_wt -5.549737469 9.931790e-03
#> 1236        b_wt -5.543393230 1.025749e-02
#> 1237        b_wt -5.537048990 1.058726e-02
#> 1238        b_wt -5.530704751 1.092054e-02
#> 1239        b_wt -5.524360511 1.125697e-02
#> 1240        b_wt -5.518016272 1.159616e-02
#> 1241        b_wt -5.511672033 1.193775e-02
#> 1242        b_wt -5.505327793 1.228158e-02
#> 1243        b_wt -5.498983554 1.262695e-02
#> 1244        b_wt -5.492639314 1.297334e-02
#> 1245        b_wt -5.486295075 1.332040e-02
#> 1246        b_wt -5.479950836 1.366776e-02
#> 1247        b_wt -5.473606596 1.401509e-02
#> 1248        b_wt -5.467262357 1.436195e-02
#> 1249        b_wt -5.460918117 1.470791e-02
#> 1250        b_wt -5.454573878 1.505273e-02
#> 1251        b_wt -5.448229638 1.539617e-02
#> 1252        b_wt -5.441885399 1.573800e-02
#> 1253        b_wt -5.435541160 1.607800e-02
#> 1254        b_wt -5.429196920 1.641579e-02
#> 1255        b_wt -5.422852681 1.675104e-02
#> 1256        b_wt -5.416508441 1.708396e-02
#> 1257        b_wt -5.410164202 1.741449e-02
#> 1258        b_wt -5.403819963 1.774258e-02
#> 1259        b_wt -5.397475723 1.806823e-02
#> 1260        b_wt -5.391131484 1.839134e-02
#> 1261        b_wt -5.384787244 1.871169e-02
#> 1262        b_wt -5.378443005 1.902985e-02
#> 1263        b_wt -5.372098765 1.934598e-02
#> 1264        b_wt -5.365754526 1.966024e-02
#> 1265        b_wt -5.359410287 1.997283e-02
#> 1266        b_wt -5.353066047 2.028398e-02
#> 1267        b_wt -5.346721808 2.059380e-02
#> 1268        b_wt -5.340377568 2.090293e-02
#> 1269        b_wt -5.334033329 2.121169e-02
#> 1270        b_wt -5.327689090 2.152042e-02
#> 1271        b_wt -5.321344850 2.182948e-02
#> 1272        b_wt -5.315000611 2.213925e-02
#> 1273        b_wt -5.308656371 2.245050e-02
#> 1274        b_wt -5.302312132 2.276354e-02
#> 1275        b_wt -5.295967893 2.307874e-02
#> 1276        b_wt -5.289623653 2.339654e-02
#> 1277        b_wt -5.283279414 2.371734e-02
#> 1278        b_wt -5.276935174 2.404158e-02
#> 1279        b_wt -5.270590935 2.437051e-02
#> 1280        b_wt -5.264246695 2.470418e-02
#> 1281        b_wt -5.257902456 2.504276e-02
#> 1282        b_wt -5.251558217 2.538666e-02
#> 1283        b_wt -5.245213977 2.573624e-02
#> 1284        b_wt -5.238869738 2.609186e-02
#> 1285        b_wt -5.232525498 2.645493e-02
#> 1286        b_wt -5.226181259 2.682550e-02
#> 1287        b_wt -5.219837020 2.720329e-02
#> 1288        b_wt -5.213492780 2.758857e-02
#> 1289        b_wt -5.207148541 2.798160e-02
#> 1290        b_wt -5.200804301 2.838262e-02
#> 1291        b_wt -5.194460062 2.879282e-02
#> 1292        b_wt -5.188115822 2.921256e-02
#> 1293        b_wt -5.181771583 2.964097e-02
#> 1294        b_wt -5.175427344 3.007818e-02
#> 1295        b_wt -5.169083104 3.052429e-02
#> 1296        b_wt -5.162738865 3.097943e-02
#> 1297        b_wt -5.156394625 3.144439e-02
#> 1298        b_wt -5.150050386 3.192001e-02
#> 1299        b_wt -5.143706147 3.240487e-02
#> 1300        b_wt -5.137361907 3.289899e-02
#> 1301        b_wt -5.131017668 3.340238e-02
#> 1302        b_wt -5.124673428 3.391504e-02
#> 1303        b_wt -5.118329189 3.443739e-02
#> 1304        b_wt -5.111984949 3.497084e-02
#> 1305        b_wt -5.105640710 3.551353e-02
#> 1306        b_wt -5.099296471 3.606545e-02
#> 1307        b_wt -5.092952231 3.662659e-02
#> 1308        b_wt -5.086607992 3.719693e-02
#> 1309        b_wt -5.080263752 3.777651e-02
#> 1310        b_wt -5.073919513 3.836741e-02
#> 1311        b_wt -5.067575274 3.896747e-02
#> 1312        b_wt -5.061231034 3.957670e-02
#> 1313        b_wt -5.054886795 4.019511e-02
#> 1314        b_wt -5.048542555 4.082272e-02
#> 1315        b_wt -5.042198316 4.145955e-02
#> 1316        b_wt -5.035854076 4.210761e-02
#> 1317        b_wt -5.029509837 4.276525e-02
#> 1318        b_wt -5.023165598 4.343225e-02
#> 1319        b_wt -5.016821358 4.410867e-02
#> 1320        b_wt -5.010477119 4.479456e-02
#> 1321        b_wt -5.004132879 4.549000e-02
#> 1322        b_wt -4.997788640 4.619674e-02
#> 1323        b_wt -4.991444401 4.691382e-02
#> 1324        b_wt -4.985100161 4.764067e-02
#> 1325        b_wt -4.978755922 4.837735e-02
#> 1326        b_wt -4.972411682 4.912392e-02
#> 1327        b_wt -4.966067443 4.988043e-02
#> 1328        b_wt -4.959723203 5.064834e-02
#> 1329        b_wt -4.953378964 5.142730e-02
#> 1330        b_wt -4.947034725 5.221632e-02
#> 1331        b_wt -4.940690485 5.301539e-02
#> 1332        b_wt -4.934346246 5.382451e-02
#> 1333        b_wt -4.928002006 5.464365e-02
#> 1334        b_wt -4.921657767 5.547381e-02
#> 1335        b_wt -4.915313528 5.631523e-02
#> 1336        b_wt -4.908969288 5.716644e-02
#> 1337        b_wt -4.902625049 5.802735e-02
#> 1338        b_wt -4.896280809 5.889783e-02
#> 1339        b_wt -4.889936570 5.977774e-02
#> 1340        b_wt -4.883592330 6.066755e-02
#> 1341        b_wt -4.877248091 6.156796e-02
#> 1342        b_wt -4.870903852 6.247717e-02
#> 1343        b_wt -4.864559612 6.339498e-02
#> 1344        b_wt -4.858215373 6.432115e-02
#> 1345        b_wt -4.851871133 6.525547e-02
#> 1346        b_wt -4.845526894 6.619794e-02
#> 1347        b_wt -4.839182655 6.714958e-02
#> 1348        b_wt -4.832838415 6.810849e-02
#> 1349        b_wt -4.826494176 6.907443e-02
#> 1350        b_wt -4.820149936 7.004716e-02
#> 1351        b_wt -4.813805697 7.102644e-02
#> 1352        b_wt -4.807461457 7.201207e-02
#> 1353        b_wt -4.801117218 7.300519e-02
#> 1354        b_wt -4.794772979 7.400414e-02
#> 1355        b_wt -4.788428739 7.500874e-02
#> 1356        b_wt -4.782084500 7.601885e-02
#> 1357        b_wt -4.775740260 7.703434e-02
#> 1358        b_wt -4.769396021 7.805512e-02
#> 1359        b_wt -4.763051782 7.908215e-02
#> 1360        b_wt -4.756707542 8.011456e-02
#> 1361        b_wt -4.750363303 8.115217e-02
#> 1362        b_wt -4.744019063 8.219502e-02
#> 1363        b_wt -4.737674824 8.324320e-02
#> 1364        b_wt -4.731330584 8.429682e-02
#> 1365        b_wt -4.724986345 8.535701e-02
#> 1366        b_wt -4.718642106 8.642355e-02
#> 1367        b_wt -4.712297866 8.749626e-02
#> 1368        b_wt -4.705953627 8.857544e-02
#> 1369        b_wt -4.699609387 8.966139e-02
#> 1370        b_wt -4.693265148 9.075445e-02
#> 1371        b_wt -4.686920909 9.185604e-02
#> 1372        b_wt -4.680576669 9.296665e-02
#> 1373        b_wt -4.674232430 9.408587e-02
#> 1374        b_wt -4.667888190 9.521417e-02
#> 1375        b_wt -4.661543951 9.635204e-02
#> 1376        b_wt -4.655199712 9.749999e-02
#> 1377        b_wt -4.648855472 9.865958e-02
#> 1378        b_wt -4.642511233 9.983222e-02
#> 1379        b_wt -4.636166993 1.010169e-01
#> 1380        b_wt -4.629822754 1.022140e-01
#> 1381        b_wt -4.623478514 1.034243e-01
#> 1382        b_wt -4.617134275 1.046482e-01
#> 1383        b_wt -4.610790036 1.058871e-01
#> 1384        b_wt -4.604445796 1.071436e-01
#> 1385        b_wt -4.598101557 1.084157e-01
#> 1386        b_wt -4.591757317 1.097037e-01
#> 1387        b_wt -4.585413078 1.110082e-01
#> 1388        b_wt -4.579068839 1.123295e-01
#> 1389        b_wt -4.572724599 1.136686e-01
#> 1390        b_wt -4.566380360 1.150293e-01
#> 1391        b_wt -4.560036120 1.164084e-01
#> 1392        b_wt -4.553691881 1.178061e-01
#> 1393        b_wt -4.547347641 1.192226e-01
#> 1394        b_wt -4.541003402 1.206582e-01
#> 1395        b_wt -4.534659163 1.221132e-01
#> 1396        b_wt -4.528314923 1.235922e-01
#> 1397        b_wt -4.521970684 1.250912e-01
#> 1398        b_wt -4.515626444 1.266102e-01
#> 1399        b_wt -4.509282205 1.281490e-01
#> 1400        b_wt -4.502937966 1.297078e-01
#> 1401        b_wt -4.496593726 1.312866e-01
#> 1402        b_wt -4.490249487 1.328891e-01
#> 1403        b_wt -4.483905247 1.345125e-01
#> 1404        b_wt -4.477561008 1.361557e-01
#> 1405        b_wt -4.471216768 1.378185e-01
#> 1406        b_wt -4.464872529 1.395008e-01
#> 1407        b_wt -4.458528290 1.412024e-01
#> 1408        b_wt -4.452184050 1.429261e-01
#> 1409        b_wt -4.445839811 1.446702e-01
#> 1410        b_wt -4.439495571 1.464329e-01
#> 1411        b_wt -4.433151332 1.482138e-01
#> 1412        b_wt -4.426807093 1.500128e-01
#> 1413        b_wt -4.420462853 1.518295e-01
#> 1414        b_wt -4.414118614 1.536656e-01
#> 1415        b_wt -4.407774374 1.555207e-01
#> 1416        b_wt -4.401430135 1.573924e-01
#> 1417        b_wt -4.395085895 1.592803e-01
#> 1418        b_wt -4.388741656 1.611841e-01
#> 1419        b_wt -4.382397417 1.631033e-01
#> 1420        b_wt -4.376053177 1.650388e-01
#> 1421        b_wt -4.369708938 1.669911e-01
#> 1422        b_wt -4.363364698 1.689574e-01
#> 1423        b_wt -4.357020459 1.709372e-01
#> 1424        b_wt -4.350676220 1.729302e-01
#> 1425        b_wt -4.344331980 1.749359e-01
#> 1426        b_wt -4.337987741 1.769543e-01
#> 1427        b_wt -4.331643501 1.789866e-01
#> 1428        b_wt -4.325299262 1.810297e-01
#> 1429        b_wt -4.318955022 1.830833e-01
#> 1430        b_wt -4.312610783 1.851468e-01
#> 1431        b_wt -4.306266544 1.872197e-01
#> 1432        b_wt -4.299922304 1.893016e-01
#> 1433        b_wt -4.293578065 1.913934e-01
#> 1434        b_wt -4.287233825 1.934927e-01
#> 1435        b_wt -4.280889586 1.955988e-01
#> 1436        b_wt -4.274545347 1.977112e-01
#> 1437        b_wt -4.268201107 1.998294e-01
#> 1438        b_wt -4.261856868 2.019527e-01
#> 1439        b_wt -4.255512628 2.040815e-01
#> 1440        b_wt -4.249168389 2.062140e-01
#> 1441        b_wt -4.242824149 2.083498e-01
#> 1442        b_wt -4.236479910 2.104882e-01
#> 1443        b_wt -4.230135671 2.126287e-01
#> 1444        b_wt -4.223791431 2.147709e-01
#> 1445        b_wt -4.217447192 2.169143e-01
#> 1446        b_wt -4.211102952 2.190581e-01
#> 1447        b_wt -4.204758713 2.212021e-01
#> 1448        b_wt -4.198414474 2.233456e-01
#> 1449        b_wt -4.192070234 2.254885e-01
#> 1450        b_wt -4.185725995 2.276304e-01
#> 1451        b_wt -4.179381755 2.297707e-01
#> 1452        b_wt -4.173037516 2.319093e-01
#> 1453        b_wt -4.166693276 2.340461e-01
#> 1454        b_wt -4.160349037 2.361811e-01
#> 1455        b_wt -4.154004798 2.383141e-01
#> 1456        b_wt -4.147660558 2.404451e-01
#> 1457        b_wt -4.141316319 2.425739e-01
#> 1458        b_wt -4.134972079 2.447008e-01
#> 1459        b_wt -4.128627840 2.468260e-01
#> 1460        b_wt -4.122283601 2.489499e-01
#> 1461        b_wt -4.115939361 2.510726e-01
#> 1462        b_wt -4.109595122 2.531944e-01
#> 1463        b_wt -4.103250882 2.553158e-01
#> 1464        b_wt -4.096906643 2.574373e-01
#> 1465        b_wt -4.090562403 2.595595e-01
#> 1466        b_wt -4.084218164 2.616827e-01
#> 1467        b_wt -4.077873925 2.638076e-01
#> 1468        b_wt -4.071529685 2.659347e-01
#> 1469        b_wt -4.065185446 2.680647e-01
#> 1470        b_wt -4.058841206 2.701990e-01
#> 1471        b_wt -4.052496967 2.723375e-01
#> 1472        b_wt -4.046152728 2.744811e-01
#> 1473        b_wt -4.039808488 2.766301e-01
#> 1474        b_wt -4.033464249 2.787854e-01
#> 1475        b_wt -4.027120009 2.809473e-01
#> 1476        b_wt -4.020775770 2.831186e-01
#> 1477        b_wt -4.014431530 2.852981e-01
#> 1478        b_wt -4.008087291 2.874864e-01
#> 1479        b_wt -4.001743052 2.896838e-01
#> 1480        b_wt -3.995398812 2.918910e-01
#> 1481        b_wt -3.989054573 2.941082e-01
#> 1482        b_wt -3.982710333 2.963380e-01
#> 1483        b_wt -3.976366094 2.985792e-01
#> 1484        b_wt -3.970021855 3.008314e-01
#> 1485        b_wt -3.963677615 3.030949e-01
#> 1486        b_wt -3.957333376 3.053697e-01
#> 1487        b_wt -3.950989136 3.076558e-01
#> 1488        b_wt -3.944644897 3.099552e-01
#> 1489        b_wt -3.938300658 3.122665e-01
#> 1490        b_wt -3.931956418 3.145887e-01
#> 1491        b_wt -3.925612179 3.169216e-01
#> 1492        b_wt -3.919267939 3.192647e-01
#> 1493        b_wt -3.912923700 3.216177e-01
#> 1494        b_wt -3.906579460 3.239812e-01
#> 1495        b_wt -3.900235221 3.263543e-01
#> 1496        b_wt -3.893890982 3.287352e-01
#> 1497        b_wt -3.887546742 3.311234e-01
#> 1498        b_wt -3.881202503 3.335180e-01
#> 1499        b_wt -3.874858263 3.359183e-01
#> 1500        b_wt -3.868514024 3.383239e-01
#> 1501        b_wt -3.862169785 3.407338e-01
#> 1502        b_wt -3.855825545 3.431462e-01
#> 1503        b_wt -3.849481306 3.455604e-01
#> 1504        b_wt -3.843137066 3.479753e-01
#> 1505        b_wt -3.836792827 3.503901e-01
#> 1506        b_wt -3.830448587 3.528037e-01
#> 1507        b_wt -3.824104348 3.552145e-01
#> 1508        b_wt -3.817760109 3.576218e-01
#> 1509        b_wt -3.811415869 3.600247e-01
#> 1510        b_wt -3.805071630 3.624222e-01
#> 1511        b_wt -3.798727390 3.648136e-01
#> 1512        b_wt -3.792383151 3.671979e-01
#> 1513        b_wt -3.786038912 3.695723e-01
#> 1514        b_wt -3.779694672 3.719377e-01
#> 1515        b_wt -3.773350433 3.742936e-01
#> 1516        b_wt -3.767006193 3.766391e-01
#> 1517        b_wt -3.760661954 3.789738e-01
#> 1518        b_wt -3.754317714 3.812972e-01
#> 1519        b_wt -3.747973475 3.836058e-01
#> 1520        b_wt -3.741629236 3.859019e-01
#> 1521        b_wt -3.735284996 3.881853e-01
#> 1522        b_wt -3.728940757 3.904556e-01
#> 1523        b_wt -3.722596517 3.927129e-01
#> 1524        b_wt -3.716252278 3.949568e-01
#> 1525        b_wt -3.709908039 3.971848e-01
#> 1526        b_wt -3.703563799 3.993988e-01
#> 1527        b_wt -3.697219560 4.015996e-01
#> 1528        b_wt -3.690875320 4.037873e-01
#> 1529        b_wt -3.684531081 4.059620e-01
#> 1530        b_wt -3.678186841 4.081240e-01
#> 1531        b_wt -3.671842602 4.102716e-01
#> 1532        b_wt -3.665498363 4.124061e-01
#> 1533        b_wt -3.659154123 4.145290e-01
#> 1534        b_wt -3.652809884 4.166406e-01
#> 1535        b_wt -3.646465644 4.187412e-01
#> 1536        b_wt -3.640121405 4.208313e-01
#> 1537        b_wt -3.633777166 4.229099e-01
#> 1538        b_wt -3.627432926 4.249778e-01
#> 1539        b_wt -3.621088687 4.270364e-01
#> 1540        b_wt -3.614744447 4.290860e-01
#> 1541        b_wt -3.608400208 4.311271e-01
#> 1542        b_wt -3.602055968 4.331597e-01
#> 1543        b_wt -3.595711729 4.351837e-01
#> 1544        b_wt -3.589367490 4.371986e-01
#> 1545        b_wt -3.583023250 4.392058e-01
#> 1546        b_wt -3.576679011 4.412053e-01
#> 1547        b_wt -3.570334771 4.431973e-01
#> 1548        b_wt -3.563990532 4.451816e-01
#> 1549        b_wt -3.557646293 4.471577e-01
#> 1550        b_wt -3.551302053 4.491241e-01
#> 1551        b_wt -3.544957814 4.510822e-01
#> 1552        b_wt -3.538613574 4.530313e-01
#> 1553        b_wt -3.532269335 4.549712e-01
#> 1554        b_wt -3.525925095 4.569012e-01
#> 1555        b_wt -3.519580856 4.588207e-01
#> 1556        b_wt -3.513236617 4.607262e-01
#> 1557        b_wt -3.506892377 4.626193e-01
#> 1558        b_wt -3.500548138 4.644991e-01
#> 1559        b_wt -3.494203898 4.663647e-01
#> 1560        b_wt -3.487859659 4.682151e-01
#> 1561        b_wt -3.481515420 4.700493e-01
#> 1562        b_wt -3.475171180 4.718619e-01
#> 1563        b_wt -3.468826941 4.736548e-01
#> 1564        b_wt -3.462482701 4.754272e-01
#> 1565        b_wt -3.456138462 4.771777e-01
#> 1566        b_wt -3.449794222 4.789051e-01
#> 1567        b_wt -3.443449983 4.806081e-01
#> 1568        b_wt -3.437105744 4.822802e-01
#> 1569        b_wt -3.430761504 4.839227e-01
#> 1570        b_wt -3.424417265 4.855359e-01
#> 1571        b_wt -3.418073025 4.871183e-01
#> 1572        b_wt -3.411728786 4.886686e-01
#> 1573        b_wt -3.405384547 4.901855e-01
#> 1574        b_wt -3.399040307 4.916622e-01
#> 1575        b_wt -3.392696068 4.930988e-01
#> 1576        b_wt -3.386351828 4.944973e-01
#> 1577        b_wt -3.380007589 4.958565e-01
#> 1578        b_wt -3.373663349 4.971754e-01
#> 1579        b_wt -3.367319110 4.984527e-01
#> 1580        b_wt -3.360974871 4.996826e-01
#> 1581        b_wt -3.354630631 5.008627e-01
#> 1582        b_wt -3.348286392 5.019978e-01
#> 1583        b_wt -3.341942152 5.030870e-01
#> 1584        b_wt -3.335597913 5.041294e-01
#> 1585        b_wt -3.329253674 5.051245e-01
#> 1586        b_wt -3.322909434 5.060678e-01
#> 1587        b_wt -3.316565195 5.069541e-01
#> 1588        b_wt -3.310220955 5.077908e-01
#> 1589        b_wt -3.303876716 5.085775e-01
#> 1590        b_wt -3.297532477 5.093138e-01
#> 1591        b_wt -3.291188237 5.099994e-01
#> 1592        b_wt -3.284843998 5.106320e-01
#> 1593        b_wt -3.278499758 5.112029e-01
#> 1594        b_wt -3.272155519 5.117222e-01
#> 1595        b_wt -3.265811279 5.121898e-01
#> 1596        b_wt -3.259467040 5.126057e-01
#> 1597        b_wt -3.253122801 5.129698e-01
#> 1598        b_wt -3.246778561 5.132821e-01
#> 1599        b_wt -3.240434322 5.135302e-01
#> 1600        b_wt -3.234090082 5.137266e-01
#> 1601        b_wt -3.227745843 5.138715e-01
#> 1602        b_wt -3.221401604 5.139649e-01
#> 1603        b_wt -3.215057364 5.140071e-01
#> 1604        b_wt -3.208713125 5.139981e-01
#> 1605        b_wt -3.202368885 5.139278e-01
#> 1606        b_wt -3.196024646 5.138052e-01
#> 1607        b_wt -3.189680406 5.136323e-01
#> 1608        b_wt -3.183336167 5.134094e-01
#> 1609        b_wt -3.176991928 5.131369e-01
#> 1610        b_wt -3.170647688 5.128149e-01
#> 1611        b_wt -3.164303449 5.124357e-01
#> 1612        b_wt -3.157959209 5.120045e-01
#> 1613        b_wt -3.151614970 5.115254e-01
#> 1614        b_wt -3.145270731 5.109988e-01
#> 1615        b_wt -3.138926491 5.104251e-01
#> 1616        b_wt -3.132582252 5.098049e-01
#> 1617        b_wt -3.126238012 5.091327e-01
#> 1618        b_wt -3.119893773 5.084106e-01
#> 1619        b_wt -3.113549533 5.076441e-01
#> 1620        b_wt -3.107205294 5.068340e-01
#> 1621        b_wt -3.100861055 5.059811e-01
#> 1622        b_wt -3.094516815 5.050860e-01
#> 1623        b_wt -3.088172576 5.041458e-01
#> 1624        b_wt -3.081828336 5.031599e-01
#> 1625        b_wt -3.075484097 5.021353e-01
#> 1626        b_wt -3.069139858 5.010728e-01
#> 1627        b_wt -3.062795618 4.999736e-01
#> 1628        b_wt -3.056451379 4.988388e-01
#> 1629        b_wt -3.050107139 4.976675e-01
#> 1630        b_wt -3.043762900 4.964576e-01
#> 1631        b_wt -3.037418660 4.952164e-01
#> 1632        b_wt -3.031074421 4.939453e-01
#> 1633        b_wt -3.024730182 4.926457e-01
#> 1634        b_wt -3.018385942 4.913188e-01
#> 1635        b_wt -3.012041703 4.899656e-01
#> 1636        b_wt -3.005697463 4.885832e-01
#> 1637        b_wt -2.999353224 4.871788e-01
#> 1638        b_wt -2.993008985 4.857539e-01
#> 1639        b_wt -2.986664745 4.843098e-01
#> 1640        b_wt -2.980320506 4.828479e-01
#> 1641        b_wt -2.973976266 4.813699e-01
#> 1642        b_wt -2.967632027 4.798741e-01
#> 1643        b_wt -2.961287787 4.783657e-01
#> 1644        b_wt -2.954943548 4.768461e-01
#> 1645        b_wt -2.948599309 4.753167e-01
#> 1646        b_wt -2.942255069 4.737787e-01
#> 1647        b_wt -2.935910830 4.722335e-01
#> 1648        b_wt -2.929566590 4.706813e-01
#> 1649        b_wt -2.923222351 4.691247e-01
#> 1650        b_wt -2.916878112 4.675648e-01
#> 1651        b_wt -2.910533872 4.660026e-01
#> 1652        b_wt -2.904189633 4.644390e-01
#> 1653        b_wt -2.897845393 4.628749e-01
#> 1654        b_wt -2.891501154 4.613112e-01
#> 1655        b_wt -2.885156914 4.597486e-01
#> 1656        b_wt -2.878812675 4.581878e-01
#> 1657        b_wt -2.872468436 4.566291e-01
#> 1658        b_wt -2.866124196 4.550729e-01
#> 1659        b_wt -2.859779957 4.535193e-01
#> 1660        b_wt -2.853435717 4.519690e-01
#> 1661        b_wt -2.847091478 4.504220e-01
#> 1662        b_wt -2.840747239 4.488778e-01
#> 1663        b_wt -2.834402999 4.473362e-01
#> 1664        b_wt -2.828058760 4.457970e-01
#> 1665        b_wt -2.821714520 4.442599e-01
#> 1666        b_wt -2.815370281 4.427246e-01
#> 1667        b_wt -2.809026041 4.411903e-01
#> 1668        b_wt -2.802681802 4.396564e-01
#> 1669        b_wt -2.796337563 4.381219e-01
#> 1670        b_wt -2.789993323 4.365864e-01
#> 1671        b_wt -2.783649084 4.350489e-01
#> 1672        b_wt -2.777304844 4.335084e-01
#> 1673        b_wt -2.770960605 4.319633e-01
#> 1674        b_wt -2.764616366 4.304131e-01
#> 1675        b_wt -2.758272126 4.288568e-01
#> 1676        b_wt -2.751927887 4.272935e-01
#> 1677        b_wt -2.745583647 4.257221e-01
#> 1678        b_wt -2.739239408 4.241416e-01
#> 1679        b_wt -2.732895168 4.225483e-01
#> 1680        b_wt -2.726550929 4.209434e-01
#> 1681        b_wt -2.720206690 4.193259e-01
#> 1682        b_wt -2.713862450 4.176946e-01
#> 1683        b_wt -2.707518211 4.160488e-01
#> 1684        b_wt -2.701173971 4.143875e-01
#> 1685        b_wt -2.694829732 4.127058e-01
#> 1686        b_wt -2.688485493 4.110058e-01
#> 1687        b_wt -2.682141253 4.092873e-01
#> 1688        b_wt -2.675797014 4.075493e-01
#> 1689        b_wt -2.669452774 4.057912e-01
#> 1690        b_wt -2.663108535 4.040122e-01
#> 1691        b_wt -2.656764295 4.022077e-01
#> 1692        b_wt -2.650420056 4.003796e-01
#> 1693        b_wt -2.644075817 3.985288e-01
#> 1694        b_wt -2.637731577 3.966546e-01
#> 1695        b_wt -2.631387338 3.947569e-01
#> 1696        b_wt -2.625043098 3.928352e-01
#> 1697        b_wt -2.618698859 3.908857e-01
#> 1698        b_wt -2.612354620 3.889094e-01
#> 1699        b_wt -2.606010380 3.869086e-01
#> 1700        b_wt -2.599666141 3.848831e-01
#> 1701        b_wt -2.593321901 3.828329e-01
#> 1702        b_wt -2.586977662 3.807582e-01
#> 1703        b_wt -2.580633423 3.786563e-01
#> 1704        b_wt -2.574289183 3.765270e-01
#> 1705        b_wt -2.567944944 3.743737e-01
#> 1706        b_wt -2.561600704 3.721968e-01
#> 1707        b_wt -2.555256465 3.699966e-01
#> 1708        b_wt -2.548912225 3.677733e-01
#> 1709        b_wt -2.542567986 3.655259e-01
#> 1710        b_wt -2.536223747 3.632530e-01
#> 1711        b_wt -2.529879507 3.609589e-01
#> 1712        b_wt -2.523535268 3.586442e-01
#> 1713        b_wt -2.517191028 3.563094e-01
#> 1714        b_wt -2.510846789 3.539552e-01
#> 1715        b_wt -2.504502550 3.515816e-01
#> 1716        b_wt -2.498158310 3.491866e-01
#> 1717        b_wt -2.491814071 3.467750e-01
#> 1718        b_wt -2.485469831 3.443473e-01
#> 1719        b_wt -2.479125592 3.419045e-01
#> 1720        b_wt -2.472781352 3.394474e-01
#> 1721        b_wt -2.466437113 3.369769e-01
#> 1722        b_wt -2.460092874 3.344912e-01
#> 1723        b_wt -2.453748634 3.319945e-01
#> 1724        b_wt -2.447404395 3.294878e-01
#> 1725        b_wt -2.441060155 3.269719e-01
#> 1726        b_wt -2.434715916 3.244480e-01
#> 1727        b_wt -2.428371677 3.219169e-01
#> 1728        b_wt -2.422027437 3.193786e-01
#> 1729        b_wt -2.415683198 3.168357e-01
#> 1730        b_wt -2.409338958 3.142891e-01
#> 1731        b_wt -2.402994719 3.117400e-01
#> 1732        b_wt -2.396650479 3.091893e-01
#> 1733        b_wt -2.390306240 3.066379e-01
#> 1734        b_wt -2.383962001 3.040870e-01
#> 1735        b_wt -2.377617761 3.015378e-01
#> 1736        b_wt -2.371273522 2.989911e-01
#> 1737        b_wt -2.364929282 2.964479e-01
#> 1738        b_wt -2.358585043 2.939087e-01
#> 1739        b_wt -2.352240804 2.913743e-01
#> 1740        b_wt -2.345896564 2.888464e-01
#> 1741        b_wt -2.339552325 2.863255e-01
#> 1742        b_wt -2.333208085 2.838117e-01
#> 1743        b_wt -2.326863846 2.813053e-01
#> 1744        b_wt -2.320519606 2.788068e-01
#> 1745        b_wt -2.314175367 2.763165e-01
#> 1746        b_wt -2.307831128 2.738356e-01
#> 1747        b_wt -2.301486888 2.713646e-01
#> 1748        b_wt -2.295142649 2.689025e-01
#> 1749        b_wt -2.288798409 2.664492e-01
#> 1750        b_wt -2.282454170 2.640047e-01
#> 1751        b_wt -2.276109931 2.615689e-01
#> 1752        b_wt -2.269765691 2.591420e-01
#> 1753        b_wt -2.263421452 2.567247e-01
#> 1754        b_wt -2.257077212 2.543150e-01
#> 1755        b_wt -2.250732973 2.519126e-01
#> 1756        b_wt -2.244388733 2.495169e-01
#> 1757        b_wt -2.238044494 2.471276e-01
#> 1758        b_wt -2.231700255 2.447441e-01
#> 1759        b_wt -2.225356015 2.423666e-01
#> 1760        b_wt -2.219011776 2.399932e-01
#> 1761        b_wt -2.212667536 2.376233e-01
#> 1762        b_wt -2.206323297 2.352562e-01
#> 1763        b_wt -2.199979058 2.328913e-01
#> 1764        b_wt -2.193634818 2.305279e-01
#> 1765        b_wt -2.187290579 2.281652e-01
#> 1766        b_wt -2.180946339 2.258023e-01
#> 1767        b_wt -2.174602100 2.234385e-01
#> 1768        b_wt -2.168257860 2.210733e-01
#> 1769        b_wt -2.161913621 2.187062e-01
#> 1770        b_wt -2.155569382 2.163365e-01
#> 1771        b_wt -2.149225142 2.139631e-01
#> 1772        b_wt -2.142880903 2.115859e-01
#> 1773        b_wt -2.136536663 2.092047e-01
#> 1774        b_wt -2.130192424 2.068194e-01
#> 1775        b_wt -2.123848185 2.044297e-01
#> 1776        b_wt -2.117503945 2.020353e-01
#> 1777        b_wt -2.111159706 1.996357e-01
#> 1778        b_wt -2.104815466 1.972311e-01
#> 1779        b_wt -2.098471227 1.948220e-01
#> 1780        b_wt -2.092126987 1.924088e-01
#> 1781        b_wt -2.085782748 1.899916e-01
#> 1782        b_wt -2.079438509 1.875709e-01
#> 1783        b_wt -2.073094269 1.851466e-01
#> 1784        b_wt -2.066750030 1.827197e-01
#> 1785        b_wt -2.060405790 1.802911e-01
#> 1786        b_wt -2.054061551 1.778615e-01
#> 1787        b_wt -2.047717312 1.754316e-01
#> 1788        b_wt -2.041373072 1.730023e-01
#> 1789        b_wt -2.035028833 1.705745e-01
#> 1790        b_wt -2.028684593 1.681499e-01
#> 1791        b_wt -2.022340354 1.657292e-01
#> 1792        b_wt -2.015996114 1.633135e-01
#> 1793        b_wt -2.009651875 1.609037e-01
#> 1794        b_wt -2.003307636 1.585010e-01
#> 1795        b_wt -1.996963396 1.561071e-01
#> 1796        b_wt -1.990619157 1.537249e-01
#> 1797        b_wt -1.984274917 1.513539e-01
#> 1798        b_wt -1.977930678 1.489954e-01
#> 1799        b_wt -1.971586439 1.466506e-01
#> 1800        b_wt -1.965242199 1.443206e-01
#> 1801        b_wt -1.958897960 1.420070e-01
#> 1802        b_wt -1.952553720 1.397150e-01
#> 1803        b_wt -1.946209481 1.374420e-01
#> 1804        b_wt -1.939865242 1.351892e-01
#> 1805        b_wt -1.933521002 1.329577e-01
#> 1806        b_wt -1.927176763 1.307485e-01
#> 1807        b_wt -1.920832523 1.285627e-01
#> 1808        b_wt -1.914488284 1.264070e-01
#> 1809        b_wt -1.908144044 1.242776e-01
#> 1810        b_wt -1.901799805 1.221750e-01
#> 1811        b_wt -1.895455566 1.200999e-01
#> 1812        b_wt -1.889111326 1.180530e-01
#> 1813        b_wt -1.882767087 1.160352e-01
#> 1814        b_wt -1.876422847 1.140527e-01
#> 1815        b_wt -1.870078608 1.121023e-01
#> 1816        b_wt -1.863734369 1.101830e-01
#> 1817        b_wt -1.857390129 1.082952e-01
#> 1818        b_wt -1.851045890 1.064392e-01
#> 1819        b_wt -1.844701650 1.046153e-01
#> 1820        b_wt -1.838357411 1.028287e-01
#> 1821        b_wt -1.832013171 1.010777e-01
#> 1822        b_wt -1.825668932 9.935938e-02
#> 1823        b_wt -1.819324693 9.767385e-02
#> 1824        b_wt -1.812980453 9.602105e-02
#> 1825        b_wt -1.806636214 9.440092e-02
#> 1826        b_wt -1.800291974 9.281703e-02
#> 1827        b_wt -1.793947735 9.126951e-02
#> 1828        b_wt -1.787603496 8.975397e-02
#> 1829        b_wt -1.781259256 8.827014e-02
#> 1830        b_wt -1.774915017 8.681769e-02
#> 1831        b_wt -1.768570777 8.539627e-02
#> 1832        b_wt -1.762226538 8.400786e-02
#> 1833        b_wt -1.755882298 8.265439e-02
#> 1834        b_wt -1.749538059 8.133041e-02
#> 1835        b_wt -1.743193820 8.003546e-02
#> 1836        b_wt -1.736849580 7.876904e-02
#> 1837        b_wt -1.730505341 7.753064e-02
#> 1838        b_wt -1.724161101 7.632089e-02
#> 1839        b_wt -1.717816862 7.514324e-02
#> 1840        b_wt -1.711472623 7.399169e-02
#> 1841        b_wt -1.705128383 7.286569e-02
#> 1842        b_wt -1.698784144 7.176470e-02
#> 1843        b_wt -1.692439904 7.068816e-02
#> 1844        b_wt -1.686095665 6.963569e-02
#> 1845        b_wt -1.679751425 6.861184e-02
#> 1846        b_wt -1.673407186 6.761045e-02
#> 1847        b_wt -1.667062947 6.663100e-02
#> 1848        b_wt -1.660718707 6.567293e-02
#> 1849        b_wt -1.654374468 6.473572e-02
#> 1850        b_wt -1.648030228 6.381883e-02
#> 1851        b_wt -1.641685989 6.292573e-02
#> 1852        b_wt -1.635341750 6.205214e-02
#> 1853        b_wt -1.628997510 6.119694e-02
#> 1854        b_wt -1.622653271 6.035962e-02
#> 1855        b_wt -1.616309031 5.953964e-02
#> 1856        b_wt -1.609964792 5.873649e-02
#> 1857        b_wt -1.603620552 5.795233e-02
#> 1858        b_wt -1.597276313 5.718466e-02
#> 1859        b_wt -1.590932074 5.643187e-02
#> 1860        b_wt -1.584587834 5.569343e-02
#> 1861        b_wt -1.578243595 5.496879e-02
#> 1862        b_wt -1.571899355 5.425741e-02
#> 1863        b_wt -1.565555116 5.356038e-02
#> 1864        b_wt -1.559210877 5.287641e-02
#> 1865        b_wt -1.552866637 5.220368e-02
#> 1866        b_wt -1.546522398 5.154159e-02
#> 1867        b_wt -1.540178158 5.088959e-02
#> 1868        b_wt -1.533833919 5.024711e-02
#> 1869        b_wt -1.527489679 4.961439e-02
#> 1870        b_wt -1.521145440 4.899089e-02
#> 1871        b_wt -1.514801201 4.837482e-02
#> 1872        b_wt -1.508456961 4.776560e-02
#> 1873        b_wt -1.502112722 4.716267e-02
#> 1874        b_wt -1.495768482 4.656547e-02
#> 1875        b_wt -1.489424243 4.597376e-02
#> 1876        b_wt -1.483080004 4.538726e-02
#> 1877        b_wt -1.476735764 4.480455e-02
#> 1878        b_wt -1.470391525 4.422514e-02
#> 1879        b_wt -1.464047285 4.364856e-02
#> 1880        b_wt -1.457703046 4.307433e-02
#> 1881        b_wt -1.451358806 4.250205e-02
#> 1882        b_wt -1.445014567 4.193142e-02
#> 1883        b_wt -1.438670328 4.136166e-02
#> 1884        b_wt -1.432326088 4.079243e-02
#> 1885        b_wt -1.425981849 4.022341e-02
#> 1886        b_wt -1.419637609 3.965432e-02
#> 1887        b_wt -1.413293370 3.908489e-02
#> 1888        b_wt -1.406949131 3.851469e-02
#> 1889        b_wt -1.400604891 3.794363e-02
#> 1890        b_wt -1.394260652 3.737159e-02
#> 1891        b_wt -1.387916412 3.679846e-02
#> 1892        b_wt -1.381572173 3.622417e-02
#> 1893        b_wt -1.375227933 3.564868e-02
#> 1894        b_wt -1.368883694 3.507174e-02
#> 1895        b_wt -1.362539455 3.449360e-02
#> 1896        b_wt -1.356195215 3.391439e-02
#> 1897        b_wt -1.349850976 3.333422e-02
#> 1898        b_wt -1.343506736 3.275322e-02
#> 1899        b_wt -1.337162497 3.217155e-02
#> 1900        b_wt -1.330818258 3.158935e-02
#> 1901        b_wt -1.324474018 3.100698e-02
#> 1902        b_wt -1.318129779 3.042471e-02
#> 1903        b_wt -1.311785539 2.984280e-02
#> 1904        b_wt -1.305441300 2.926152e-02
#> 1905        b_wt -1.299097060 2.868117e-02
#> 1906        b_wt -1.292752821 2.810227e-02
#> 1907        b_wt -1.286408582 2.752525e-02
#> 1908        b_wt -1.280064342 2.695031e-02
#> 1909        b_wt -1.273720103 2.637778e-02
#> 1910        b_wt -1.267375863 2.580799e-02
#> 1911        b_wt -1.261031624 2.524129e-02
#> 1912        b_wt -1.254687385 2.467838e-02
#> 1913        b_wt -1.248343145 2.411998e-02
#> 1914        b_wt -1.241998906 2.356590e-02
#> 1915        b_wt -1.235654666 2.301649e-02
#> 1916        b_wt -1.229310427 2.247207e-02
#> 1917        b_wt -1.222966188 2.193296e-02
#> 1918        b_wt -1.216621948 2.139984e-02
#> 1919        b_wt -1.210277709 2.087389e-02
#> 1920        b_wt -1.203933469 2.035438e-02
#> 1921        b_wt -1.197589230 1.984159e-02
#> 1922        b_wt -1.191244990 1.933578e-02
#> 1923        b_wt -1.184900751 1.883724e-02
#> 1924        b_wt -1.178556512 1.834636e-02
#> 1925        b_wt -1.172212272 1.786505e-02
#> 1926        b_wt -1.165868033 1.739186e-02
#> 1927        b_wt -1.159523793 1.692701e-02
#> 1928        b_wt -1.153179554 1.647068e-02
#> 1929        b_wt -1.146835315 1.602306e-02
#> 1930        b_wt -1.140491075 1.558433e-02
#> 1931        b_wt -1.134146836 1.515673e-02
#> 1932        b_wt -1.127802596 1.473855e-02
#> 1933        b_wt -1.121458357 1.432977e-02
#> 1934        b_wt -1.115114117 1.393048e-02
#> 1935        b_wt -1.108769878 1.354079e-02
#> 1936        b_wt -1.102425639 1.316077e-02
#> 1937        b_wt -1.096081399 1.279237e-02
#> 1938        b_wt -1.089737160 1.243430e-02
#> 1939        b_wt -1.083392920 1.208608e-02
#> 1940        b_wt -1.077048681 1.174772e-02
#> 1941        b_wt -1.070704442 1.141924e-02
#> 1942        b_wt -1.064360202 1.110062e-02
#> 1943        b_wt -1.058015963 1.079333e-02
#> 1944        b_wt -1.051671723 1.049666e-02
#> 1945        b_wt -1.045327484 1.020968e-02
#> 1946        b_wt -1.038983244 9.932323e-03
#> 1947        b_wt -1.032639005 9.664488e-03
#> 1948        b_wt -1.026294766 9.406076e-03
#> 1949        b_wt -1.019950526 9.158038e-03
#> 1950        b_wt -1.013606287 8.920240e-03
#> 1951        b_wt -1.007262047 8.691378e-03
#> 1952        b_wt -1.000917808 8.471290e-03
#> 1953        b_wt -0.994573569 8.259803e-03
#> 1954        b_wt -0.988229329 8.056733e-03
#> 1955        b_wt -0.981885090 7.862535e-03
#> 1956        b_wt -0.975540850 7.677537e-03
#> 1957        b_wt -0.969196611 7.500211e-03
#> 1958        b_wt -0.962852371 7.330330e-03
#> 1959        b_wt -0.956508132 7.167661e-03
#> 1960        b_wt -0.950163893 7.011967e-03
#> 1961        b_wt -0.943819653 6.863293e-03
#> 1962        b_wt -0.937475414 6.722259e-03
#> 1963        b_wt -0.931131174 6.587294e-03
#> 1964        b_wt -0.924786935 6.458140e-03
#> 1965        b_wt -0.918442696 6.334540e-03
#> 1966        b_wt -0.912098456 6.216233e-03
#> 1967        b_wt -0.905754217 6.103004e-03
#> 1968        b_wt -0.899409977 5.995553e-03
#> 1969        b_wt -0.893065738 5.892459e-03
#> 1970        b_wt -0.886721498 5.793470e-03
#> 1971        b_wt -0.880377259 5.698336e-03
#> 1972        b_wt -0.874033020 5.606812e-03
#> 1973        b_wt -0.867688780 5.518658e-03
#> 1974        b_wt -0.861344541 5.434230e-03
#> 1975        b_wt -0.855000301 5.352649e-03
#> 1976        b_wt -0.848656062 5.273614e-03
#> 1977        b_wt -0.842311823 5.196916e-03
#> 1978        b_wt -0.835967583 5.122353e-03
#> 1979        b_wt -0.829623344 5.049727e-03
#> 1980        b_wt -0.823279104 4.979115e-03
#> 1981        b_wt -0.816934865 4.910074e-03
#> 1982        b_wt -0.810590625 4.842337e-03
#> 1983        b_wt -0.804246386 4.775748e-03
#> 1984        b_wt -0.797902147 4.710164e-03
#> 1985        b_wt -0.791557907 4.645446e-03
#> 1986        b_wt -0.785213668 4.581546e-03
#> 1987        b_wt -0.778869428 4.518268e-03
#> 1988        b_wt -0.772525189 4.455440e-03
#> 1989        b_wt -0.766180950 4.392969e-03
#> 1990        b_wt -0.759836710 4.330770e-03
#> 1991        b_wt -0.753492471 4.268765e-03
#> 1992        b_wt -0.747148231 4.206889e-03
#> 1993        b_wt -0.740803992 4.145060e-03
#> 1994        b_wt -0.734459752 4.083217e-03
#> 1995        b_wt -0.728115513 4.021321e-03
#> 1996        b_wt -0.721771274 3.959341e-03
#> 1997        b_wt -0.715427034 3.897251e-03
#> 1998        b_wt -0.709082795 3.835020e-03
#> 1999        b_wt -0.702738555 3.772621e-03
#> 2000        b_wt -0.696394316 3.710075e-03
#> 2001        b_wt -0.690050077 3.647385e-03
#> 2002        b_wt -0.683705837 3.584560e-03
#> 2003        b_wt -0.677361598 3.521614e-03
#> 2004        b_wt -0.671017358 3.458562e-03
#> 2005        b_wt -0.664673119 3.395420e-03
#> 2006        b_wt -0.658328879 3.332242e-03
#> 2007        b_wt -0.651984640 3.269057e-03
#> 2008        b_wt -0.645640401 3.205901e-03
#> 2009        b_wt -0.639296161 3.142810e-03
#> 2010        b_wt -0.632951922 3.079823e-03
#> 2011        b_wt -0.626607682 3.017031e-03
#> 2012        b_wt -0.620263443 2.954455e-03
#> 2013        b_wt -0.613919204 2.892138e-03
#> 2014        b_wt -0.607574964 2.830124e-03
#> 2015        b_wt -0.601230725 2.768458e-03
#> 2016        b_wt -0.594886485 2.707186e-03
#> 2017        b_wt -0.588542246 2.646454e-03
#> 2018        b_wt -0.582198007 2.586248e-03
#> 2019        b_wt -0.575853767 2.526593e-03
#> 2020        b_wt -0.569509528 2.467529e-03
#> 2021        b_wt -0.563165288 2.409095e-03
#> 2022        b_wt -0.556821049 2.351327e-03
#> 2023        b_wt -0.550476809 2.294388e-03
#> 2024        b_wt -0.544132570 2.238255e-03
#> 2025        b_wt -0.537788331 2.182905e-03
#> 2026        b_wt -0.531444091 2.128361e-03
#> 2027        b_wt -0.525099852 2.074648e-03
#> 2028        b_wt -0.518755612 2.021785e-03
#> 2029        b_wt -0.512411373 1.969906e-03
#> 2030        b_wt -0.506067134 1.919010e-03
#> 2031        b_wt -0.499722894 1.869011e-03
#> 2032        b_wt -0.493378655 1.819914e-03
#> 2033        b_wt -0.487034415 1.771721e-03
#> 2034        b_wt -0.480690176 1.724433e-03
#> 2035        b_wt -0.474345936 1.678131e-03
#> 2036        b_wt -0.468001697 1.632853e-03
#> 2037        b_wt -0.461657458 1.588457e-03
#> 2038        b_wt -0.455313218 1.544929e-03
#> 2039        b_wt -0.448968979 1.502258e-03
#> 2040        b_wt -0.442624739 1.460427e-03
#> 2041        b_wt -0.436280500 1.419465e-03
#> 2042        b_wt -0.429936261 1.379446e-03
#> 2043        b_wt -0.423592021 1.340197e-03
#> 2044        b_wt -0.417247782 1.301700e-03
#> 2045        b_wt -0.410903542 1.263931e-03
#> 2046        b_wt -0.404559303 1.226869e-03
#> 2047        b_wt -0.398215063 1.190508e-03
#> 2048        b_wt -0.391870824 1.154946e-03
#> 2049       b_cyl -3.179600735 1.304231e-03
#> 2050       b_cyl -3.176131314 1.317718e-03
#> 2051       b_cyl -3.172661893 1.330586e-03
#> 2052       b_cyl -3.169192471 1.342903e-03
#> 2053       b_cyl -3.165723050 1.354751e-03
#> 2054       b_cyl -3.162253628 1.366345e-03
#> 2055       b_cyl -3.158784207 1.377810e-03
#> 2056       b_cyl -3.155314786 1.389275e-03
#> 2057       b_cyl -3.151845364 1.400880e-03
#> 2058       b_cyl -3.148375943 1.412770e-03
#> 2059       b_cyl -3.144906521 1.425262e-03
#> 2060       b_cyl -3.141437100 1.438453e-03
#> 2061       b_cyl -3.137967679 1.452509e-03
#> 2062       b_cyl -3.134498257 1.467603e-03
#> 2063       b_cyl -3.131028836 1.483906e-03
#> 2064       b_cyl -3.127559414 1.501593e-03
#> 2065       b_cyl -3.124089993 1.521188e-03
#> 2066       b_cyl -3.120620572 1.542707e-03
#> 2067       b_cyl -3.117151150 1.566243e-03
#> 2068       b_cyl -3.113681729 1.591967e-03
#> 2069       b_cyl -3.110212307 1.620045e-03
#> 2070       b_cyl -3.106742886 1.650638e-03
#> 2071       b_cyl -3.103273465 1.684309e-03
#> 2072       b_cyl -3.099804043 1.721149e-03
#> 2073       b_cyl -3.096334622 1.761029e-03
#> 2074       b_cyl -3.092865200 1.804071e-03
#> 2075       b_cyl -3.089395779 1.850390e-03
#> 2076       b_cyl -3.085926358 1.900090e-03
#> 2077       b_cyl -3.082456936 1.953595e-03
#> 2078       b_cyl -3.078987515 2.011202e-03
#> 2079       b_cyl -3.075518093 2.072457e-03
#> 2080       b_cyl -3.072048672 2.137403e-03
#> 2081       b_cyl -3.068579251 2.206066e-03
#> 2082       b_cyl -3.065109829 2.278462e-03
#> 2083       b_cyl -3.061640408 2.354756e-03
#> 2084       b_cyl -3.058170986 2.435477e-03
#> 2085       b_cyl -3.054701565 2.519861e-03
#> 2086       b_cyl -3.051232144 2.607849e-03
#> 2087       b_cyl -3.047762722 2.699375e-03
#> 2088       b_cyl -3.044293301 2.794357e-03
#> 2089       b_cyl -3.040823879 2.892702e-03
#> 2090       b_cyl -3.037354458 2.995020e-03
#> 2091       b_cyl -3.033885037 3.100409e-03
#> 2092       b_cyl -3.030415615 3.208715e-03
#> 2093       b_cyl -3.026946194 3.319791e-03
#> 2094       b_cyl -3.023476772 3.433482e-03
#> 2095       b_cyl -3.020007351 3.549627e-03
#> 2096       b_cyl -3.016537930 3.668426e-03
#> 2097       b_cyl -3.013068508 3.789338e-03
#> 2098       b_cyl -3.009599087 3.912062e-03
#> 2099       b_cyl -3.006129665 4.036413e-03
#> 2100       b_cyl -3.002660244 4.162204e-03
#> 2101       b_cyl -2.999190823 4.289249e-03
#> 2102       b_cyl -2.995721401 4.417469e-03
#> 2103       b_cyl -2.992251980 4.546565e-03
#> 2104       b_cyl -2.988782558 4.676252e-03
#> 2105       b_cyl -2.985313137 4.806360e-03
#> 2106       b_cyl -2.981843716 4.936719e-03
#> 2107       b_cyl -2.978374294 5.067169e-03
#> 2108       b_cyl -2.974904873 5.197530e-03
#> 2109       b_cyl -2.971435451 5.327581e-03
#> 2110       b_cyl -2.967966030 5.457205e-03
#> 2111       b_cyl -2.964496609 5.586281e-03
#> 2112       b_cyl -2.961027187 5.714698e-03
#> 2113       b_cyl -2.957557766 5.842354e-03
#> 2114       b_cyl -2.954088344 5.969116e-03
#> 2115       b_cyl -2.950618923 6.094726e-03
#> 2116       b_cyl -2.947149502 6.219291e-03
#> 2117       b_cyl -2.943680080 6.342758e-03
#> 2118       b_cyl -2.940210659 6.465083e-03
#> 2119       b_cyl -2.936741238 6.586229e-03
#> 2120       b_cyl -2.933271816 6.706169e-03
#> 2121       b_cyl -2.929802395 6.824598e-03
#> 2122       b_cyl -2.926332973 6.941778e-03
#> 2123       b_cyl -2.922863552 7.057719e-03
#> 2124       b_cyl -2.919394131 7.172430e-03
#> 2125       b_cyl -2.915924709 7.285920e-03
#> 2126       b_cyl -2.912455288 7.398206e-03
#> 2127       b_cyl -2.908985866 7.509099e-03
#> 2128       b_cyl -2.905516445 7.618777e-03
#> 2129       b_cyl -2.902047024 7.727335e-03
#> 2130       b_cyl -2.898577602 7.834802e-03
#> 2131       b_cyl -2.895108181 7.941205e-03
#> 2132       b_cyl -2.891638759 8.046577e-03
#> 2133       b_cyl -2.888169338 8.150819e-03
#> 2134       b_cyl -2.884699917 8.254001e-03
#> 2135       b_cyl -2.881230495 8.356261e-03
#> 2136       b_cyl -2.877761074 8.457626e-03
#> 2137       b_cyl -2.874291652 8.558128e-03
#> 2138       b_cyl -2.870822231 8.657796e-03
#> 2139       b_cyl -2.867352810 8.756597e-03
#> 2140       b_cyl -2.863883388 8.854524e-03
#> 2141       b_cyl -2.860413967 8.951734e-03
#> 2142       b_cyl -2.856944545 9.048265e-03
#> 2143       b_cyl -2.853475124 9.144158e-03
#> 2144       b_cyl -2.850005703 9.239457e-03
#> 2145       b_cyl -2.846536281 9.334193e-03
#> 2146       b_cyl -2.843066860 9.428365e-03
#> 2147       b_cyl -2.839597438 9.522153e-03
#> 2148       b_cyl -2.836128017 9.615632e-03
#> 2149       b_cyl -2.832658596 9.708889e-03
#> 2150       b_cyl -2.829189174 9.802018e-03
#> 2151       b_cyl -2.825719753 9.895124e-03
#> 2152       b_cyl -2.822250331 9.988394e-03
#> 2153       b_cyl -2.818780910 1.008198e-02
#> 2154       b_cyl -2.815311489 1.017604e-02
#> 2155       b_cyl -2.811842067 1.027074e-02
#> 2156       b_cyl -2.808372646 1.036627e-02
#> 2157       b_cyl -2.804903224 1.046282e-02
#> 2158       b_cyl -2.801433803 1.056089e-02
#> 2159       b_cyl -2.797964382 1.066065e-02
#> 2160       b_cyl -2.794494960 1.076229e-02
#> 2161       b_cyl -2.791025539 1.086609e-02
#> 2162       b_cyl -2.787556117 1.097232e-02
#> 2163       b_cyl -2.784086696 1.108128e-02
#> 2164       b_cyl -2.780617275 1.119375e-02
#> 2165       b_cyl -2.777147853 1.131015e-02
#> 2166       b_cyl -2.773678432 1.143049e-02
#> 2167       b_cyl -2.770209010 1.155512e-02
#> 2168       b_cyl -2.766739589 1.168442e-02
#> 2169       b_cyl -2.763270168 1.181876e-02
#> 2170       b_cyl -2.759800746 1.195900e-02
#> 2171       b_cyl -2.756331325 1.210616e-02
#> 2172       b_cyl -2.752861903 1.225977e-02
#> 2173       b_cyl -2.749392482 1.242023e-02
#> 2174       b_cyl -2.745923061 1.258794e-02
#> 2175       b_cyl -2.742453639 1.276329e-02
#> 2176       b_cyl -2.738984218 1.294694e-02
#> 2177       b_cyl -2.735514796 1.314092e-02
#> 2178       b_cyl -2.732045375 1.334396e-02
#> 2179       b_cyl -2.728575954 1.355641e-02
#> 2180       b_cyl -2.725106532 1.377865e-02
#> 2181       b_cyl -2.721637111 1.401104e-02
#> 2182       b_cyl -2.718167689 1.425391e-02
#> 2183       b_cyl -2.714698268 1.451009e-02
#> 2184       b_cyl -2.711228847 1.477780e-02
#> 2185       b_cyl -2.707759425 1.505714e-02
#> 2186       b_cyl -2.704290004 1.534838e-02
#> 2187       b_cyl -2.700820582 1.565180e-02
#> 2188       b_cyl -2.697351161 1.596764e-02
#> 2189       b_cyl -2.693881740 1.629837e-02
#> 2190       b_cyl -2.690412318 1.664293e-02
#> 2191       b_cyl -2.686942897 1.700068e-02
#> 2192       b_cyl -2.683473475 1.737176e-02
#> 2193       b_cyl -2.680004054 1.775634e-02
#> 2194       b_cyl -2.676534633 1.815454e-02
#> 2195       b_cyl -2.673065211 1.856819e-02
#> 2196       b_cyl -2.669595790 1.899729e-02
#> 2197       b_cyl -2.666126368 1.944032e-02
#> 2198       b_cyl -2.662656947 1.989732e-02
#> 2199       b_cyl -2.659187526 2.036832e-02
#> 2200       b_cyl -2.655718104 2.085332e-02
#> 2201       b_cyl -2.652248683 2.135336e-02
#> 2202       b_cyl -2.648779261 2.186961e-02
#> 2203       b_cyl -2.645309840 2.239973e-02
#> 2204       b_cyl -2.641840419 2.294364e-02
#> 2205       b_cyl -2.638370997 2.350125e-02
#> 2206       b_cyl -2.634901576 2.407246e-02
#> 2207       b_cyl -2.631432154 2.465749e-02
#> 2208       b_cyl -2.627962733 2.525862e-02
#> 2209       b_cyl -2.624493312 2.587283e-02
#> 2210       b_cyl -2.621023890 2.649997e-02
#> 2211       b_cyl -2.617554469 2.713985e-02
#> 2212       b_cyl -2.614085047 2.779228e-02
#> 2213       b_cyl -2.610615626 2.845707e-02
#> 2214       b_cyl -2.607146205 2.913657e-02
#> 2215       b_cyl -2.603676783 2.982816e-02
#> 2216       b_cyl -2.600207362 3.053136e-02
#> 2217       b_cyl -2.596737940 3.124595e-02
#> 2218       b_cyl -2.593268519 3.197173e-02
#> 2219       b_cyl -2.589799098 3.270848e-02
#> 2220       b_cyl -2.586329676 3.345776e-02
#> 2221       b_cyl -2.582860255 3.421825e-02
#> 2222       b_cyl -2.579390833 3.498901e-02
#> 2223       b_cyl -2.575921412 3.576988e-02
#> 2224       b_cyl -2.572451991 3.656070e-02
#> 2225       b_cyl -2.568982569 3.736132e-02
#> 2226       b_cyl -2.565513148 3.817275e-02
#> 2227       b_cyl -2.562043726 3.899485e-02
#> 2228       b_cyl -2.558574305 3.982639e-02
#> 2229       b_cyl -2.555104884 4.066734e-02
#> 2230       b_cyl -2.551635462 4.151768e-02
#> 2231       b_cyl -2.548166041 4.237740e-02
#> 2232       b_cyl -2.544696619 4.324720e-02
#> 2233       b_cyl -2.541227198 4.412810e-02
#> 2234       b_cyl -2.537757777 4.501864e-02
#> 2235       b_cyl -2.534288355 4.591895e-02
#> 2236       b_cyl -2.530818934 4.682921e-02
#> 2237       b_cyl -2.527349512 4.774962e-02
#> 2238       b_cyl -2.523880091 4.868062e-02
#> 2239       b_cyl -2.520410670 4.962467e-02
#> 2240       b_cyl -2.516941248 5.057983e-02
#> 2241       b_cyl -2.513471827 5.154644e-02
#> 2242       b_cyl -2.510002405 5.252486e-02
#> 2243       b_cyl -2.506532984 5.351548e-02
#> 2244       b_cyl -2.503063563 5.451870e-02
#> 2245       b_cyl -2.499594141 5.553787e-02
#> 2246       b_cyl -2.496124720 5.657118e-02
#> 2247       b_cyl -2.492655298 5.761875e-02
#> 2248       b_cyl -2.489185877 5.868109e-02
#> 2249       b_cyl -2.485716456 5.975871e-02
#> 2250       b_cyl -2.482247034 6.085212e-02
#> 2251       b_cyl -2.478777613 6.196464e-02
#> 2252       b_cyl -2.475308191 6.309559e-02
#> 2253       b_cyl -2.471838770 6.424426e-02
#> 2254       b_cyl -2.468369349 6.541119e-02
#> 2255       b_cyl -2.464899927 6.659691e-02
#> 2256       b_cyl -2.461430506 6.780195e-02
#> 2257       b_cyl -2.457961084 6.902918e-02
#> 2258       b_cyl -2.454491663 7.027953e-02
#> 2259       b_cyl -2.451022242 7.155104e-02
#> 2260       b_cyl -2.447552820 7.284416e-02
#> 2261       b_cyl -2.444083399 7.415937e-02
#> 2262       b_cyl -2.440613977 7.549711e-02
#> 2263       b_cyl -2.437144556 7.685937e-02
#> 2264       b_cyl -2.433675135 7.824921e-02
#> 2265       b_cyl -2.430205713 7.966304e-02
#> 2266       b_cyl -2.426736292 8.110124e-02
#> 2267       b_cyl -2.423266870 8.256415e-02
#> 2268       b_cyl -2.419797449 8.405209e-02
#> 2269       b_cyl -2.416328028 8.556583e-02
#> 2270       b_cyl -2.412858606 8.711099e-02
#> 2271       b_cyl -2.409389185 8.868225e-02
#> 2272       b_cyl -2.405919763 9.027985e-02
#> 2273       b_cyl -2.402450342 9.190405e-02
#> 2274       b_cyl -2.398980921 9.355508e-02
#> 2275       b_cyl -2.395511499 9.523318e-02
#> 2276       b_cyl -2.392042078 9.694428e-02
#> 2277       b_cyl -2.388572656 9.868387e-02
#> 2278       b_cyl -2.385103235 1.004513e-01
#> 2279       b_cyl -2.381633814 1.022468e-01
#> 2280       b_cyl -2.378164392 1.040705e-01
#> 2281       b_cyl -2.374694971 1.059227e-01
#> 2282       b_cyl -2.371225549 1.078083e-01
#> 2283       b_cyl -2.367756128 1.097251e-01
#> 2284       b_cyl -2.364286707 1.116713e-01
#> 2285       b_cyl -2.360817285 1.136470e-01
#> 2286       b_cyl -2.357347864 1.156525e-01
#> 2287       b_cyl -2.353878442 1.176880e-01
#> 2288       b_cyl -2.350409021 1.197572e-01
#> 2289       b_cyl -2.346939600 1.218610e-01
#> 2290       b_cyl -2.343470178 1.239959e-01
#> 2291       b_cyl -2.340000757 1.261621e-01
#> 2292       b_cyl -2.336531335 1.283600e-01
#> 2293       b_cyl -2.333061914 1.305898e-01
#> 2294       b_cyl -2.329592493 1.328540e-01
#> 2295       b_cyl -2.326123071 1.351567e-01
#> 2296       b_cyl -2.322653650 1.374925e-01
#> 2297       b_cyl -2.319184228 1.398618e-01
#> 2298       b_cyl -2.315714807 1.422650e-01
#> 2299       b_cyl -2.312245386 1.447023e-01
#> 2300       b_cyl -2.308775964 1.471745e-01
#> 2301       b_cyl -2.305306543 1.496895e-01
#> 2302       b_cyl -2.301837121 1.522399e-01
#> 2303       b_cyl -2.298367700 1.548259e-01
#> 2304       b_cyl -2.294898279 1.574479e-01
#> 2305       b_cyl -2.291428857 1.601061e-01
#> 2306       b_cyl -2.287959436 1.628008e-01
#> 2307       b_cyl -2.284490014 1.655399e-01
#> 2308       b_cyl -2.281020593 1.683174e-01
#> 2309       b_cyl -2.277551172 1.711324e-01
#> 2310       b_cyl -2.274081750 1.739851e-01
#> 2311       b_cyl -2.270612329 1.768756e-01
#> 2312       b_cyl -2.267142907 1.798042e-01
#> 2313       b_cyl -2.263673486 1.827770e-01
#> 2314       b_cyl -2.260204065 1.857914e-01
#> 2315       b_cyl -2.256734643 1.888446e-01
#> 2316       b_cyl -2.253265222 1.919365e-01
#> 2317       b_cyl -2.249795800 1.950673e-01
#> 2318       b_cyl -2.246326379 1.982372e-01
#> 2319       b_cyl -2.242856958 2.014504e-01
#> 2320       b_cyl -2.239387536 2.047081e-01
#> 2321       b_cyl -2.235918115 2.080053e-01
#> 2322       b_cyl -2.232448693 2.113420e-01
#> 2323       b_cyl -2.228979272 2.147183e-01
#> 2324       b_cyl -2.225509851 2.181343e-01
#> 2325       b_cyl -2.222040429 2.215925e-01
#> 2326       b_cyl -2.218571008 2.250978e-01
#> 2327       b_cyl -2.215101586 2.286432e-01
#> 2328       b_cyl -2.211632165 2.322289e-01
#> 2329       b_cyl -2.208162744 2.358549e-01
#> 2330       b_cyl -2.204693322 2.395213e-01
#> 2331       b_cyl -2.201223901 2.432287e-01
#> 2332       b_cyl -2.197754479 2.469861e-01
#> 2333       b_cyl -2.194285058 2.507844e-01
#> 2334       b_cyl -2.190815637 2.546236e-01
#> 2335       b_cyl -2.187346215 2.585040e-01
#> 2336       b_cyl -2.183876794 2.624256e-01
#> 2337       b_cyl -2.180407372 2.663886e-01
#> 2338       b_cyl -2.176937951 2.704011e-01
#> 2339       b_cyl -2.173468530 2.744568e-01
#> 2340       b_cyl -2.169999108 2.785540e-01
#> 2341       b_cyl -2.166529687 2.826927e-01
#> 2342       b_cyl -2.163060265 2.868729e-01
#> 2343       b_cyl -2.159590844 2.910945e-01
#> 2344       b_cyl -2.156121423 2.953635e-01
#> 2345       b_cyl -2.152652001 2.996772e-01
#> 2346       b_cyl -2.149182580 3.040317e-01
#> 2347       b_cyl -2.145713158 3.084266e-01
#> 2348       b_cyl -2.142243737 3.128616e-01
#> 2349       b_cyl -2.138774316 3.173363e-01
#> 2350       b_cyl -2.135304894 3.218539e-01
#> 2351       b_cyl -2.131835473 3.264149e-01
#> 2352       b_cyl -2.128366051 3.310133e-01
#> 2353       b_cyl -2.124896630 3.356482e-01
#> 2354       b_cyl -2.121427209 3.403187e-01
#> 2355       b_cyl -2.117957787 3.450238e-01
#> 2356       b_cyl -2.114488366 3.497640e-01
#> 2357       b_cyl -2.111018944 3.545418e-01
#> 2358       b_cyl -2.107549523 3.593498e-01
#> 2359       b_cyl -2.104080102 3.641863e-01
#> 2360       b_cyl -2.100610680 3.690498e-01
#> 2361       b_cyl -2.097141259 3.739386e-01
#> 2362       b_cyl -2.093671837 3.788511e-01
#> 2363       b_cyl -2.090202416 3.837897e-01
#> 2364       b_cyl -2.086732995 3.887468e-01
#> 2365       b_cyl -2.083263573 3.937204e-01
#> 2366       b_cyl -2.079794152 3.987082e-01
#> 2367       b_cyl -2.076324730 4.037080e-01
#> 2368       b_cyl -2.072855309 4.087177e-01
#> 2369       b_cyl -2.069385888 4.137355e-01
#> 2370       b_cyl -2.065916466 4.187571e-01
#> 2371       b_cyl -2.062447045 4.237799e-01
#> 2372       b_cyl -2.058977623 4.288012e-01
#> 2373       b_cyl -2.055508202 4.338186e-01
#> 2374       b_cyl -2.052038781 4.388295e-01
#> 2375       b_cyl -2.048569359 4.438297e-01
#> 2376       b_cyl -2.045099938 4.488161e-01
#> 2377       b_cyl -2.041630516 4.537870e-01
#> 2378       b_cyl -2.038161095 4.587400e-01
#> 2379       b_cyl -2.034691674 4.636727e-01
#> 2380       b_cyl -2.031222252 4.685828e-01
#> 2381       b_cyl -2.027752831 4.734652e-01
#> 2382       b_cyl -2.024283409 4.783155e-01
#> 2383       b_cyl -2.020813988 4.831352e-01
#> 2384       b_cyl -2.017344567 4.879223e-01
#> 2385       b_cyl -2.013875145 4.926748e-01
#> 2386       b_cyl -2.010405724 4.973909e-01
#> 2387       b_cyl -2.006936302 5.020669e-01
#> 2388       b_cyl -2.003466881 5.066949e-01
#> 2389       b_cyl -1.999997460 5.112807e-01
#> 2390       b_cyl -1.996528038 5.158229e-01
#> 2391       b_cyl -1.993058617 5.203204e-01
#> 2392       b_cyl -1.989589195 5.247720e-01
#> 2393       b_cyl -1.986119774 5.291767e-01
#> 2394       b_cyl -1.982650353 5.335220e-01
#> 2395       b_cyl -1.979180931 5.378185e-01
#> 2396       b_cyl -1.975711510 5.420655e-01
#> 2397       b_cyl -1.972242088 5.462626e-01
#> 2398       b_cyl -1.968772667 5.504096e-01
#> 2399       b_cyl -1.965303246 5.545063e-01
#> 2400       b_cyl -1.961833824 5.585429e-01
#> 2401       b_cyl -1.958364403 5.625268e-01
#> 2402       b_cyl -1.954894981 5.664606e-01
#> 2403       b_cyl -1.951425560 5.703446e-01
#> 2404       b_cyl -1.947956139 5.741794e-01
#> 2405       b_cyl -1.944486717 5.779654e-01
#> 2406       b_cyl -1.941017296 5.816967e-01
#> 2407       b_cyl -1.937547874 5.853766e-01
#> 2408       b_cyl -1.934078453 5.890105e-01
#> 2409       b_cyl -1.930609032 5.925996e-01
#> 2410       b_cyl -1.927139610 5.961450e-01
#> 2411       b_cyl -1.923670189 5.996478e-01
#> 2412       b_cyl -1.920200767 6.031057e-01
#> 2413       b_cyl -1.916731346 6.065188e-01
#> 2414       b_cyl -1.913261925 6.098943e-01
#> 2415       b_cyl -1.909792503 6.132341e-01
#> 2416       b_cyl -1.906323082 6.165398e-01
#> 2417       b_cyl -1.902853660 6.198132e-01
#> 2418       b_cyl -1.899384239 6.230548e-01
#> 2419       b_cyl -1.895914818 6.262632e-01
#> 2420       b_cyl -1.892445396 6.294462e-01
#> 2421       b_cyl -1.888975975 6.326059e-01
#> 2422       b_cyl -1.885506553 6.357446e-01
#> 2423       b_cyl -1.882037132 6.388643e-01
#> 2424       b_cyl -1.878567711 6.419674e-01
#> 2425       b_cyl -1.875098289 6.450535e-01
#> 2426       b_cyl -1.871628868 6.481290e-01
#> 2427       b_cyl -1.868159446 6.511962e-01
#> 2428       b_cyl -1.864690025 6.542575e-01
#> 2429       b_cyl -1.861220604 6.573153e-01
#> 2430       b_cyl -1.857751182 6.603720e-01
#> 2431       b_cyl -1.854281761 6.634307e-01
#> 2432       b_cyl -1.850812339 6.664946e-01
#> 2433       b_cyl -1.847342918 6.695659e-01
#> 2434       b_cyl -1.843873497 6.726467e-01
#> 2435       b_cyl -1.840404075 6.757393e-01
#> 2436       b_cyl -1.836934654 6.788458e-01
#> 2437       b_cyl -1.833465232 6.819709e-01
#> 2438       b_cyl -1.829995811 6.851168e-01
#> 2439       b_cyl -1.826526390 6.882836e-01
#> 2440       b_cyl -1.823056968 6.914731e-01
#> 2441       b_cyl -1.819587547 6.946868e-01
#> 2442       b_cyl -1.816118125 6.979262e-01
#> 2443       b_cyl -1.812648704 7.011952e-01
#> 2444       b_cyl -1.809179283 7.044970e-01
#> 2445       b_cyl -1.805709861 7.078285e-01
#> 2446       b_cyl -1.802240440 7.111905e-01
#> 2447       b_cyl -1.798771018 7.145834e-01
#> 2448       b_cyl -1.795301597 7.180077e-01
#> 2449       b_cyl -1.791832176 7.214650e-01
#> 2450       b_cyl -1.788362754 7.249601e-01
#> 2451       b_cyl -1.784893333 7.284864e-01
#> 2452       b_cyl -1.781423911 7.320436e-01
#> 2453       b_cyl -1.777954490 7.356310e-01
#> 2454       b_cyl -1.774485069 7.392476e-01
#> 2455       b_cyl -1.771015647 7.428926e-01
#> 2456       b_cyl -1.767546226 7.465705e-01
#> 2457       b_cyl -1.764076804 7.502735e-01
#> 2458       b_cyl -1.760607383 7.539997e-01
#> 2459       b_cyl -1.757137962 7.577474e-01
#> 2460       b_cyl -1.753668540 7.615145e-01
#> 2461       b_cyl -1.750199119 7.652990e-01
#> 2462       b_cyl -1.746729697 7.691010e-01
#> 2463       b_cyl -1.743260276 7.729153e-01
#> 2464       b_cyl -1.739790855 7.767385e-01
#> 2465       b_cyl -1.736321433 7.805682e-01
#> 2466       b_cyl -1.732852012 7.844016e-01
#> 2467       b_cyl -1.729382590 7.882361e-01
#> 2468       b_cyl -1.725913169 7.920683e-01
#> 2469       b_cyl -1.722443748 7.958940e-01
#> 2470       b_cyl -1.718974326 7.997107e-01
#> 2471       b_cyl -1.715504905 8.035157e-01
#> 2472       b_cyl -1.712035483 8.073060e-01
#> 2473       b_cyl -1.708566062 8.110790e-01
#> 2474       b_cyl -1.705096641 8.148299e-01
#> 2475       b_cyl -1.701627219 8.185532e-01
#> 2476       b_cyl -1.698157798 8.222494e-01
#> 2477       b_cyl -1.694688376 8.259161e-01
#> 2478       b_cyl -1.691218955 8.295508e-01
#> 2479       b_cyl -1.687749534 8.331511e-01
#> 2480       b_cyl -1.684280112 8.367134e-01
#> 2481       b_cyl -1.680810691 8.402282e-01
#> 2482       b_cyl -1.677341269 8.437010e-01
#> 2483       b_cyl -1.673871848 8.471300e-01
#> 2484       b_cyl -1.670402427 8.505134e-01
#> 2485       b_cyl -1.666933005 8.538496e-01
#> 2486       b_cyl -1.663463584 8.571372e-01
#> 2487       b_cyl -1.659994162 8.603629e-01
#> 2488       b_cyl -1.656524741 8.635360e-01
#> 2489       b_cyl -1.653055320 8.666560e-01
#> 2490       b_cyl -1.649585898 8.697219e-01
#> 2491       b_cyl -1.646116477 8.727330e-01
#> 2492       b_cyl -1.642647055 8.756885e-01
#> 2493       b_cyl -1.639177634 8.785776e-01
#> 2494       b_cyl -1.635708213 8.814060e-01
#> 2495       b_cyl -1.632238791 8.841767e-01
#> 2496       b_cyl -1.628769370 8.868895e-01
#> 2497       b_cyl -1.625299948 8.895439e-01
#> 2498       b_cyl -1.621830527 8.921396e-01
#> 2499       b_cyl -1.618361106 8.946688e-01
#> 2500       b_cyl -1.614891684 8.971321e-01
#> 2501       b_cyl -1.611422263 8.995359e-01
#> 2502       b_cyl -1.607952841 9.018798e-01
#> 2503       b_cyl -1.604483420 9.041636e-01
#> 2504       b_cyl -1.601013999 9.063870e-01
#> 2505       b_cyl -1.597544577 9.085449e-01
#> 2506       b_cyl -1.594075156 9.106317e-01
#> 2507       b_cyl -1.590605734 9.126566e-01
#> 2508       b_cyl -1.587136313 9.146192e-01
#> 2509       b_cyl -1.583666892 9.165187e-01
#> 2510       b_cyl -1.580197470 9.183545e-01
#> 2511       b_cyl -1.576728049 9.201238e-01
#> 2512       b_cyl -1.573258627 9.218137e-01
#> 2513       b_cyl -1.569789206 9.234367e-01
#> 2514       b_cyl -1.566319785 9.249916e-01
#> 2515       b_cyl -1.562850363 9.264771e-01
#> 2516       b_cyl -1.559380942 9.278920e-01
#> 2517       b_cyl -1.555911520 9.292350e-01
#> 2518       b_cyl -1.552442099 9.304878e-01
#> 2519       b_cyl -1.548972678 9.316634e-01
#> 2520       b_cyl -1.545503256 9.327613e-01
#> 2521       b_cyl -1.542033835 9.337799e-01
#> 2522       b_cyl -1.538564413 9.347175e-01
#> 2523       b_cyl -1.535094992 9.355723e-01
#> 2524       b_cyl -1.531625571 9.363274e-01
#> 2525       b_cyl -1.528156149 9.369893e-01
#> 2526       b_cyl -1.524686728 9.375619e-01
#> 2527       b_cyl -1.521217306 9.380433e-01
#> 2528       b_cyl -1.517747885 9.384319e-01
#> 2529       b_cyl -1.514278464 9.387259e-01
#> 2530       b_cyl -1.510809042 9.389116e-01
#> 2531       b_cyl -1.507339621 9.389877e-01
#> 2532       b_cyl -1.503870200 9.389637e-01
#> 2533       b_cyl -1.500400778 9.388386e-01
#> 2534       b_cyl -1.496931357 9.386112e-01
#> 2535       b_cyl -1.493461935 9.382804e-01
#> 2536       b_cyl -1.489992514 9.378377e-01
#> 2537       b_cyl -1.486523093 9.372730e-01
#> 2538       b_cyl -1.483053671 9.366028e-01
#> 2539       b_cyl -1.479584250 9.358271e-01
#> 2540       b_cyl -1.476114828 9.349460e-01
#> 2541       b_cyl -1.472645407 9.339596e-01
#> 2542       b_cyl -1.469175986 9.328657e-01
#> 2543       b_cyl -1.465706564 9.316461e-01
#> 2544       b_cyl -1.462237143 9.303239e-01
#> 2545       b_cyl -1.458767721 9.289004e-01
#> 2546       b_cyl -1.455298300 9.273771e-01
#> 2547       b_cyl -1.451828879 9.257557e-01
#> 2548       b_cyl -1.448359457 9.240379e-01
#> 2549       b_cyl -1.444890036 9.222062e-01
#> 2550       b_cyl -1.441420614 9.202819e-01
#> 2551       b_cyl -1.437951193 9.182695e-01
#> 2552       b_cyl -1.434481772 9.161719e-01
#> 2553       b_cyl -1.431012350 9.139917e-01
#> 2554       b_cyl -1.427542929 9.117320e-01
#> 2555       b_cyl -1.424073507 9.093837e-01
#> 2556       b_cyl -1.420604086 9.069588e-01
#> 2557       b_cyl -1.417134665 9.044659e-01
#> 2558       b_cyl -1.413665243 9.019084e-01
#> 2559       b_cyl -1.410195822 8.992897e-01
#> 2560       b_cyl -1.406726400 8.966132e-01
#> 2561       b_cyl -1.403256979 8.938765e-01
#> 2562       b_cyl -1.399787558 8.910848e-01
#> 2563       b_cyl -1.396318136 8.882477e-01
#> 2564       b_cyl -1.392848715 8.853685e-01
#> 2565       b_cyl -1.389379293 8.824507e-01
#> 2566       b_cyl -1.385909872 8.794974e-01
#> 2567       b_cyl -1.382440451 8.765097e-01
#> 2568       b_cyl -1.378971029 8.734892e-01
#> 2569       b_cyl -1.375501608 8.704439e-01
#> 2570       b_cyl -1.372032186 8.673765e-01
#> 2571       b_cyl -1.368562765 8.642896e-01
#> 2572       b_cyl -1.365093344 8.611855e-01
#> 2573       b_cyl -1.361623922 8.580662e-01
#> 2574       b_cyl -1.358154501 8.549321e-01
#> 2575       b_cyl -1.354685079 8.517881e-01
#> 2576       b_cyl -1.351215658 8.486357e-01
#> 2577       b_cyl -1.347746237 8.454765e-01
#> 2578       b_cyl -1.344276815 8.423116e-01
#> 2579       b_cyl -1.340807394 8.391421e-01
#> 2580       b_cyl -1.337337972 8.359684e-01
#> 2581       b_cyl -1.333868551 8.327921e-01
#> 2582       b_cyl -1.330399130 8.296135e-01
#> 2583       b_cyl -1.326929708 8.264330e-01
#> 2584       b_cyl -1.323460287 8.232507e-01
#> 2585       b_cyl -1.319990865 8.200666e-01
#> 2586       b_cyl -1.316521444 8.168801e-01
#> 2587       b_cyl -1.313052023 8.136908e-01
#> 2588       b_cyl -1.309582601 8.104985e-01
#> 2589       b_cyl -1.306113180 8.073023e-01
#> 2590       b_cyl -1.302643758 8.041017e-01
#> 2591       b_cyl -1.299174337 8.008956e-01
#> 2592       b_cyl -1.295704916 7.976824e-01
#> 2593       b_cyl -1.292235494 7.944604e-01
#> 2594       b_cyl -1.288766073 7.912293e-01
#> 2595       b_cyl -1.285296651 7.879879e-01
#> 2596       b_cyl -1.281827230 7.847349e-01
#> 2597       b_cyl -1.278357809 7.814691e-01
#> 2598       b_cyl -1.274888387 7.781882e-01
#> 2599       b_cyl -1.271418966 7.748886e-01
#> 2600       b_cyl -1.267949544 7.715715e-01
#> 2601       b_cyl -1.264480123 7.682356e-01
#> 2602       b_cyl -1.261010702 7.648794e-01
#> 2603       b_cyl -1.257541280 7.615018e-01
#> 2604       b_cyl -1.254071859 7.581011e-01
#> 2605       b_cyl -1.250602437 7.546706e-01
#> 2606       b_cyl -1.247133016 7.512143e-01
#> 2607       b_cyl -1.243663595 7.477311e-01
#> 2608       b_cyl -1.240194173 7.442199e-01
#> 2609       b_cyl -1.236724752 7.406797e-01
#> 2610       b_cyl -1.233255330 7.371096e-01
#> 2611       b_cyl -1.229785909 7.335020e-01
#> 2612       b_cyl -1.226316488 7.298616e-01
#> 2613       b_cyl -1.222847066 7.261885e-01
#> 2614       b_cyl -1.219377645 7.224822e-01
#> 2615       b_cyl -1.215908223 7.187423e-01
#> 2616       b_cyl -1.212438802 7.149683e-01
#> 2617       b_cyl -1.208969381 7.111544e-01
#> 2618       b_cyl -1.205499959 7.073032e-01
#> 2619       b_cyl -1.202030538 7.034172e-01
#> 2620       b_cyl -1.198561116 6.994965e-01
#> 2621       b_cyl -1.195091695 6.955413e-01
#> 2622       b_cyl -1.191622274 6.915518e-01
#> 2623       b_cyl -1.188152852 6.875246e-01
#> 2624       b_cyl -1.184683431 6.834597e-01
#> 2625       b_cyl -1.181214009 6.793620e-01
#> 2626       b_cyl -1.177744588 6.752323e-01
#> 2627       b_cyl -1.174275167 6.710711e-01
#> 2628       b_cyl -1.170805745 6.668794e-01
#> 2629       b_cyl -1.167336324 6.626563e-01
#> 2630       b_cyl -1.163866902 6.583999e-01
#> 2631       b_cyl -1.160397481 6.541165e-01
#> 2632       b_cyl -1.156928060 6.498074e-01
#> 2633       b_cyl -1.153458638 6.454737e-01
#> 2634       b_cyl -1.149989217 6.411168e-01
#> 2635       b_cyl -1.146519795 6.367378e-01
#> 2636       b_cyl -1.143050374 6.323343e-01
#> 2637       b_cyl -1.139580953 6.279128e-01
#> 2638       b_cyl -1.136111531 6.234749e-01
#> 2639       b_cyl -1.132642110 6.190221e-01
#> 2640       b_cyl -1.129172688 6.145560e-01
#> 2641       b_cyl -1.125703267 6.100785e-01
#> 2642       b_cyl -1.122233846 6.055897e-01
#> 2643       b_cyl -1.118764424 6.010935e-01
#> 2644       b_cyl -1.115295003 5.965921e-01
#> 2645       b_cyl -1.111825581 5.920873e-01
#> 2646       b_cyl -1.108356160 5.875809e-01
#> 2647       b_cyl -1.104886739 5.830745e-01
#> 2648       b_cyl -1.101417317 5.785709e-01
#> 2649       b_cyl -1.097947896 5.740723e-01
#> 2650       b_cyl -1.094478474 5.695805e-01
#> 2651       b_cyl -1.091009053 5.650970e-01
#> 2652       b_cyl -1.087539632 5.606237e-01
#> 2653       b_cyl -1.084070210 5.561624e-01
#> 2654       b_cyl -1.080600789 5.517163e-01
#> 2655       b_cyl -1.077131367 5.472885e-01
#> 2656       b_cyl -1.073661946 5.428785e-01
#> 2657       b_cyl -1.070192525 5.384880e-01
#> 2658       b_cyl -1.066723103 5.341186e-01
#> 2659       b_cyl -1.063253682 5.297715e-01
#> 2660       b_cyl -1.059784260 5.254498e-01
#> 2661       b_cyl -1.056314839 5.211584e-01
#> 2662       b_cyl -1.052845418 5.168943e-01
#> 2663       b_cyl -1.049375996 5.126585e-01
#> 2664       b_cyl -1.045906575 5.084522e-01
#> 2665       b_cyl -1.042437153 5.042764e-01
#> 2666       b_cyl -1.038967732 5.001322e-01
#> 2667       b_cyl -1.035498311 4.960280e-01
#> 2668       b_cyl -1.032028889 4.919571e-01
#> 2669       b_cyl -1.028559468 4.879203e-01
#> 2670       b_cyl -1.025090046 4.839179e-01
#> 2671       b_cyl -1.021620625 4.799504e-01
#> 2672       b_cyl -1.018151204 4.760182e-01
#> 2673       b_cyl -1.014681782 4.721284e-01
#> 2674       b_cyl -1.011212361 4.682756e-01
#> 2675       b_cyl -1.007742939 4.644584e-01
#> 2676       b_cyl -1.004273518 4.606764e-01
#> 2677       b_cyl -1.000804097 4.569296e-01
#> 2678       b_cyl -0.997334675 4.532176e-01
#> 2679       b_cyl -0.993865254 4.495450e-01
#> 2680       b_cyl -0.990395832 4.459088e-01
#> 2681       b_cyl -0.986926411 4.423054e-01
#> 2682       b_cyl -0.983456990 4.387341e-01
#> 2683       b_cyl -0.979987568 4.351939e-01
#> 2684       b_cyl -0.976518147 4.316841e-01
#> 2685       b_cyl -0.973048725 4.282063e-01
#> 2686       b_cyl -0.969579304 4.247601e-01
#> 2687       b_cyl -0.966109883 4.213404e-01
#> 2688       b_cyl -0.962640461 4.179458e-01
#> 2689       b_cyl -0.959171040 4.145752e-01
#> 2690       b_cyl -0.955701618 4.112273e-01
#> 2691       b_cyl -0.952232197 4.079018e-01
#> 2692       b_cyl -0.948762776 4.045993e-01
#> 2693       b_cyl -0.945293354 4.013145e-01
#> 2694       b_cyl -0.941823933 3.980460e-01
#> 2695       b_cyl -0.938354511 3.947924e-01
#> 2696       b_cyl -0.934885090 3.915523e-01
#> 2697       b_cyl -0.931415669 3.883242e-01
#> 2698       b_cyl -0.927946247 3.851086e-01
#> 2699       b_cyl -0.924476826 3.819013e-01
#> 2700       b_cyl -0.921007404 3.787010e-01
#> 2701       b_cyl -0.917537983 3.755062e-01
#> 2702       b_cyl -0.914068562 3.723156e-01
#> 2703       b_cyl -0.910599140 3.691279e-01
#> 2704       b_cyl -0.907129719 3.659419e-01
#> 2705       b_cyl -0.903660297 3.627555e-01
#> 2706       b_cyl -0.900190876 3.595677e-01
#> 2707       b_cyl -0.896721455 3.563773e-01
#> 2708       b_cyl -0.893252033 3.531833e-01
#> 2709       b_cyl -0.889782612 3.499845e-01
#> 2710       b_cyl -0.886313190 3.467792e-01
#> 2711       b_cyl -0.882843769 3.435662e-01
#> 2712       b_cyl -0.879374348 3.403453e-01
#> 2713       b_cyl -0.875904926 3.371157e-01
#> 2714       b_cyl -0.872435505 3.338769e-01
#> 2715       b_cyl -0.868966083 3.306280e-01
#> 2716       b_cyl -0.865496662 3.273675e-01
#> 2717       b_cyl -0.862027241 3.240940e-01
#> 2718       b_cyl -0.858557819 3.208088e-01
#> 2719       b_cyl -0.855088398 3.175115e-01
#> 2720       b_cyl -0.851618976 3.142018e-01
#> 2721       b_cyl -0.848149555 3.108795e-01
#> 2722       b_cyl -0.844680134 3.075438e-01
#> 2723       b_cyl -0.841210712 3.041927e-01
#> 2724       b_cyl -0.837741291 3.008289e-01
#> 2725       b_cyl -0.834271869 2.974524e-01
#> 2726       b_cyl -0.830802448 2.940634e-01
#> 2727       b_cyl -0.827333027 2.906622e-01
#> 2728       b_cyl -0.823863605 2.872491e-01
#> 2729       b_cyl -0.820394184 2.838220e-01
#> 2730       b_cyl -0.816924762 2.803842e-01
#> 2731       b_cyl -0.813455341 2.769365e-01
#> 2732       b_cyl -0.809985920 2.734796e-01
#> 2733       b_cyl -0.806516498 2.700143e-01
#> 2734       b_cyl -0.803047077 2.665414e-01
#> 2735       b_cyl -0.799577655 2.630610e-01
#> 2736       b_cyl -0.796108234 2.595755e-01
#> 2737       b_cyl -0.792638813 2.560864e-01
#> 2738       b_cyl -0.789169391 2.525951e-01
#> 2739       b_cyl -0.785699970 2.491028e-01
#> 2740       b_cyl -0.782230548 2.456110e-01
#> 2741       b_cyl -0.778761127 2.421219e-01
#> 2742       b_cyl -0.775291706 2.386378e-01
#> 2743       b_cyl -0.771822284 2.351600e-01
#> 2744       b_cyl -0.768352863 2.316904e-01
#> 2745       b_cyl -0.764883441 2.282309e-01
#> 2746       b_cyl -0.761414020 2.247833e-01
#> 2747       b_cyl -0.757944599 2.213511e-01
#> 2748       b_cyl -0.754475177 2.179381e-01
#> 2749       b_cyl -0.751005756 2.145442e-01
#> 2750       b_cyl -0.747536334 2.111715e-01
#> 2751       b_cyl -0.744066913 2.078220e-01
#> 2752       b_cyl -0.740597492 2.044978e-01
#> 2753       b_cyl -0.737128070 2.012023e-01
#> 2754       b_cyl -0.733658649 1.979428e-01
#> 2755       b_cyl -0.730189227 1.947159e-01
#> 2756       b_cyl -0.726719806 1.915235e-01
#> 2757       b_cyl -0.723250385 1.883676e-01
#> 2758       b_cyl -0.719780963 1.852500e-01
#> 2759       b_cyl -0.716311542 1.821723e-01
#> 2760       b_cyl -0.712842120 1.791467e-01
#> 2761       b_cyl -0.709372699 1.761654e-01
#> 2762       b_cyl -0.705903278 1.732297e-01
#> 2763       b_cyl -0.702433856 1.703408e-01
#> 2764       b_cyl -0.698964435 1.674999e-01
#> 2765       b_cyl -0.695495013 1.647081e-01
#> 2766       b_cyl -0.692025592 1.619757e-01
#> 2767       b_cyl -0.688556171 1.592971e-01
#> 2768       b_cyl -0.685086749 1.566702e-01
#> 2769       b_cyl -0.681617328 1.540952e-01
#> 2770       b_cyl -0.678147906 1.515725e-01
#> 2771       b_cyl -0.674678485 1.491022e-01
#> 2772       b_cyl -0.671209064 1.466912e-01
#> 2773       b_cyl -0.667739642 1.443375e-01
#> 2774       b_cyl -0.664270221 1.420351e-01
#> 2775       b_cyl -0.660800799 1.397835e-01
#> 2776       b_cyl -0.657331378 1.375820e-01
#> 2777       b_cyl -0.653861957 1.354298e-01
#> 2778       b_cyl -0.650392535 1.333299e-01
#> 2779       b_cyl -0.646923114 1.312838e-01
#> 2780       b_cyl -0.643453692 1.292828e-01
#> 2781       b_cyl -0.639984271 1.273257e-01
#> 2782       b_cyl -0.636514850 1.254110e-01
#> 2783       b_cyl -0.633045428 1.235372e-01
#> 2784       b_cyl -0.629576007 1.217042e-01
#> 2785       b_cyl -0.626106585 1.199158e-01
#> 2786       b_cyl -0.622637164 1.181624e-01
#> 2787       b_cyl -0.619167743 1.164424e-01
#> 2788       b_cyl -0.615698321 1.147540e-01
#> 2789       b_cyl -0.612228900 1.130956e-01
#> 2790       b_cyl -0.608759478 1.114654e-01
#> 2791       b_cyl -0.605290057 1.098674e-01
#> 2792       b_cyl -0.601820636 1.082935e-01
#> 2793       b_cyl -0.598351214 1.067420e-01
#> 2794       b_cyl -0.594881793 1.052115e-01
#> 2795       b_cyl -0.591412371 1.037004e-01
#> 2796       b_cyl -0.587942950 1.022075e-01
#> 2797       b_cyl -0.584473529 1.007340e-01
#> 2798       b_cyl -0.581004107 9.927636e-02
#> 2799       b_cyl -0.577534686 9.783256e-02
#> 2800       b_cyl -0.574065264 9.640166e-02
#> 2801       b_cyl -0.570595843 9.498279e-02
#> 2802       b_cyl -0.567126422 9.357518e-02
#> 2803       b_cyl -0.563657000 9.217946e-02
#> 2804       b_cyl -0.560187579 9.079460e-02
#> 2805       b_cyl -0.556718157 8.941911e-02
#> 2806       b_cyl -0.553248736 8.805272e-02
#> 2807       b_cyl -0.549779315 8.669525e-02
#> 2808       b_cyl -0.546309893 8.534660e-02
#> 2809       b_cyl -0.542840472 8.400751e-02
#> 2810       b_cyl -0.539371050 8.267875e-02
#> 2811       b_cyl -0.535901629 8.135920e-02
#> 2812       b_cyl -0.532432208 8.004918e-02
#> 2813       b_cyl -0.528962786 7.874903e-02
#> 2814       b_cyl -0.525493365 7.745917e-02
#> 2815       b_cyl -0.522023943 7.618047e-02
#> 2816       b_cyl -0.518554522 7.491558e-02
#> 2817       b_cyl -0.515085101 7.366289e-02
#> 2818       b_cyl -0.511615679 7.242302e-02
#> 2819       b_cyl -0.508146258 7.119660e-02
#> 2820       b_cyl -0.504676836 6.998427e-02
#> 2821       b_cyl -0.501207415 6.878668e-02
#> 2822       b_cyl -0.497737994 6.760816e-02
#> 2823       b_cyl -0.494268572 6.644627e-02
#> 2824       b_cyl -0.490799151 6.530140e-02
#> 2825       b_cyl -0.487329729 6.417415e-02
#> 2826       b_cyl -0.483860308 6.306505e-02
#> 2827       b_cyl -0.480390887 6.197466e-02
#> 2828       b_cyl -0.476921465 6.090694e-02
#> 2829       b_cyl -0.473452044 5.986026e-02
#> 2830       b_cyl -0.469982622 5.883379e-02
#> 2831       b_cyl -0.466513201 5.782781e-02
#> 2832       b_cyl -0.463043780 5.684257e-02
#> 2833       b_cyl -0.459574358 5.587824e-02
#> 2834       b_cyl -0.456104937 5.493767e-02
#> 2835       b_cyl -0.452635515 5.402051e-02
#> 2836       b_cyl -0.449166094 5.312440e-02
#> 2837       b_cyl -0.445696673 5.224921e-02
#> 2838       b_cyl -0.442227251 5.139477e-02
#> 2839       b_cyl -0.438757830 5.056085e-02
#> 2840       b_cyl -0.435288408 4.974872e-02
#> 2841       b_cyl -0.431818987 4.895941e-02
#> 2842       b_cyl -0.428349566 4.818927e-02
#> 2843       b_cyl -0.424880144 4.743779e-02
#> 2844       b_cyl -0.421410723 4.670442e-02
#> 2845       b_cyl -0.417941301 4.598857e-02
#> 2846       b_cyl -0.414471880 4.529010e-02
#> 2847       b_cyl -0.411002459 4.461092e-02
#> 2848       b_cyl -0.407533037 4.394680e-02
#> 2849       b_cyl -0.404063616 4.329699e-02
#> 2850       b_cyl -0.400594194 4.266072e-02
#> 2851       b_cyl -0.397124773 4.203721e-02
#> 2852       b_cyl -0.393655352 4.142567e-02
#> 2853       b_cyl -0.390185930 4.082745e-02
#> 2854       b_cyl -0.386716509 4.023928e-02
#> 2855       b_cyl -0.383247087 3.966016e-02
#> 2856       b_cyl -0.379777666 3.908931e-02
#> 2857       b_cyl -0.376308245 3.852595e-02
#> 2858       b_cyl -0.372838823 3.796934e-02
#> 2859       b_cyl -0.369369402 3.741956e-02
#> 2860       b_cyl -0.365899980 3.687504e-02
#> 2861       b_cyl -0.362430559 3.633474e-02
#> 2862       b_cyl -0.358961138 3.579805e-02
#> 2863       b_cyl -0.355491716 3.526438e-02
#> 2864       b_cyl -0.352022295 3.473319e-02
#> 2865       b_cyl -0.348552873 3.420414e-02
#> 2866       b_cyl -0.345083452 3.367656e-02
#> 2867       b_cyl -0.341614031 3.314988e-02
#> 2868       b_cyl -0.338144609 3.262377e-02
#> 2869       b_cyl -0.334675188 3.209795e-02
#> 2870       b_cyl -0.331205766 3.157218e-02
#> 2871       b_cyl -0.327736345 3.104626e-02
#> 2872       b_cyl -0.324266924 3.051998e-02
#> 2873       b_cyl -0.320797502 2.999331e-02
#> 2874       b_cyl -0.317328081 2.946627e-02
#> 2875       b_cyl -0.313858659 2.893887e-02
#> 2876       b_cyl -0.310389238 2.841119e-02
#> 2877       b_cyl -0.306919817 2.788334e-02
#> 2878       b_cyl -0.303450395 2.735555e-02
#> 2879       b_cyl -0.299980974 2.682807e-02
#> 2880       b_cyl -0.296511552 2.630114e-02
#> 2881       b_cyl -0.293042131 2.577505e-02
#> 2882       b_cyl -0.289572710 2.525007e-02
#> 2883       b_cyl -0.286103288 2.472654e-02
#> 2884       b_cyl -0.282633867 2.420527e-02
#> 2885       b_cyl -0.279164445 2.368640e-02
#> 2886       b_cyl -0.275695024 2.317025e-02
#> 2887       b_cyl -0.272225603 2.265721e-02
#> 2888       b_cyl -0.268756181 2.214765e-02
#> 2889       b_cyl -0.265286760 2.164197e-02
#> 2890       b_cyl -0.261817338 2.114136e-02
#> 2891       b_cyl -0.258347917 2.064592e-02
#> 2892       b_cyl -0.254878496 2.015572e-02
#> 2893       b_cyl -0.251409074 1.967112e-02
#> 2894       b_cyl -0.247939653 1.919246e-02
#> 2895       b_cyl -0.244470231 1.872007e-02
#> 2896       b_cyl -0.241000810 1.825511e-02
#> 2897       b_cyl -0.237531389 1.779801e-02
#> 2898       b_cyl -0.234061967 1.734824e-02
#> 2899       b_cyl -0.230592546 1.690604e-02
#> 2900       b_cyl -0.227123124 1.647163e-02
#> 2901       b_cyl -0.223653703 1.604523e-02
#> 2902       b_cyl -0.220184282 1.562761e-02
#> 2903       b_cyl -0.216714860 1.521979e-02
#> 2904       b_cyl -0.213245439 1.482053e-02
#> 2905       b_cyl -0.209776017 1.442993e-02
#> 2906       b_cyl -0.206306596 1.404805e-02
#> 2907       b_cyl -0.202837175 1.367497e-02
#> 2908       b_cyl -0.199367753 1.331093e-02
#> 2909       b_cyl -0.195898332 1.295766e-02
#> 2910       b_cyl -0.192428910 1.261325e-02
#> 2911       b_cyl -0.188959489 1.227768e-02
#> 2912       b_cyl -0.185490068 1.195090e-02
#> 2913       b_cyl -0.182020646 1.163288e-02
#> 2914       b_cyl -0.178551225 1.132354e-02
#> 2915       b_cyl -0.175081803 1.102461e-02
#> 2916       b_cyl -0.171612382 1.073439e-02
#> 2917       b_cyl -0.168142961 1.045255e-02
#> 2918       b_cyl -0.164673539 1.017898e-02
#> 2919       b_cyl -0.161204118 9.913579e-03
#> 2920       b_cyl -0.157734696 9.656235e-03
#> 2921       b_cyl -0.154265275 9.408094e-03
#> 2922       b_cyl -0.150795854 9.168290e-03
#> 2923       b_cyl -0.147326432 8.936109e-03
#> 2924       b_cyl -0.143857011 8.711428e-03
#> 2925       b_cyl -0.140387589 8.494125e-03
#> 2926       b_cyl -0.136918168 8.284078e-03
#> 2927       b_cyl -0.133448747 8.081962e-03
#> 2928       b_cyl -0.129979325 7.887665e-03
#> 2929       b_cyl -0.126509904 7.700190e-03
#> 2930       b_cyl -0.123040482 7.519422e-03
#> 2931       b_cyl -0.119571061 7.345247e-03
#> 2932       b_cyl -0.116101640 7.177553e-03
#> 2933       b_cyl -0.112632218 7.016638e-03
#> 2934       b_cyl -0.109162797 6.863006e-03
#> 2935       b_cyl -0.105693375 6.715459e-03
#> 2936       b_cyl -0.102223954 6.573887e-03
#> 2937       b_cyl -0.098754533 6.438185e-03
#> 2938       b_cyl -0.095285111 6.308246e-03
#> 2939       b_cyl -0.091815690 6.184061e-03
#> 2940       b_cyl -0.088346269 6.066602e-03
#> 2941       b_cyl -0.084876847 5.954516e-03
#> 2942       b_cyl -0.081407426 5.847690e-03
#> 2943       b_cyl -0.077938004 5.746012e-03
#> 2944       b_cyl -0.074468583 5.649367e-03
#> 2945       b_cyl -0.070999162 5.557637e-03
#> 2946       b_cyl -0.067529740 5.471656e-03
#> 2947       b_cyl -0.064060319 5.390414e-03
#> 2948       b_cyl -0.060590897 5.313636e-03
#> 2949       b_cyl -0.057121476 5.241187e-03
#> 2950       b_cyl -0.053652055 5.172932e-03
#> 2951       b_cyl -0.050182633 5.108730e-03
#> 2952       b_cyl -0.046713212 5.049023e-03
#> 2953       b_cyl -0.043243790 4.993289e-03
#> 2954       b_cyl -0.039774369 4.941068e-03
#> 2955       b_cyl -0.036304948 4.892199e-03
#> 2956       b_cyl -0.032835526 4.846522e-03
#> 2957       b_cyl -0.029366105 4.803871e-03
#> 2958       b_cyl -0.025896683 4.764367e-03
#> 2959       b_cyl -0.022427262 4.727805e-03
#> 2960       b_cyl -0.018957841 4.693646e-03
#> 2961       b_cyl -0.015488419 4.661715e-03
#> 2962       b_cyl -0.012018998 4.631831e-03
#> 2963       b_cyl -0.008549576 4.603816e-03
#> 2964       b_cyl -0.005080155 4.577577e-03
#> 2965       b_cyl -0.001610734 4.553026e-03
#> 2966       b_cyl  0.001858688 4.529688e-03
#> 2967       b_cyl  0.005328109 4.507383e-03
#> 2968       b_cyl  0.008797531 4.485934e-03
#> 2969       b_cyl  0.012266952 4.465165e-03
#> 2970       b_cyl  0.015736373 4.444904e-03
#> 2971       b_cyl  0.019205795 4.424986e-03
#> 2972       b_cyl  0.022675216 4.405135e-03
#> 2973       b_cyl  0.026144638 4.385191e-03
#> 2974       b_cyl  0.029614059 4.365001e-03
#> 2975       b_cyl  0.033083480 4.344416e-03
#> 2976       b_cyl  0.036552902 4.323293e-03
#> 2977       b_cyl  0.040022323 4.301316e-03
#> 2978       b_cyl  0.043491745 4.278435e-03
#> 2979       b_cyl  0.046961166 4.254560e-03
#> 2980       b_cyl  0.050430587 4.229586e-03
#> 2981       b_cyl  0.053900009 4.203415e-03
#> 2982       b_cyl  0.057369430 4.175956e-03
#> 2983       b_cyl  0.060838852 4.146903e-03
#> 2984       b_cyl  0.064308273 4.116263e-03
#> 2985       b_cyl  0.067777694 4.084100e-03
#> 2986       b_cyl  0.071247116 4.050376e-03
#> 2987       b_cyl  0.074716537 4.015061e-03
#> 2988       b_cyl  0.078185959 3.978138e-03
#> 2989       b_cyl  0.081655380 3.939427e-03
#> 2990       b_cyl  0.085124801 3.898898e-03
#> 2991       b_cyl  0.088594223 3.856791e-03
#> 2992       b_cyl  0.092063644 3.813142e-03
#> 2993       b_cyl  0.095533066 3.767998e-03
#> 2994       b_cyl  0.099002487 3.721413e-03
#> 2995       b_cyl  0.102471908 3.673381e-03
#> 2996       b_cyl  0.105941330 3.623857e-03
#> 2997       b_cyl  0.109410751 3.573184e-03
#> 2998       b_cyl  0.112880173 3.521465e-03
#> 2999       b_cyl  0.116349594 3.468810e-03
#> 3000       b_cyl  0.119819015 3.415337e-03
#> 3001       b_cyl  0.123288437 3.361166e-03
#> 3002       b_cyl  0.126757858 3.306357e-03
#> 3003       b_cyl  0.130227280 3.251211e-03
#> 3004       b_cyl  0.133696701 3.195872e-03
#> 3005       b_cyl  0.137166122 3.140486e-03
#> 3006       b_cyl  0.140635544 3.085204e-03
#> 3007       b_cyl  0.144104965 3.030175e-03
#> 3008       b_cyl  0.147574387 2.975678e-03
#> 3009       b_cyl  0.151043808 2.921847e-03
#> 3010       b_cyl  0.154513229 2.868811e-03
#> 3011       b_cyl  0.157982651 2.816715e-03
#> 3012       b_cyl  0.161452072 2.765698e-03
#> 3013       b_cyl  0.164921494 2.715896e-03
#> 3014       b_cyl  0.168390915 2.667670e-03
#> 3015       b_cyl  0.171860336 2.621106e-03
#> 3016       b_cyl  0.175329758 2.576193e-03
#> 3017       b_cyl  0.178799179 2.533033e-03
#> 3018       b_cyl  0.182268601 2.491718e-03
#> 3019       b_cyl  0.185738022 2.452332e-03
#> 3020       b_cyl  0.189207443 2.415162e-03
#> 3021       b_cyl  0.192676865 2.380367e-03
#> 3022       b_cyl  0.196146286 2.347715e-03
#> 3023       b_cyl  0.199615708 2.317236e-03
#> 3024       b_cyl  0.203085129 2.288952e-03
#> 3025       b_cyl  0.206554550 2.262874e-03
#> 3026       b_cyl  0.210023972 2.239116e-03
#> 3027       b_cyl  0.213493393 2.217947e-03
#> 3028       b_cyl  0.216962815 2.198923e-03
#> 3029       b_cyl  0.220432236 2.182000e-03
#> 3030       b_cyl  0.223901657 2.167126e-03
#> 3031       b_cyl  0.227371079 2.154238e-03
#> 3032       b_cyl  0.230840500 2.143271e-03
#> 3033       b_cyl  0.234309922 2.134530e-03
#> 3034       b_cyl  0.237779343 2.127469e-03
#> 3035       b_cyl  0.241248764 2.121985e-03
#> 3036       b_cyl  0.244718186 2.117967e-03
#> 3037       b_cyl  0.248187607 2.115300e-03
#> 3038       b_cyl  0.251657029 2.113862e-03
#> 3039       b_cyl  0.255126450 2.113702e-03
#> 3040       b_cyl  0.258595871 2.114479e-03
#> 3041       b_cyl  0.262065293 2.116011e-03
#> 3042       b_cyl  0.265534714 2.118161e-03
#> 3043       b_cyl  0.269004136 2.120792e-03
#> 3044       b_cyl  0.272473557 2.123764e-03
#> 3045       b_cyl  0.275942978 2.126940e-03
#> 3046       b_cyl  0.279412400 2.130115e-03
#> 3047       b_cyl  0.282881821 2.133137e-03
#> 3048       b_cyl  0.286351243 2.135878e-03
#> 3049       b_cyl  0.289820664 2.138209e-03
#> 3050       b_cyl  0.293290085 2.140006e-03
#> 3051       b_cyl  0.296759507 2.141069e-03
#> 3052       b_cyl  0.300228928 2.141200e-03
#> 3053       b_cyl  0.303698350 2.140389e-03
#> 3054       b_cyl  0.307167771 2.138538e-03
#> 3055       b_cyl  0.310637192 2.135554e-03
#> 3056       b_cyl  0.314106614 2.131351e-03
#> 3057       b_cyl  0.317576035 2.125780e-03
#> 3058       b_cyl  0.321045457 2.118549e-03
#> 3059       b_cyl  0.324514878 2.109847e-03
#> 3060       b_cyl  0.327984299 2.099622e-03
#> 3061       b_cyl  0.331453721 2.087831e-03
#> 3062       b_cyl  0.334923142 2.074437e-03
#> 3063       b_cyl  0.338392564 2.059411e-03
#> 3064       b_cyl  0.341861985 2.042333e-03
#> 3065       b_cyl  0.345331406 2.023585e-03
#> 3066       b_cyl  0.348800828 2.003168e-03
#> 3067       b_cyl  0.352270249 1.981088e-03
#> 3068       b_cyl  0.355739671 1.957361e-03
#> 3069       b_cyl  0.359209092 1.932006e-03
#> 3070       b_cyl  0.362678513 1.904759e-03
#> 3071       b_cyl  0.366147935 1.875895e-03
#> 3072       b_cyl  0.369617356 1.845538e-03
# }
```
