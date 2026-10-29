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
[`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html)
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
[`?insight::find_parameters`](https://easystats.github.io/insight/reference/find_parameters.BGGM.html).

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
#> Chain 1: Gradient evaluation took 6e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.06 seconds.
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
#> Chain 1:  Elapsed Time: 0.017 seconds (Warm-up)
#> Chain 1:                0.014 seconds (Sampling)
#> Chain 1:                0.031 seconds (Total)
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
#> Chain 2:  Elapsed Time: 0.018 seconds (Warm-up)
#> Chain 2:                0.014 seconds (Sampling)
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
#> Chain 3:  Elapsed Time: 0.017 seconds (Warm-up)
#> Chain 3:                0.016 seconds (Sampling)
#> Chain 3:                0.033 seconds (Total)
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
#> Chain 4:  Elapsed Time: 0.018 seconds (Warm-up)
#> Chain 4:                0.014 seconds (Sampling)
#> Chain 4:                0.032 seconds (Total)
#> Chain 4: 
estimate_density(model)
#>        Parameter             x            y
#> 1    b_Intercept 33.7290129300 0.0009684682
#> 2    b_Intercept 33.7408577947 0.0009878243
#> 3    b_Intercept 33.7527026594 0.0010070817
#> 4    b_Intercept 33.7645475241 0.0010262529
#> 5    b_Intercept 33.7763923888 0.0010453453
#> 6    b_Intercept 33.7882372536 0.0010643956
#> 7    b_Intercept 33.8000821183 0.0010834352
#> 8    b_Intercept 33.8119269830 0.0011024912
#> 9    b_Intercept 33.8237718477 0.0011215929
#> 10   b_Intercept 33.8356167124 0.0011407817
#> 11   b_Intercept 33.8474615771 0.0011601329
#> 12   b_Intercept 33.8593064418 0.0011796702
#> 13   b_Intercept 33.8711513065 0.0011994377
#> 14   b_Intercept 33.8829961712 0.0012194814
#> 15   b_Intercept 33.8948410359 0.0012398499
#> 16   b_Intercept 33.9066859006 0.0012607099
#> 17   b_Intercept 33.9185307653 0.0012820648
#> 18   b_Intercept 33.9303756300 0.0013039542
#> 19   b_Intercept 33.9422204947 0.0013264379
#> 20   b_Intercept 33.9540653595 0.0013495774
#> 21   b_Intercept 33.9659102242 0.0013735395
#> 22   b_Intercept 33.9777550889 0.0013984617
#> 23   b_Intercept 33.9895999536 0.0014242889
#> 24   b_Intercept 34.0014448183 0.0014510891
#> 25   b_Intercept 34.0132896830 0.0014789303
#> 26   b_Intercept 34.0251345477 0.0015078901
#> 27   b_Intercept 34.0369794124 0.0015384032
#> 28   b_Intercept 34.0488242771 0.0015702202
#> 29   b_Intercept 34.0606691418 0.0016034087
#> 30   b_Intercept 34.0725140065 0.0016380355
#> 31   b_Intercept 34.0843588712 0.0016741666
#> 32   b_Intercept 34.0962037359 0.0017121711
#> 33   b_Intercept 34.1080486006 0.0017520354
#> 34   b_Intercept 34.1198934653 0.0017936407
#> 35   b_Intercept 34.1317383301 0.0018370436
#> 36   b_Intercept 34.1435831948 0.0018822981
#> 37   b_Intercept 34.1554280595 0.0019295803
#> 38   b_Intercept 34.1672729242 0.0019792929
#> 39   b_Intercept 34.1791177889 0.0020310362
#> 40   b_Intercept 34.1909626536 0.0020848475
#> 41   b_Intercept 34.2028075183 0.0021407612
#> 42   b_Intercept 34.2146523830 0.0021988079
#> 43   b_Intercept 34.2264972477 0.0022595356
#> 44   b_Intercept 34.2383421124 0.0023225807
#> 45   b_Intercept 34.2501869771 0.0023878330
#> 46   b_Intercept 34.2620318418 0.0024553005
#> 47   b_Intercept 34.2738767065 0.0025249872
#> 48   b_Intercept 34.2857215712 0.0025971534
#> 49   b_Intercept 34.2975664360 0.0026719133
#> 50   b_Intercept 34.3094113007 0.0027488606
#> 51   b_Intercept 34.3212561654 0.0028279745
#> 52   b_Intercept 34.3331010301 0.0029092301
#> 53   b_Intercept 34.3449458948 0.0029925986
#> 54   b_Intercept 34.3567907595 0.0030786334
#> 55   b_Intercept 34.3686356242 0.0031666740
#> 56   b_Intercept 34.3804804889 0.0032566704
#> 57   b_Intercept 34.3923253536 0.0033485714
#> 58   b_Intercept 34.4041702183 0.0034423226
#> 59   b_Intercept 34.4160150830 0.0035381587
#> 60   b_Intercept 34.4278599477 0.0036358849
#> 61   b_Intercept 34.4397048124 0.0037352190
#> 62   b_Intercept 34.4515496771 0.0038360907
#> 63   b_Intercept 34.4633945418 0.0039384278
#> 64   b_Intercept 34.4752394066 0.0040422254
#> 65   b_Intercept 34.4870842713 0.0041476064
#> 66   b_Intercept 34.4989291360 0.0042541607
#> 67   b_Intercept 34.5107740007 0.0043618097
#> 68   b_Intercept 34.5226188654 0.0044704749
#> 69   b_Intercept 34.5344637301 0.0045800779
#> 70   b_Intercept 34.5463085948 0.0046907069
#> 71   b_Intercept 34.5581534595 0.0048021105
#> 72   b_Intercept 34.5699983242 0.0049141612
#> 73   b_Intercept 34.5818431889 0.0050267875
#> 74   b_Intercept 34.5936880536 0.0051399200
#> 75   b_Intercept 34.6055329183 0.0052535290
#> 76   b_Intercept 34.6173777830 0.0053675430
#> 77   b_Intercept 34.6292226477 0.0054818277
#> 78   b_Intercept 34.6410675124 0.0055963317
#> 79   b_Intercept 34.6529123772 0.0057110072
#> 80   b_Intercept 34.6647572419 0.0058258102
#> 81   b_Intercept 34.6766021066 0.0059407103
#> 82   b_Intercept 34.6884469713 0.0060556442
#> 83   b_Intercept 34.7002918360 0.0061705894
#> 84   b_Intercept 34.7121367007 0.0062855286
#> 85   b_Intercept 34.7239815654 0.0064004493
#> 86   b_Intercept 34.7358264301 0.0065153400
#> 87   b_Intercept 34.7476712948 0.0066302052
#> 88   b_Intercept 34.7595161595 0.0067450601
#> 89   b_Intercept 34.7713610242 0.0068599212
#> 90   b_Intercept 34.7832058889 0.0069748094
#> 91   b_Intercept 34.7950507536 0.0070897552
#> 92   b_Intercept 34.8068956183 0.0072048287
#> 93   b_Intercept 34.8187404831 0.0073200591
#> 94   b_Intercept 34.8305853478 0.0074354928
#> 95   b_Intercept 34.8424302125 0.0075511797
#> 96   b_Intercept 34.8542750772 0.0076671728
#> 97   b_Intercept 34.8661199419 0.0077836322
#> 98   b_Intercept 34.8779648066 0.0079005962
#> 99   b_Intercept 34.8898096713 0.0080181027
#> 100  b_Intercept 34.9016545360 0.0081362194
#> 101  b_Intercept 34.9134994007 0.0082550151
#> 102  b_Intercept 34.9253442654 0.0083746466
#> 103  b_Intercept 34.9371891301 0.0084953037
#> 104  b_Intercept 34.9490339948 0.0086169110
#> 105  b_Intercept 34.9608788595 0.0087395388
#> 106  b_Intercept 34.9727237242 0.0088632563
#> 107  b_Intercept 34.9845685889 0.0089881314
#> 108  b_Intercept 34.9964134537 0.0091145989
#> 109  b_Intercept 35.0082583184 0.0092424267
#> 110  b_Intercept 35.0201031831 0.0093716492
#> 111  b_Intercept 35.0319480478 0.0095023206
#> 112  b_Intercept 35.0437929125 0.0096344919
#> 113  b_Intercept 35.0556377772 0.0097684627
#> 114  b_Intercept 35.0674826419 0.0099042685
#> 115  b_Intercept 35.0793275066 0.0100417271
#> 116  b_Intercept 35.0911723713 0.0101808662
#> 117  b_Intercept 35.1030172360 0.0103217093
#> 118  b_Intercept 35.1148621007 0.0104643398
#> 119  b_Intercept 35.1267069654 0.0106091562
#> 120  b_Intercept 35.1385518301 0.0107557172
#> 121  b_Intercept 35.1503966948 0.0109040197
#> 122  b_Intercept 35.1622415595 0.0110540561
#> 123  b_Intercept 35.1740864243 0.0112058151
#> 124  b_Intercept 35.1859312890 0.0113596335
#> 125  b_Intercept 35.1977761537 0.0115152553
#> 126  b_Intercept 35.2096210184 0.0116725109
#> 127  b_Intercept 35.2214658831 0.0118313677
#> 128  b_Intercept 35.2333107478 0.0119917905
#> 129  b_Intercept 35.2451556125 0.0121538767
#> 130  b_Intercept 35.2570004772 0.0123177218
#> 131  b_Intercept 35.2688453419 0.0124829750
#> 132  b_Intercept 35.2806902066 0.0126495918
#> 133  b_Intercept 35.2925350713 0.0128175273
#> 134  b_Intercept 35.3043799360 0.0129867367
#> 135  b_Intercept 35.3162248007 0.0131574832
#> 136  b_Intercept 35.3280696654 0.0133294149
#> 137  b_Intercept 35.3399145302 0.0135024607
#> 138  b_Intercept 35.3517593949 0.0136765847
#> 139  b_Intercept 35.3636042596 0.0138517537
#> 140  b_Intercept 35.3754491243 0.0140280819
#> 141  b_Intercept 35.3872939890 0.0142055309
#> 142  b_Intercept 35.3991388537 0.0143839397
#> 143  b_Intercept 35.4109837184 0.0145632992
#> 144  b_Intercept 35.4228285831 0.0147436059
#> 145  b_Intercept 35.4346734478 0.0149248885
#> 146  b_Intercept 35.4465183125 0.0151073919
#> 147  b_Intercept 35.4583631772 0.0152908899
#> 148  b_Intercept 35.4702080419 0.0154754140
#> 149  b_Intercept 35.4820529066 0.0156610019
#> 150  b_Intercept 35.4938977713 0.0158476979
#> 151  b_Intercept 35.5057426360 0.0160358097
#> 152  b_Intercept 35.5175875008 0.0162253048
#> 153  b_Intercept 35.5294323655 0.0164161532
#> 154  b_Intercept 35.5412772302 0.0166084375
#> 155  b_Intercept 35.5531220949 0.0168022462
#> 156  b_Intercept 35.5649669596 0.0169978232
#> 157  b_Intercept 35.5768118243 0.0171955540
#> 158  b_Intercept 35.5886566890 0.0173952125
#> 159  b_Intercept 35.6005015537 0.0175969195
#> 160  b_Intercept 35.6123464184 0.0178008000
#> 161  b_Intercept 35.6241912831 0.0180069827
#> 162  b_Intercept 35.6360361478 0.0182162933
#> 163  b_Intercept 35.6478810125 0.0184283822
#> 164  b_Intercept 35.6597258772 0.0186433046
#> 165  b_Intercept 35.6715707419 0.0188612050
#> 166  b_Intercept 35.6834156066 0.0190822294
#> 167  b_Intercept 35.6952604714 0.0193070119
#> 168  b_Intercept 35.7071053361 0.0195358434
#> 169  b_Intercept 35.7189502008 0.0197683589
#> 170  b_Intercept 35.7307950655 0.0200047017
#> 171  b_Intercept 35.7426399302 0.0202450130
#> 172  b_Intercept 35.7544847949 0.0204895175
#> 173  b_Intercept 35.7663296596 0.0207395004
#> 174  b_Intercept 35.7781745243 0.0209939621
#> 175  b_Intercept 35.7900193890 0.0212530236
#> 176  b_Intercept 35.8018642537 0.0215168016
#> 177  b_Intercept 35.8137091184 0.0217854079
#> 178  b_Intercept 35.8255539831 0.0220599399
#> 179  b_Intercept 35.8373988478 0.0223400734
#> 180  b_Intercept 35.8492437125 0.0226253996
#> 181  b_Intercept 35.8610885773 0.0229159959
#> 182  b_Intercept 35.8729334420 0.0232119336
#> 183  b_Intercept 35.8847783067 0.0235136901
#> 184  b_Intercept 35.8966231714 0.0238221321
#> 185  b_Intercept 35.9084680361 0.0241361162
#> 186  b_Intercept 35.9203129008 0.0244556748
#> 187  b_Intercept 35.9321577655 0.0247808341
#> 188  b_Intercept 35.9440026302 0.0251116143
#> 189  b_Intercept 35.9558474949 0.0254494362
#> 190  b_Intercept 35.9676923596 0.0257931420
#> 191  b_Intercept 35.9795372243 0.0261424749
#> 192  b_Intercept 35.9913820890 0.0264974211
#> 193  b_Intercept 36.0032269537 0.0268579616
#> 194  b_Intercept 36.0150718184 0.0272247934
#> 195  b_Intercept 36.0269166831 0.0275980461
#> 196  b_Intercept 36.0387615479 0.0279767725
#> 197  b_Intercept 36.0506064126 0.0283609307
#> 198  b_Intercept 36.0624512773 0.0287504759
#> 199  b_Intercept 36.0742961420 0.0291454208
#> 200  b_Intercept 36.0861410067 0.0295471203
#> 201  b_Intercept 36.0979858714 0.0299540134
#> 202  b_Intercept 36.1098307361 0.0303660458
#> 203  b_Intercept 36.1216756008 0.0307831628
#> 204  b_Intercept 36.1335204655 0.0312053096
#> 205  b_Intercept 36.1453653302 0.0316333388
#> 206  b_Intercept 36.1572101949 0.0320667797
#> 207  b_Intercept 36.1690550596 0.0325050459
#> 208  b_Intercept 36.1808999243 0.0329480874
#> 209  b_Intercept 36.1927447890 0.0333958557
#> 210  b_Intercept 36.2045896538 0.0338486117
#> 211  b_Intercept 36.2164345185 0.0343070304
#> 212  b_Intercept 36.2282793832 0.0347700123
#> 213  b_Intercept 36.2401242479 0.0352375219
#> 214  b_Intercept 36.2519691126 0.0357095258
#> 215  b_Intercept 36.2638139773 0.0361859932
#> 216  b_Intercept 36.2756588420 0.0366679552
#> 217  b_Intercept 36.2875037067 0.0371545427
#> 218  b_Intercept 36.2993485714 0.0376455045
#> 219  b_Intercept 36.3111934361 0.0381408237
#> 220  b_Intercept 36.3230383008 0.0386404855
#> 221  b_Intercept 36.3348831655 0.0391450032
#> 222  b_Intercept 36.3467280302 0.0396545761
#> 223  b_Intercept 36.3585728949 0.0401684538
#> 224  b_Intercept 36.3704177596 0.0406866301
#> 225  b_Intercept 36.3822626244 0.0412090990
#> 226  b_Intercept 36.3941074891 0.0417358667
#> 227  b_Intercept 36.4059523538 0.0422681584
#> 228  b_Intercept 36.4177972185 0.0428047201
#> 229  b_Intercept 36.4296420832 0.0433455437
#> 230  b_Intercept 36.4414869479 0.0438906195
#> 231  b_Intercept 36.4533318126 0.0444399358
#> 232  b_Intercept 36.4651766773 0.0449942230
#> 233  b_Intercept 36.4770215420 0.0455531944
#> 234  b_Intercept 36.4888664067 0.0461163332
#> 235  b_Intercept 36.5007112714 0.0466836107
#> 236  b_Intercept 36.5125561361 0.0472549939
#> 237  b_Intercept 36.5244010008 0.0478306784
#> 238  b_Intercept 36.5362458655 0.0484113066
#> 239  b_Intercept 36.5480907302 0.0489958643
#> 240  b_Intercept 36.5599355950 0.0495842893
#> 241  b_Intercept 36.5717804597 0.0501765141
#> 242  b_Intercept 36.5836253244 0.0507724652
#> 243  b_Intercept 36.5954701891 0.0513728737
#> 244  b_Intercept 36.6073150538 0.0519769801
#> 245  b_Intercept 36.6191599185 0.0525844589
#> 246  b_Intercept 36.6310047832 0.0531951993
#> 247  b_Intercept 36.6428496479 0.0538090841
#> 248  b_Intercept 36.6546945126 0.0544263081
#> 249  b_Intercept 36.6665393773 0.0550468456
#> 250  b_Intercept 36.6783842420 0.0556700074
#> 251  b_Intercept 36.6902291067 0.0562956393
#> 252  b_Intercept 36.7020739714 0.0569235818
#> 253  b_Intercept 36.7139188361 0.0575536698
#> 254  b_Intercept 36.7257637009 0.0581862065
#> 255  b_Intercept 36.7376085656 0.0588203977
#> 256  b_Intercept 36.7494534303 0.0594560443
#> 257  b_Intercept 36.7612982950 0.0600929551
#> 258  b_Intercept 36.7731431597 0.0607309356
#> 259  b_Intercept 36.7849880244 0.0613698796
#> 260  b_Intercept 36.7968328891 0.0620094305
#> 261  b_Intercept 36.8086777538 0.0626492821
#> 262  b_Intercept 36.8205226185 0.0632892312
#> 263  b_Intercept 36.8323674832 0.0639290749
#> 264  b_Intercept 36.8442123479 0.0645685812
#> 265  b_Intercept 36.8560572126 0.0652073367
#> 266  b_Intercept 36.8679020773 0.0658452271
#> 267  b_Intercept 36.8797469420 0.0664820632
#> 268  b_Intercept 36.8915918067 0.0671176608
#> 269  b_Intercept 36.9034366715 0.0677518402
#> 270  b_Intercept 36.9152815362 0.0683840078
#> 271  b_Intercept 36.9271264009 0.0690141748
#> 272  b_Intercept 36.9389712656 0.0696423097
#> 273  b_Intercept 36.9508161303 0.0702682747
#> 274  b_Intercept 36.9626609950 0.0708919407
#> 275  b_Intercept 36.9745058597 0.0715129174
#> 276  b_Intercept 36.9863507244 0.0721308306
#> 277  b_Intercept 36.9981955891 0.0727460620
#> 278  b_Intercept 37.0100404538 0.0733585449
#> 279  b_Intercept 37.0218853185 0.0739682245
#> 280  b_Intercept 37.0337301832 0.0745750573
#> 281  b_Intercept 37.0455750479 0.0751782075
#> 282  b_Intercept 37.0574199126 0.0757784350
#> 283  b_Intercept 37.0692647773 0.0763758017
#> 284  b_Intercept 37.0811096421 0.0769703410
#> 285  b_Intercept 37.0929545068 0.0775620988
#> 286  b_Intercept 37.1047993715 0.0781507208
#> 287  b_Intercept 37.1166442362 0.0787364162
#> 288  b_Intercept 37.1284891009 0.0793196570
#> 289  b_Intercept 37.1403339656 0.0799005668
#> 290  b_Intercept 37.1521788303 0.0804792805
#> 291  b_Intercept 37.1640236950 0.0810558718
#> 292  b_Intercept 37.1758685597 0.0816302060
#> 293  b_Intercept 37.1877134244 0.0822029910
#> 294  b_Intercept 37.1995582891 0.0827744288
#> 295  b_Intercept 37.2114031538 0.0833447310
#> 296  b_Intercept 37.2232480185 0.0839141183
#> 297  b_Intercept 37.2350928832 0.0844827640
#> 298  b_Intercept 37.2469377480 0.0850511280
#> 299  b_Intercept 37.2587826127 0.0856195171
#> 300  b_Intercept 37.2706274774 0.0861881962
#> 301  b_Intercept 37.2824723421 0.0867574350
#> 302  b_Intercept 37.2943172068 0.0873276307
#> 303  b_Intercept 37.3061620715 0.0878993498
#> 304  b_Intercept 37.3180069362 0.0884727079
#> 305  b_Intercept 37.3298518009 0.0890479942
#> 306  b_Intercept 37.3416966656 0.0896254986
#> 307  b_Intercept 37.3535415303 0.0902055121
#> 308  b_Intercept 37.3653863950 0.0907892241
#> 309  b_Intercept 37.3772312597 0.0913763311
#> 310  b_Intercept 37.3890761244 0.0919670514
#> 311  b_Intercept 37.4009209891 0.0925616667
#> 312  b_Intercept 37.4127658538 0.0931604560
#> 313  b_Intercept 37.4246107186 0.0937644347
#> 314  b_Intercept 37.4364555833 0.0943739913
#> 315  b_Intercept 37.4483004480 0.0949887416
#> 316  b_Intercept 37.4601453127 0.0956089364
#> 317  b_Intercept 37.4719901774 0.0962348202
#> 318  b_Intercept 37.4838350421 0.0968668342
#> 319  b_Intercept 37.4956799068 0.0975067435
#> 320  b_Intercept 37.5075247715 0.0981532076
#> 321  b_Intercept 37.5193696362 0.0988064302
#> 322  b_Intercept 37.5312145009 0.0994666079
#> 323  b_Intercept 37.5430593656 0.1001339296
#> 324  b_Intercept 37.5549042303 0.1008101497
#> 325  b_Intercept 37.5667490950 0.1014946368
#> 326  b_Intercept 37.5785939597 0.1021869003
#> 327  b_Intercept 37.5904388245 0.1028870821
#> 328  b_Intercept 37.6022836892 0.1035953163
#> 329  b_Intercept 37.6141285539 0.1043124610
#> 330  b_Intercept 37.6259734186 0.1050396764
#> 331  b_Intercept 37.6378182833 0.1057753696
#> 332  b_Intercept 37.6496631480 0.1065196279
#> 333  b_Intercept 37.6615080127 0.1072725312
#> 334  b_Intercept 37.6733528774 0.1080341520
#> 335  b_Intercept 37.6851977421 0.1088068799
#> 336  b_Intercept 37.6970426068 0.1095887567
#> 337  b_Intercept 37.7088874715 0.1103795408
#> 338  b_Intercept 37.7207323362 0.1111792613
#> 339  b_Intercept 37.7325772009 0.1119879407
#> 340  b_Intercept 37.7444220656 0.1128068778
#> 341  b_Intercept 37.7562669303 0.1136361454
#> 342  b_Intercept 37.7681117951 0.1144743797
#> 343  b_Intercept 37.7799566598 0.1153215630
#> 344  b_Intercept 37.7918015245 0.1161776710
#> 345  b_Intercept 37.8036463892 0.1170428833
#> 346  b_Intercept 37.8154912539 0.1179193134
#> 347  b_Intercept 37.8273361186 0.1188045053
#> 348  b_Intercept 37.8391809833 0.1196983964
#> 349  b_Intercept 37.8510258480 0.1206009180
#> 350  b_Intercept 37.8628707127 0.1215119952
#> 351  b_Intercept 37.8747155774 0.1224331945
#> 352  b_Intercept 37.8865604421 0.1233635001
#> 353  b_Intercept 37.8984053068 0.1243019990
#> 354  b_Intercept 37.9102501715 0.1252485777
#> 355  b_Intercept 37.9220950362 0.1262031162
#> 356  b_Intercept 37.9339399009 0.1271660908
#> 357  b_Intercept 37.9457847657 0.1281383223
#> 358  b_Intercept 37.9576296304 0.1291179802
#> 359  b_Intercept 37.9694744951 0.1301049045
#> 360  b_Intercept 37.9813193598 0.1310989287
#> 361  b_Intercept 37.9931642245 0.1320998798
#> 362  b_Intercept 38.0050090892 0.1331092003
#> 363  b_Intercept 38.0168539539 0.1341251774
#> 364  b_Intercept 38.0286988186 0.1351473358
#> 365  b_Intercept 38.0405436833 0.1361754611
#> 366  b_Intercept 38.0523885480 0.1372093323
#> 367  b_Intercept 38.0642334127 0.1382494110
#> 368  b_Intercept 38.0760782774 0.1392954376
#> 369  b_Intercept 38.0879231421 0.1403462856
#> 370  b_Intercept 38.0997680068 0.1414016939
#> 371  b_Intercept 38.1116128716 0.1424613950
#> 372  b_Intercept 38.1234577363 0.1435251703
#> 373  b_Intercept 38.1353026010 0.1445935618
#> 374  b_Intercept 38.1471474657 0.1456651545
#> 375  b_Intercept 38.1589923304 0.1467396453
#> 376  b_Intercept 38.1708371951 0.1478167262
#> 377  b_Intercept 38.1826820598 0.1488960841
#> 378  b_Intercept 38.1945269245 0.1499776681
#> 379  b_Intercept 38.2063717892 0.1510608065
#> 380  b_Intercept 38.2182166539 0.1521449701
#> 381  b_Intercept 38.2300615186 0.1532298214
#> 382  b_Intercept 38.2419063833 0.1543150196
#> 383  b_Intercept 38.2537512480 0.1554001844
#> 384  b_Intercept 38.2655961127 0.1564847096
#> 385  b_Intercept 38.2774409774 0.1575682516
#> 386  b_Intercept 38.2892858422 0.1586504594
#> 387  b_Intercept 38.3011307069 0.1597309816
#> 388  b_Intercept 38.3129755716 0.1608094672
#> 389  b_Intercept 38.3248204363 0.1618848301
#> 390  b_Intercept 38.3366653010 0.1629570462
#> 391  b_Intercept 38.3485101657 0.1640258937
#> 392  b_Intercept 38.3603550304 0.1650910316
#> 393  b_Intercept 38.3721998951 0.1661521221
#> 394  b_Intercept 38.3840447598 0.1672082141
#> 395  b_Intercept 38.3958896245 0.1682585827
#> 396  b_Intercept 38.4077344892 0.1693036580
#> 397  b_Intercept 38.4195793539 0.1703431311
#> 398  b_Intercept 38.4314242186 0.1713766993
#> 399  b_Intercept 38.4432690833 0.1724040226
#> 400  b_Intercept 38.4551139480 0.1734228441
#> 401  b_Intercept 38.4669588128 0.1744346784
#> 402  b_Intercept 38.4788036775 0.1754392683
#> 403  b_Intercept 38.4906485422 0.1764363655
#> 404  b_Intercept 38.5024934069 0.1774257300
#> 405  b_Intercept 38.5143382716 0.1784056253
#> 406  b_Intercept 38.5261831363 0.1793762694
#> 407  b_Intercept 38.5380280010 0.1803383733
#> 408  b_Intercept 38.5498728657 0.1812917558
#> 409  b_Intercept 38.5617177304 0.1822362457
#> 410  b_Intercept 38.5735625951 0.1831711082
#> 411  b_Intercept 38.5854074598 0.1840945566
#> 412  b_Intercept 38.5972523245 0.1850085728
#> 413  b_Intercept 38.6090971892 0.1859130482
#> 414  b_Intercept 38.6209420539 0.1868078847
#> 415  b_Intercept 38.6327869187 0.1876929939
#> 416  b_Intercept 38.6446317834 0.1885659568
#> 417  b_Intercept 38.6564766481 0.1894284498
#> 418  b_Intercept 38.6683215128 0.1902809968
#> 419  b_Intercept 38.6801663775 0.1911235687
#> 420  b_Intercept 38.6920112422 0.1919561453
#> 421  b_Intercept 38.7038561069 0.1927775435
#> 422  b_Intercept 38.7157009716 0.1935871808
#> 423  b_Intercept 38.7275458363 0.1943868450
#> 424  b_Intercept 38.7393907010 0.1951765662
#> 425  b_Intercept 38.7512355657 0.1959563821
#> 426  b_Intercept 38.7630804304 0.1967263374
#> 427  b_Intercept 38.7749252951 0.1974836584
#> 428  b_Intercept 38.7867701598 0.1982312738
#> 429  b_Intercept 38.7986150245 0.1989692791
#> 430  b_Intercept 38.8104598893 0.1996977561
#> 431  b_Intercept 38.8223047540 0.2004167917
#> 432  b_Intercept 38.8341496187 0.2011248937
#> 433  b_Intercept 38.8459944834 0.2018227018
#> 434  b_Intercept 38.8578393481 0.2025114539
#> 435  b_Intercept 38.8696842128 0.2031912618
#> 436  b_Intercept 38.8815290775 0.2038622404
#> 437  b_Intercept 38.8933739422 0.2045240522
#> 438  b_Intercept 38.9052188069 0.2051752853
#> 439  b_Intercept 38.9170636716 0.2058181571
#> 440  b_Intercept 38.9289085363 0.2064527953
#> 441  b_Intercept 38.9407534010 0.2070793289
#> 442  b_Intercept 38.9525982657 0.2076978873
#> 443  b_Intercept 38.9644431304 0.2083068658
#> 444  b_Intercept 38.9762879951 0.2089077251
#> 445  b_Intercept 38.9881328599 0.2095011082
#> 446  b_Intercept 38.9999777246 0.2100871434
#> 447  b_Intercept 39.0118225893 0.2106659578
#> 448  b_Intercept 39.0236674540 0.2112369269
#> 449  b_Intercept 39.0355123187 0.2117997142
#> 450  b_Intercept 39.0473571834 0.2123557443
#> 451  b_Intercept 39.0592020481 0.2129051289
#> 452  b_Intercept 39.0710469128 0.2134479761
#> 453  b_Intercept 39.0828917775 0.2139843901
#> 454  b_Intercept 39.0947366422 0.2145127228
#> 455  b_Intercept 39.1065815069 0.2150348159
#> 456  b_Intercept 39.1184263716 0.2155508128
#> 457  b_Intercept 39.1302712363 0.2160607839
#> 458  b_Intercept 39.1421161010 0.2165647929
#> 459  b_Intercept 39.1539609658 0.2170619390
#> 460  b_Intercept 39.1658058305 0.2175524882
#> 461  b_Intercept 39.1776506952 0.2180372278
#> 462  b_Intercept 39.1894955599 0.2185161710
#> 463  b_Intercept 39.2013404246 0.2189893217
#> 464  b_Intercept 39.2131852893 0.2194564128
#> 465  b_Intercept 39.2250301540 0.2199162176
#> 466  b_Intercept 39.2368750187 0.2203701300
#> 467  b_Intercept 39.2487198834 0.2208180895
#> 468  b_Intercept 39.2605647481 0.2212600244
#> 469  b_Intercept 39.2724096128 0.2216958515
#> 470  b_Intercept 39.2842544775 0.2221240702
#> 471  b_Intercept 39.2960993422 0.2225454009
#> 472  b_Intercept 39.3079442069 0.2229601646
#> 473  b_Intercept 39.3197890716 0.2233682069
#> 474  b_Intercept 39.3316339364 0.2237693620
#> 475  b_Intercept 39.3434788011 0.2241627148
#> 476  b_Intercept 39.3553236658 0.2245472673
#> 477  b_Intercept 39.3671685305 0.2249241711
#> 478  b_Intercept 39.3790133952 0.2252931960
#> 479  b_Intercept 39.3908582599 0.2256541025
#> 480  b_Intercept 39.4027031246 0.2260066426
#> 481  b_Intercept 39.4145479893 0.2263480566
#> 482  b_Intercept 39.4263928540 0.2266801804
#> 483  b_Intercept 39.4382377187 0.2270028972
#> 484  b_Intercept 39.4500825834 0.2273159195
#> 485  b_Intercept 39.4619274481 0.2276189555
#> 486  b_Intercept 39.4737723128 0.2279100367
#> 487  b_Intercept 39.4856171775 0.2281888592
#> 488  b_Intercept 39.4974620423 0.2284565548
#> 489  b_Intercept 39.5093069070 0.2287128266
#> 490  b_Intercept 39.5211517717 0.2289573806
#> 491  b_Intercept 39.5329966364 0.2291894756
#> 492  b_Intercept 39.5448415011 0.2294059490
#> 493  b_Intercept 39.5566863658 0.2296096277
#> 494  b_Intercept 39.5685312305 0.2298002528
#> 495  b_Intercept 39.5803760952 0.2299775747
#> 496  b_Intercept 39.5922209599 0.2301413536
#> 497  b_Intercept 39.6040658246 0.2302883852
#> 498  b_Intercept 39.6159106893 0.2304201263
#> 499  b_Intercept 39.6277555540 0.2305375478
#> 500  b_Intercept 39.6396004187 0.2306404882
#> 501  b_Intercept 39.6514452834 0.2307288011
#> 502  b_Intercept 39.6632901481 0.2308009911
#> 503  b_Intercept 39.6751350129 0.2308552560
#> 504  b_Intercept 39.6869798776 0.2308945196
#> 505  b_Intercept 39.6988247423 0.2309187381
#> 506  b_Intercept 39.7106696070 0.2309278855
#> 507  b_Intercept 39.7225144717 0.2309219535
#> 508  b_Intercept 39.7343593364 0.2308969682
#> 509  b_Intercept 39.7462042011 0.2308565908
#> 510  b_Intercept 39.7580490658 0.2308013193
#> 511  b_Intercept 39.7698939305 0.2307312522
#> 512  b_Intercept 39.7817387952 0.2306465044
#> 513  b_Intercept 39.7935836599 0.2305451096
#> 514  b_Intercept 39.8054285246 0.2304273305
#> 515  b_Intercept 39.8172733893 0.2302954885
#> 516  b_Intercept 39.8291182540 0.2301497858
#> 517  b_Intercept 39.8409631187 0.2299904359
#> 518  b_Intercept 39.8528079835 0.2298173021
#> 519  b_Intercept 39.8646528482 0.2296276200
#> 520  b_Intercept 39.8764977129 0.2294252159
#> 521  b_Intercept 39.8883425776 0.2292103534
#> 522  b_Intercept 39.9001874423 0.2289833000
#> 523  b_Intercept 39.9120323070 0.2287443265
#> 524  b_Intercept 39.9238771717 0.2284914473
#> 525  b_Intercept 39.9357220364 0.2282263648
#> 526  b_Intercept 39.9475669011 0.2279504076
#> 527  b_Intercept 39.9594117658 0.2276638416
#> 528  b_Intercept 39.9712566305 0.2273669281
#> 529  b_Intercept 39.9831014952 0.2270591164
#> 530  b_Intercept 39.9949463599 0.2267395359
#> 531  b_Intercept 40.0067912246 0.2264105285
#> 532  b_Intercept 40.0186360894 0.2260723052
#> 533  b_Intercept 40.0304809541 0.2257250654
#> 534  b_Intercept 40.0423258188 0.2253689967
#> 535  b_Intercept 40.0541706835 0.2250021130
#> 536  b_Intercept 40.0660155482 0.2246265271
#> 537  b_Intercept 40.0778604129 0.2242426680
#> 538  b_Intercept 40.0897052776 0.2238506383
#> 539  b_Intercept 40.1015501423 0.2234505257
#> 540  b_Intercept 40.1133950070 0.2230413047
#> 541  b_Intercept 40.1252398717 0.2226229059
#> 542  b_Intercept 40.1370847364 0.2221965834
#> 543  b_Intercept 40.1489296011 0.2217623333
#> 544  b_Intercept 40.1607744658 0.2213201379
#> 545  b_Intercept 40.1726193305 0.2208698121
#> 546  b_Intercept 40.1844641952 0.2204092202
#> 547  b_Intercept 40.1963090600 0.2199404708
#> 548  b_Intercept 40.2081539247 0.2194634725
#> 549  b_Intercept 40.2199987894 0.2189781250
#> 550  b_Intercept 40.2318436541 0.2184843196
#> 551  b_Intercept 40.2436885188 0.2179802376
#> 552  b_Intercept 40.2555333835 0.2174664828
#> 553  b_Intercept 40.2673782482 0.2169437810
#> 554  b_Intercept 40.2792231129 0.2164119946
#> 555  b_Intercept 40.2910679776 0.2158709857
#> 556  b_Intercept 40.3029128423 0.2153199047
#> 557  b_Intercept 40.3147577070 0.2147571621
#> 558  b_Intercept 40.3266025717 0.2141846886
#> 559  b_Intercept 40.3384474364 0.2136023667
#> 560  b_Intercept 40.3502923011 0.2130100862
#> 561  b_Intercept 40.3621371658 0.2124077454
#> 562  b_Intercept 40.3739820306 0.2117926951
#> 563  b_Intercept 40.3858268953 0.2111669124
#> 564  b_Intercept 40.3976717600 0.2105308094
#> 565  b_Intercept 40.4095166247 0.2098843559
#> 566  b_Intercept 40.4213614894 0.2092275367
#> 567  b_Intercept 40.4332063541 0.2085590135
#> 568  b_Intercept 40.4450512188 0.2078784830
#> 569  b_Intercept 40.4568960835 0.2071877105
#> 570  b_Intercept 40.4687409482 0.2064867796
#> 571  b_Intercept 40.4805858129 0.2057757911
#> 572  b_Intercept 40.4924306776 0.2050547604
#> 573  b_Intercept 40.5042755423 0.2043212467
#> 574  b_Intercept 40.5161204070 0.2035782508
#> 575  b_Intercept 40.5279652717 0.2028259760
#> 576  b_Intercept 40.5398101365 0.2020646410
#> 577  b_Intercept 40.5516550012 0.2012944798
#> 578  b_Intercept 40.5634998659 0.2005142404
#> 579  b_Intercept 40.5753447306 0.1997250069
#> 580  b_Intercept 40.5871895953 0.1989280038
#> 581  b_Intercept 40.5990344600 0.1981235413
#> 582  b_Intercept 40.6108793247 0.1973119389
#> 583  b_Intercept 40.6227241894 0.1964931104
#> 584  b_Intercept 40.6345690541 0.1956665501
#> 585  b_Intercept 40.6464139188 0.1948341544
#> 586  b_Intercept 40.6582587835 0.1939962769
#> 587  b_Intercept 40.6701036482 0.1931532726
#> 588  b_Intercept 40.6819485129 0.1923054971
#> 589  b_Intercept 40.6937933776 0.1914523923
#> 590  b_Intercept 40.7056382423 0.1905952814
#> 591  b_Intercept 40.7174831071 0.1897347334
#> 592  b_Intercept 40.7293279718 0.1888710811
#> 593  b_Intercept 40.7411728365 0.1880046497
#> 594  b_Intercept 40.7530177012 0.1871355155
#> 595  b_Intercept 40.7648625659 0.1862640478
#> 596  b_Intercept 40.7767074306 0.1853909351
#> 597  b_Intercept 40.7885522953 0.1845164368
#> 598  b_Intercept 40.8003971600 0.1836407984
#> 599  b_Intercept 40.8122420247 0.1827642507
#> 600  b_Intercept 40.8240868894 0.1818869080
#> 601  b_Intercept 40.8359317541 0.1810092036
#> 602  b_Intercept 40.8477766188 0.1801312896
#> 603  b_Intercept 40.8596214835 0.1792533020
#> 604  b_Intercept 40.8714663482 0.1783753600
#> 605  b_Intercept 40.8833112129 0.1774976123
#> 606  b_Intercept 40.8951560777 0.1766201559
#> 607  b_Intercept 40.9070009424 0.1757430068
#> 608  b_Intercept 40.9188458071 0.1748661844
#> 609  b_Intercept 40.9306906718 0.1739896930
#> 610  b_Intercept 40.9425355365 0.1731135368
#> 611  b_Intercept 40.9543804012 0.1722377071
#> 612  b_Intercept 40.9662252659 0.1713620609
#> 613  b_Intercept 40.9780701306 0.1704865187
#> 614  b_Intercept 40.9899149953 0.1696109893
#> 615  b_Intercept 41.0017598600 0.1687353711
#> 616  b_Intercept 41.0136047247 0.1678594516
#> 617  b_Intercept 41.0254495894 0.1669830740
#> 618  b_Intercept 41.0372944541 0.1661061086
#> 619  b_Intercept 41.0491393188 0.1652284047
#> 620  b_Intercept 41.0609841836 0.1643498066
#> 621  b_Intercept 41.0728290483 0.1634700061
#> 622  b_Intercept 41.0846739130 0.1625886517
#> 623  b_Intercept 41.0965187777 0.1617057703
#> 624  b_Intercept 41.1083636424 0.1608211936
#> 625  b_Intercept 41.1202085071 0.1599347542
#> 626  b_Intercept 41.1320533718 0.1590462861
#> 627  b_Intercept 41.1438982365 0.1581549144
#> 628  b_Intercept 41.1557431012 0.1572610505
#> 629  b_Intercept 41.1675879659 0.1563645624
#> 630  b_Intercept 41.1794328306 0.1554653091
#> 631  b_Intercept 41.1912776953 0.1545631557
#> 632  b_Intercept 41.2031225600 0.1536574341
#> 633  b_Intercept 41.2149674247 0.1527480937
#> 634  b_Intercept 41.2268122894 0.1518354227
#> 635  b_Intercept 41.2386571542 0.1509193341
#> 636  b_Intercept 41.2505020189 0.1499997502
#> 637  b_Intercept 41.2623468836 0.1490764250
#> 638  b_Intercept 41.2741917483 0.1481485681
#> 639  b_Intercept 41.2860366130 0.1472170251
#> 640  b_Intercept 41.2978814777 0.1462817775
#> 641  b_Intercept 41.3097263424 0.1453428170
#> 642  b_Intercept 41.3215712071 0.1444001451
#> 643  b_Intercept 41.3334160718 0.1434529614
#> 644  b_Intercept 41.3452609365 0.1425018801
#> 645  b_Intercept 41.3571058012 0.1415472133
#> 646  b_Intercept 41.3689506659 0.1405890222
#> 647  b_Intercept 41.3807955306 0.1396273772
#> 648  b_Intercept 41.3926403953 0.1386620230
#> 649  b_Intercept 41.4044852601 0.1376928339
#> 650  b_Intercept 41.4163301248 0.1367205613
#> 651  b_Intercept 41.4281749895 0.1357453268
#> 652  b_Intercept 41.4400198542 0.1347672592
#> 653  b_Intercept 41.4518647189 0.1337864946
#> 654  b_Intercept 41.4637095836 0.1328025440
#> 655  b_Intercept 41.4755544483 0.1318162895
#> 656  b_Intercept 41.4873993130 0.1308279362
#> 657  b_Intercept 41.4992441777 0.1298376556
#> 658  b_Intercept 41.5110890424 0.1288456245
#> 659  b_Intercept 41.5229339071 0.1278518233
#> 660  b_Intercept 41.5347787718 0.1268565936
#> 661  b_Intercept 41.5466236365 0.1258603362
#> 662  b_Intercept 41.5584685012 0.1248632490
#> 663  b_Intercept 41.5703133659 0.1238655324
#> 664  b_Intercept 41.5821582307 0.1228673842
#> 665  b_Intercept 41.5940030954 0.1218690806
#> 666  b_Intercept 41.6058479601 0.1208709353
#> 667  b_Intercept 41.6176928248 0.1198731570
#> 668  b_Intercept 41.6295376895 0.1188759549
#> 669  b_Intercept 41.6413825542 0.1178795384
#> 670  b_Intercept 41.6532274189 0.1168844075
#> 671  b_Intercept 41.6650722836 0.1158907373
#> 672  b_Intercept 41.6769171483 0.1148986510
#> 673  b_Intercept 41.6887620130 0.1139083546
#> 674  b_Intercept 41.7006068777 0.1129200525
#> 675  b_Intercept 41.7124517424 0.1119341874
#> 676  b_Intercept 41.7242966071 0.1109513273
#> 677  b_Intercept 41.7361414718 0.1099712274
#> 678  b_Intercept 41.7479863365 0.1089940814
#> 679  b_Intercept 41.7598312013 0.1080200809
#> 680  b_Intercept 41.7716760660 0.1070494153
#> 681  b_Intercept 41.7835209307 0.1060833097
#> 682  b_Intercept 41.7953657954 0.1051211428
#> 683  b_Intercept 41.8072106601 0.1041630055
#> 684  b_Intercept 41.8190555248 0.1032090712
#> 685  b_Intercept 41.8309003895 0.1022595106
#> 686  b_Intercept 41.8427452542 0.1013152154
#> 687  b_Intercept 41.8545901189 0.1003764118
#> 688  b_Intercept 41.8664349836 0.0994426011
#> 689  b_Intercept 41.8782798483 0.0985139356
#> 690  b_Intercept 41.8901247130 0.0975905645
#> 691  b_Intercept 41.9019695777 0.0966728119
#> 692  b_Intercept 41.9138144424 0.0957621843
#> 693  b_Intercept 41.9256593072 0.0948573870
#> 694  b_Intercept 41.9375041719 0.0939585494
#> 695  b_Intercept 41.9493490366 0.0930657976
#> 696  b_Intercept 41.9611939013 0.0921792543
#> 697  b_Intercept 41.9730387660 0.0913003818
#> 698  b_Intercept 41.9848836307 0.0904285995
#> 699  b_Intercept 41.9967284954 0.0895634550
#> 700  b_Intercept 42.0085733601 0.0887050502
#> 701  b_Intercept 42.0204182248 0.0878534830
#> 702  b_Intercept 42.0322630895 0.0870094614
#> 703  b_Intercept 42.0441079542 0.0861739681
#> 704  b_Intercept 42.0559528189 0.0853456465
#> 705  b_Intercept 42.0677976836 0.0845245724
#> 706  b_Intercept 42.0796425483 0.0837108174
#> 707  b_Intercept 42.0914874130 0.0829044496
#> 708  b_Intercept 42.1033322778 0.0821075056
#> 709  b_Intercept 42.1151771425 0.0813183554
#> 710  b_Intercept 42.1270220072 0.0805368050
#> 711  b_Intercept 42.1388668719 0.0797628985
#> 712  b_Intercept 42.1507117366 0.0789966756
#> 713  b_Intercept 42.1625566013 0.0782392756
#> 714  b_Intercept 42.1744014660 0.0774908156
#> 715  b_Intercept 42.1862463307 0.0767501443
#> 716  b_Intercept 42.1980911954 0.0760172764
#> 717  b_Intercept 42.2099360601 0.0752922227
#> 718  b_Intercept 42.2217809248 0.0745751692
#> 719  b_Intercept 42.2336257895 0.0738680521
#> 720  b_Intercept 42.2454706542 0.0731687444
#> 721  b_Intercept 42.2573155189 0.0724772319
#> 722  b_Intercept 42.2691603836 0.0717934967
#> 723  b_Intercept 42.2810052484 0.0711175166
#> 724  b_Intercept 42.2928501131 0.0704507801
#> 725  b_Intercept 42.3046949778 0.0697924531
#> 726  b_Intercept 42.3165398425 0.0691417517
#> 727  b_Intercept 42.3283847072 0.0684986312
#> 728  b_Intercept 42.3402295719 0.0678630437
#> 729  b_Intercept 42.3520744366 0.0672355162
#> 730  b_Intercept 42.3639193013 0.0666169748
#> 731  b_Intercept 42.3757641660 0.0660057454
#> 732  b_Intercept 42.3876090307 0.0654017610
#> 733  b_Intercept 42.3994538954 0.0648049523
#> 734  b_Intercept 42.4112987601 0.0642152468
#> 735  b_Intercept 42.4231436248 0.0636343103
#> 736  b_Intercept 42.4349884895 0.0630605426
#> 737  b_Intercept 42.4468333543 0.0624935737
#> 738  b_Intercept 42.4586782190 0.0619333184
#> 739  b_Intercept 42.4705230837 0.0613796898
#> 740  b_Intercept 42.4823679484 0.0608334526
#> 741  b_Intercept 42.4942128131 0.0602946299
#> 742  b_Intercept 42.5060576778 0.0597620838
#> 743  b_Intercept 42.5179025425 0.0592357196
#> 744  b_Intercept 42.5297474072 0.0587154415
#> 745  b_Intercept 42.5415922719 0.0582012371
#> 746  b_Intercept 42.5534371366 0.0576945152
#> 747  b_Intercept 42.5652820013 0.0571935050
#> 748  b_Intercept 42.5771268660 0.0566981072
#> 749  b_Intercept 42.5889717307 0.0562082223
#> 750  b_Intercept 42.6008165954 0.0557237506
#> 751  b_Intercept 42.6126614601 0.0552455634
#> 752  b_Intercept 42.6245063249 0.0547730598
#> 753  b_Intercept 42.6363511896 0.0543055857
#> 754  b_Intercept 42.6481960543 0.0538430407
#> 755  b_Intercept 42.6600409190 0.0533853247
#> 756  b_Intercept 42.6718857837 0.0529326564
#> 757  b_Intercept 42.6837306484 0.0524855906
#> 758  b_Intercept 42.6955755131 0.0520429704
#> 759  b_Intercept 42.7074203778 0.0516046962
#> 760  b_Intercept 42.7192652425 0.0511706682
#> 761  b_Intercept 42.7311101072 0.0507407865
#> 762  b_Intercept 42.7429549719 0.0503158967
#> 763  b_Intercept 42.7547998366 0.0498950696
#> 764  b_Intercept 42.7666447013 0.0494780071
#> 765  b_Intercept 42.7784895660 0.0490646093
#> 766  b_Intercept 42.7903344307 0.0486547765
#> 767  b_Intercept 42.8021792955 0.0482488220
#> 768  b_Intercept 42.8140241602 0.0478467310
#> 769  b_Intercept 42.8258690249 0.0474478219
#> 770  b_Intercept 42.8377138896 0.0470519945
#> 771  b_Intercept 42.8495587543 0.0466591486
#> 772  b_Intercept 42.8614036190 0.0462691997
#> 773  b_Intercept 42.8732484837 0.0458827774
#> 774  b_Intercept 42.8850933484 0.0454989523
#> 775  b_Intercept 42.8969382131 0.0451176244
#> 776  b_Intercept 42.9087830778 0.0447386938
#> 777  b_Intercept 42.9206279425 0.0443620610
#> 778  b_Intercept 42.9324728072 0.0439879930
#> 779  b_Intercept 42.9443176719 0.0436161891
#> 780  b_Intercept 42.9561625366 0.0432463055
#> 781  b_Intercept 42.9680074014 0.0428782456
#> 782  b_Intercept 42.9798522661 0.0425119132
#> 783  b_Intercept 42.9916971308 0.0421473046
#> 784  b_Intercept 43.0035419955 0.0417845354
#> 785  b_Intercept 43.0153868602 0.0414231375
#> 786  b_Intercept 43.0272317249 0.0410630218
#> 787  b_Intercept 43.0390765896 0.0407041010
#> 788  b_Intercept 43.0509214543 0.0403462893
#> 789  b_Intercept 43.0627663190 0.0399897112
#> 790  b_Intercept 43.0746111837 0.0396340701
#> 791  b_Intercept 43.0864560484 0.0392792328
#> 792  b_Intercept 43.0983009131 0.0389251260
#> 793  b_Intercept 43.1101457778 0.0385716789
#> 794  b_Intercept 43.1219906425 0.0382188809
#> 795  b_Intercept 43.1338355072 0.0378666625
#> 796  b_Intercept 43.1456803720 0.0375148643
#> 797  b_Intercept 43.1575252367 0.0371634326
#> 798  b_Intercept 43.1693701014 0.0368123170
#> 799  b_Intercept 43.1812149661 0.0364614703
#> 800  b_Intercept 43.1930598308 0.0361108944
#> 801  b_Intercept 43.2049046955 0.0357604778
#> 802  b_Intercept 43.2167495602 0.0354101895
#> 803  b_Intercept 43.2285944249 0.0350600026
#> 804  b_Intercept 43.2404392896 0.0347098937
#> 805  b_Intercept 43.2522841543 0.0343598491
#> 806  b_Intercept 43.2641290190 0.0340098459
#> 807  b_Intercept 43.2759738837 0.0336598714
#> 808  b_Intercept 43.2878187484 0.0333099232
#> 809  b_Intercept 43.2996636131 0.0329600019
#> 810  b_Intercept 43.3115084779 0.0326101142
#> 811  b_Intercept 43.3233533426 0.0322602833
#> 812  b_Intercept 43.3351982073 0.0319105189
#> 813  b_Intercept 43.3470430720 0.0315608410
#> 814  b_Intercept 43.3588879367 0.0312112726
#> 815  b_Intercept 43.3707328014 0.0308618394
#> 816  b_Intercept 43.3825776661 0.0305126212
#> 817  b_Intercept 43.3944225308 0.0301636419
#> 818  b_Intercept 43.4062673955 0.0298149295
#> 819  b_Intercept 43.4181122602 0.0294665253
#> 820  b_Intercept 43.4299571249 0.0291184736
#> 821  b_Intercept 43.4418019896 0.0287708718
#> 822  b_Intercept 43.4536468543 0.0284238363
#> 823  b_Intercept 43.4654917190 0.0280773462
#> 824  b_Intercept 43.4773365837 0.0277314578
#> 825  b_Intercept 43.4891814485 0.0273862295
#> 826  b_Intercept 43.5010263132 0.0270417211
#> 827  b_Intercept 43.5128711779 0.0266982483
#> 828  b_Intercept 43.5247160426 0.0263556858
#> 829  b_Intercept 43.5365609073 0.0260140922
#> 830  b_Intercept 43.5484057720 0.0256735366
#> 831  b_Intercept 43.5602506367 0.0253340897
#> 832  b_Intercept 43.5720955014 0.0249960357
#> 833  b_Intercept 43.5839403661 0.0246594545
#> 834  b_Intercept 43.5957852308 0.0243242666
#> 835  b_Intercept 43.6076300955 0.0239905496
#> 836  b_Intercept 43.6194749602 0.0236583822
#> 837  b_Intercept 43.6313198249 0.0233279212
#> 838  b_Intercept 43.6431646896 0.0229996466
#> 839  b_Intercept 43.6550095543 0.0226732338
#> 840  b_Intercept 43.6668544191 0.0223487668
#> 841  b_Intercept 43.6786992838 0.0220263302
#> 842  b_Intercept 43.6905441485 0.0217060094
#> 843  b_Intercept 43.7023890132 0.0213884038
#> 844  b_Intercept 43.7142338779 0.0210733263
#> 845  b_Intercept 43.7260787426 0.0207606966
#> 846  b_Intercept 43.7379236073 0.0204506019
#> 847  b_Intercept 43.7497684720 0.0201431296
#> 848  b_Intercept 43.7616133367 0.0198386490
#> 849  b_Intercept 43.7734582014 0.0195375654
#> 850  b_Intercept 43.7853030661 0.0192394352
#> 851  b_Intercept 43.7971479308 0.0189443427
#> 852  b_Intercept 43.8089927955 0.0186523714
#> 853  b_Intercept 43.8208376602 0.0183636035
#> 854  b_Intercept 43.8326825250 0.0180790572
#> 855  b_Intercept 43.8445273897 0.0177980026
#> 856  b_Intercept 43.8563722544 0.0175204474
#> 857  b_Intercept 43.8682171191 0.0172464626
#> 858  b_Intercept 43.8800619838 0.0169761171
#> 859  b_Intercept 43.8919068485 0.0167100650
#> 860  b_Intercept 43.9037517132 0.0164483451
#> 861  b_Intercept 43.9155965779 0.0161904934
#> 862  b_Intercept 43.9274414426 0.0159365591
#> 863  b_Intercept 43.9392863073 0.0156865873
#> 864  b_Intercept 43.9511311720 0.0154407630
#> 865  b_Intercept 43.9629760367 0.0152000382
#> 866  b_Intercept 43.9748209014 0.0149634014
#> 867  b_Intercept 43.9866657661 0.0147308702
#> 868  b_Intercept 43.9985106308 0.0145024574
#> 869  b_Intercept 44.0103554956 0.0142781704
#> 870  b_Intercept 44.0222003603 0.0140588742
#> 871  b_Intercept 44.0340452250 0.0138440338
#> 872  b_Intercept 44.0458900897 0.0136332833
#> 873  b_Intercept 44.0577349544 0.0134265969
#> 874  b_Intercept 44.0695798191 0.0132239432
#> 875  b_Intercept 44.0814246838 0.0130256407
#> 876  b_Intercept 44.0932695485 0.0128320562
#> 877  b_Intercept 44.1051144132 0.0126423205
#> 878  b_Intercept 44.1169592779 0.0124563692
#> 879  b_Intercept 44.1288041426 0.0122741324
#> 880  b_Intercept 44.1406490073 0.0120955357
#> 881  b_Intercept 44.1524938720 0.0119214008
#> 882  b_Intercept 44.1643387367 0.0117507623
#> 883  b_Intercept 44.1761836014 0.0115834229
#> 884  b_Intercept 44.1880284662 0.0114192824
#> 885  b_Intercept 44.1998733309 0.0112582368
#> 886  b_Intercept 44.2117181956 0.0111005889
#> 887  b_Intercept 44.2235630603 0.0109461611
#> 888  b_Intercept 44.2354079250 0.0107943994
#> 889  b_Intercept 44.2472527897 0.0106451861
#> 890  b_Intercept 44.2590976544 0.0104984026
#> 891  b_Intercept 44.2709425191 0.0103539867
#> 892  b_Intercept 44.2827873838 0.0102122766
#> 893  b_Intercept 44.2946322485 0.0100725380
#> 894  b_Intercept 44.3064771132 0.0099346532
#> 895  b_Intercept 44.3183219779 0.0097985061
#> 896  b_Intercept 44.3301668426 0.0096639822
#> 897  b_Intercept 44.3420117073 0.0095312365
#> 898  b_Intercept 44.3538565721 0.0093999428
#> 899  b_Intercept 44.3657014368 0.0092698638
#> 900  b_Intercept 44.3775463015 0.0091409031
#> 901  b_Intercept 44.3893911662 0.0090129677
#> 902  b_Intercept 44.4012360309 0.0088860359
#> 903  b_Intercept 44.4130808956 0.0087600903
#> 904  b_Intercept 44.4249257603 0.0086348653
#> 905  b_Intercept 44.4367706250 0.0085102971
#> 906  b_Intercept 44.4486154897 0.0083863267
#> 907  b_Intercept 44.4604603544 0.0082629007
#> 908  b_Intercept 44.4723052191 0.0081400823
#> 909  b_Intercept 44.4841500838 0.0080177140
#> 910  b_Intercept 44.4959949485 0.0078957527
#> 911  b_Intercept 44.5078398132 0.0077741790
#> 912  b_Intercept 44.5196846779 0.0076529788
#> 913  b_Intercept 44.5315295427 0.0075321942
#> 914  b_Intercept 44.5433744074 0.0074118357
#> 915  b_Intercept 44.5552192721 0.0072918590
#> 916  b_Intercept 44.5670641368 0.0071722813
#> 917  b_Intercept 44.5789090015 0.0070531246
#> 918  b_Intercept 44.5907538662 0.0069344242
#> 919  b_Intercept 44.6025987309 0.0068163558
#> 920  b_Intercept 44.6144435956 0.0066988382
#> 921  b_Intercept 44.6262884603 0.0065819162
#> 922  b_Intercept 44.6381333250 0.0064656378
#> 923  b_Intercept 44.6499781897 0.0063500532
#> 924  b_Intercept 44.6618230544 0.0062353778
#> 925  b_Intercept 44.6736679191 0.0061216298
#> 926  b_Intercept 44.6855127838 0.0060087913
#> 927  b_Intercept 44.6973576486 0.0058969221
#> 928  b_Intercept 44.7092025133 0.0057860821
#> 929  b_Intercept 44.7210473780 0.0056764187
#> 930  b_Intercept 44.7328922427 0.0055681959
#> 931  b_Intercept 44.7447371074 0.0054612305
#> 932  b_Intercept 44.7565819721 0.0053555794
#> 933  b_Intercept 44.7684268368 0.0052512978
#> 934  b_Intercept 44.7802717015 0.0051484390
#> 935  b_Intercept 44.7921165662 0.0050474408
#> 936  b_Intercept 44.8039614309 0.0049480698
#> 937  b_Intercept 44.8158062956 0.0048502994
#> 938  b_Intercept 44.8276511603 0.0047541677
#> 939  b_Intercept 44.8394960250 0.0046597102
#> 940  b_Intercept 44.8513408897 0.0045671838
#> 941  b_Intercept 44.8631857544 0.0044766918
#> 942  b_Intercept 44.8750306192 0.0043879703
#> 943  b_Intercept 44.8868754839 0.0043010333
#> 944  b_Intercept 44.8987203486 0.0042158917
#> 945  b_Intercept 44.9105652133 0.0041325698
#> 946  b_Intercept 44.9224100780 0.0040515632
#> 947  b_Intercept 44.9342549427 0.0039723522
#> 948  b_Intercept 44.9460998074 0.0038949256
#> 949  b_Intercept 44.9579446721 0.0038192686
#> 950  b_Intercept 44.9697895368 0.0037453631
#> 951  b_Intercept 44.9816344015 0.0036734981
#> 952  b_Intercept 44.9934792662 0.0036035010
#> 953  b_Intercept 45.0053241309 0.0035351501
#> 954  b_Intercept 45.0171689956 0.0034684097
#> 955  b_Intercept 45.0290138603 0.0034032416
#> 956  b_Intercept 45.0408587250 0.0033397009
#> 957  b_Intercept 45.0527035898 0.0032779610
#> 958  b_Intercept 45.0645484545 0.0032176249
#> 959  b_Intercept 45.0763933192 0.0031586438
#> 960  b_Intercept 45.0882381839 0.0031009678
#> 961  b_Intercept 45.1000830486 0.0030445460
#> 962  b_Intercept 45.1119279133 0.0029895928
#> 963  b_Intercept 45.1237727780 0.0029358101
#> 964  b_Intercept 45.1356176427 0.0028830813
#> 965  b_Intercept 45.1474625074 0.0028313539
#> 966  b_Intercept 45.1593073721 0.0027805763
#> 967  b_Intercept 45.1711522368 0.0027307953
#> 968  b_Intercept 45.1829971015 0.0026819754
#> 969  b_Intercept 45.1948419662 0.0026339149
#> 970  b_Intercept 45.2066868309 0.0025865676
#> 971  b_Intercept 45.2185316957 0.0025398888
#> 972  b_Intercept 45.2303765604 0.0024938358
#> 973  b_Intercept 45.2422214251 0.0024485170
#> 974  b_Intercept 45.2540662898 0.0024037136
#> 975  b_Intercept 45.2659111545 0.0023593914
#> 976  b_Intercept 45.2777560192 0.0023155188
#> 977  b_Intercept 45.2896008839 0.0022720658
#> 978  b_Intercept 45.3014457486 0.0022290667
#> 979  b_Intercept 45.3132906133 0.0021864625
#> 980  b_Intercept 45.3251354780 0.0021441887
#> 981  b_Intercept 45.3369803427 0.0021022287
#> 982  b_Intercept 45.3488252074 0.0020605676
#> 983  b_Intercept 45.3606700721 0.0020192084
#> 984  b_Intercept 45.3725149368 0.0019781870
#> 985  b_Intercept 45.3843598015 0.0019374315
#> 986  b_Intercept 45.3962046663 0.0018969390
#> 987  b_Intercept 45.4080495310 0.0018567085
#> 988  b_Intercept 45.4198943957 0.0018167409
#> 989  b_Intercept 45.4317392604 0.0017771020
#> 990  b_Intercept 45.4435841251 0.0017377552
#> 991  b_Intercept 45.4554289898 0.0016986916
#> 992  b_Intercept 45.4672738545 0.0016599202
#> 993  b_Intercept 45.4791187192 0.0016214510
#> 994  b_Intercept 45.4909635839 0.0015833319
#> 995  b_Intercept 45.5028084486 0.0015456058
#> 996  b_Intercept 45.5146533133 0.0015082289
#> 997  b_Intercept 45.5264981780 0.0014712151
#> 998  b_Intercept 45.5383430427 0.0014345784
#> 999  b_Intercept 45.5501879074 0.0013983333
#> 1000 b_Intercept 45.5620327721 0.0013626180
#> 1001 b_Intercept 45.5738776369 0.0013273380
#> 1002 b_Intercept 45.5857225016 0.0012925042
#> 1003 b_Intercept 45.5975673663 0.0012581302
#> 1004 b_Intercept 45.6094122310 0.0012242291
#> 1005 b_Intercept 45.6212570957 0.0011908982
#> 1006 b_Intercept 45.6331019604 0.0011581360
#> 1007 b_Intercept 45.6449468251 0.0011258911
#> 1008 b_Intercept 45.6567916898 0.0010941729
#> 1009 b_Intercept 45.6686365545 0.0010629904
#> 1010 b_Intercept 45.6804814192 0.0010323780
#> 1011 b_Intercept 45.6923262839 0.0010024549
#> 1012 b_Intercept 45.7041711486 0.0009730922
#> 1013 b_Intercept 45.7160160133 0.0009442935
#> 1014 b_Intercept 45.7278608780 0.0009160619
#> 1015 b_Intercept 45.7397057428 0.0008883996
#> 1016 b_Intercept 45.7515506075 0.0008614343
#> 1017 b_Intercept 45.7633954722 0.0008350794
#> 1018 b_Intercept 45.7752403369 0.0008092921
#> 1019 b_Intercept 45.7870852016 0.0007840699
#> 1020 b_Intercept 45.7989300663 0.0007594095
#> 1021 b_Intercept 45.8107749310 0.0007353635
#> 1022 b_Intercept 45.8226197957 0.0007119720
#> 1023 b_Intercept 45.8344646604 0.0006891225
#> 1024 b_Intercept 45.8463095251 0.0006668083
#> 1025        b_wt -6.1750313102 0.0008472559
#> 1026        b_wt -6.1694053690 0.0008608107
#> 1027        b_wt -6.1637794278 0.0008742644
#> 1028        b_wt -6.1581534866 0.0008876426
#> 1029        b_wt -6.1525275454 0.0009009729
#> 1030        b_wt -6.1469016042 0.0009142849
#> 1031        b_wt -6.1412756629 0.0009276207
#> 1032        b_wt -6.1356497217 0.0009410338
#> 1033        b_wt -6.1300237805 0.0009545601
#> 1034        b_wt -6.1243978393 0.0009682399
#> 1035        b_wt -6.1187718981 0.0009821145
#> 1036        b_wt -6.1131459569 0.0009962270
#> 1037        b_wt -6.1075200157 0.0010107063
#> 1038        b_wt -6.1018940744 0.0010255603
#> 1039        b_wt -6.0962681332 0.0010408264
#> 1040        b_wt -6.0906421920 0.0010565535
#> 1041        b_wt -6.0850162508 0.0010727916
#> 1042        b_wt -6.0793903096 0.0010896213
#> 1043        b_wt -6.0737643684 0.0011072274
#> 1044        b_wt -6.0681384272 0.0011255352
#> 1045        b_wt -6.0625124859 0.0011445963
#> 1046        b_wt -6.0568865447 0.0011644620
#> 1047        b_wt -6.0512606035 0.0011851837
#> 1048        b_wt -6.0456346623 0.0012069433
#> 1049        b_wt -6.0400087211 0.0012298239
#> 1050        b_wt -6.0343827799 0.0012537478
#> 1051        b_wt -6.0287568387 0.0012787635
#> 1052        b_wt -6.0231308974 0.0013049185
#> 1053        b_wt -6.0175049562 0.0013322595
#> 1054        b_wt -6.0118790150 0.0013611172
#> 1055        b_wt -6.0062530738 0.0013913488
#> 1056        b_wt -6.0006271326 0.0014229301
#> 1057        b_wt -5.9950011914 0.0014559018
#> 1058        b_wt -5.9893752502 0.0014903032
#> 1059        b_wt -5.9837493089 0.0015262218
#> 1060        b_wt -5.9781233677 0.0015640225
#> 1061        b_wt -5.9724974265 0.0016033874
#> 1062        b_wt -5.9668714853 0.0016443485
#> 1063        b_wt -5.9612455441 0.0016869367
#> 1064        b_wt -5.9556196029 0.0017311813
#> 1065        b_wt -5.9499936617 0.0017773130
#> 1066        b_wt -5.9443677204 0.0018254337
#> 1067        b_wt -5.9387417792 0.0018753069
#> 1068        b_wt -5.9331158380 0.0019269546
#> 1069        b_wt -5.9274898968 0.0019803977
#> 1070        b_wt -5.9218639556 0.0020356557
#> 1071        b_wt -5.9162380144 0.0020931241
#> 1072        b_wt -5.9106120731 0.0021525798
#> 1073        b_wt -5.9049861319 0.0022139111
#> 1074        b_wt -5.8993601907 0.0022771313
#> 1075        b_wt -5.8937342495 0.0023422524
#> 1076        b_wt -5.8881083083 0.0024093177
#> 1077        b_wt -5.8824823671 0.0024787991
#> 1078        b_wt -5.8768564259 0.0025502173
#> 1079        b_wt -5.8712304846 0.0026235791
#> 1080        b_wt -5.8656045434 0.0026988907
#> 1081        b_wt -5.8599786022 0.0027761573
#> 1082        b_wt -5.8543526610 0.0028555836
#> 1083        b_wt -5.8487267198 0.0029373069
#> 1084        b_wt -5.8431007786 0.0030209968
#> 1085        b_wt -5.8374748374 0.0031066542
#> 1086        b_wt -5.8318488961 0.0031942793
#> 1087        b_wt -5.8262229549 0.0032838715
#> 1088        b_wt -5.8205970137 0.0033757967
#> 1089        b_wt -5.8149710725 0.0034698484
#> 1090        b_wt -5.8093451313 0.0035658571
#> 1091        b_wt -5.8037191901 0.0036638178
#> 1092        b_wt -5.7980932489 0.0037637248
#> 1093        b_wt -5.7924673076 0.0038655733
#> 1094        b_wt -5.7868413664 0.0039698718
#> 1095        b_wt -5.7812154252 0.0040760859
#> 1096        b_wt -5.7755894840 0.0041842045
#> 1097        b_wt -5.7699635428 0.0042942154
#> 1098        b_wt -5.7643376016 0.0044061052
#> 1099        b_wt -5.7587116604 0.0045200182
#> 1100        b_wt -5.7530857191 0.0046361118
#> 1101        b_wt -5.7474597779 0.0047540219
#> 1102        b_wt -5.7418338367 0.0048737282
#> 1103        b_wt -5.7362078955 0.0049952088
#> 1104        b_wt -5.7305819543 0.0051184404
#> 1105        b_wt -5.7249560131 0.0052436867
#> 1106        b_wt -5.7193300719 0.0053707831
#> 1107        b_wt -5.7137041306 0.0054995275
#> 1108        b_wt -5.7080781894 0.0056298880
#> 1109        b_wt -5.7024522482 0.0057618313
#> 1110        b_wt -5.6968263070 0.0058953227
#> 1111        b_wt -5.6912003658 0.0060306964
#> 1112        b_wt -5.6855744246 0.0061675353
#> 1113        b_wt -5.6799484834 0.0063057762
#> 1114        b_wt -5.6743225421 0.0064453765
#> 1115        b_wt -5.6686966009 0.0065862926
#> 1116        b_wt -5.6630706597 0.0067285647
#> 1117        b_wt -5.6574447185 0.0068722819
#> 1118        b_wt -5.6518187773 0.0070171426
#> 1119        b_wt -5.6461928361 0.0071630993
#> 1120        b_wt -5.6405668949 0.0073101044
#> 1121        b_wt -5.6349409536 0.0074581103
#> 1122        b_wt -5.6293150124 0.0076072066
#> 1123        b_wt -5.6236890712 0.0077572849
#> 1124        b_wt -5.6180631300 0.0079081918
#> 1125        b_wt -5.6124371888 0.0080598843
#> 1126        b_wt -5.6068112476 0.0082123215
#> 1127        b_wt -5.6011853063 0.0083654640
#> 1128        b_wt -5.5955593651 0.0085194227
#> 1129        b_wt -5.5899334239 0.0086740148
#> 1130        b_wt -5.5843074827 0.0088291942
#> 1131        b_wt -5.5786815415 0.0089849385
#> 1132        b_wt -5.5730556003 0.0091412291
#> 1133        b_wt -5.5674296591 0.0092980801
#> 1134        b_wt -5.5618037178 0.0094555663
#> 1135        b_wt -5.5561777766 0.0096135762
#> 1136        b_wt -5.5505518354 0.0097721191
#> 1137        b_wt -5.5449258942 0.0099312097
#> 1138        b_wt -5.5392999530 0.0100908688
#> 1139        b_wt -5.5336740118 0.0102512149
#> 1140        b_wt -5.5280480706 0.0104123032
#> 1141        b_wt -5.5224221293 0.0105741057
#> 1142        b_wt -5.5167961881 0.0107366806
#> 1143        b_wt -5.5111702469 0.0109000931
#> 1144        b_wt -5.5055443057 0.0110644153
#> 1145        b_wt -5.4999183645 0.0112299845
#> 1146        b_wt -5.4942924233 0.0113967533
#> 1147        b_wt -5.4886664821 0.0115647806
#> 1148        b_wt -5.4830405408 0.0117341792
#> 1149        b_wt -5.4774145996 0.0119050688
#> 1150        b_wt -5.4717886584 0.0120776467
#> 1151        b_wt -5.4661627172 0.0122524591
#> 1152        b_wt -5.4605367760 0.0124292794
#> 1153        b_wt -5.4549108348 0.0126082640
#> 1154        b_wt -5.4492848936 0.0127895749
#> 1155        b_wt -5.4436589523 0.0129733795
#> 1156        b_wt -5.4380330111 0.0131602163
#> 1157        b_wt -5.4324070699 0.0133504317
#> 1158        b_wt -5.4267811287 0.0135438178
#> 1159        b_wt -5.4211551875 0.0137405667
#> 1160        b_wt -5.4155292463 0.0139408732
#> 1161        b_wt -5.4099033051 0.0141449350
#> 1162        b_wt -5.4042773638 0.0143538514
#> 1163        b_wt -5.3986514226 0.0145673254
#> 1164        b_wt -5.3930254814 0.0147853134
#> 1165        b_wt -5.3873995402 0.0150080195
#> 1166        b_wt -5.3817735990 0.0152356477
#> 1167        b_wt -5.3761476578 0.0154685279
#> 1168        b_wt -5.3705217166 0.0157081560
#> 1169        b_wt -5.3648957753 0.0159534563
#> 1170        b_wt -5.3592698341 0.0162046220
#> 1171        b_wt -5.3536438929 0.0164618426
#> 1172        b_wt -5.3480179517 0.0167253039
#> 1173        b_wt -5.3423920105 0.0169959138
#> 1174        b_wt -5.3367660693 0.0172742966
#> 1175        b_wt -5.3311401281 0.0175595652
#> 1176        b_wt -5.3255141868 0.0178518754
#> 1177        b_wt -5.3198882456 0.0181513763
#> 1178        b_wt -5.3142623044 0.0184582104
#> 1179        b_wt -5.3086363632 0.0187739988
#> 1180        b_wt -5.3030104220 0.0190980461
#> 1181        b_wt -5.2973844808 0.0194298772
#> 1182        b_wt -5.2917585395 0.0197695892
#> 1183        b_wt -5.2861325983 0.0201172708
#> 1184        b_wt -5.2805066571 0.0204730647
#> 1185        b_wt -5.2748807159 0.0208391439
#> 1186        b_wt -5.2692547747 0.0212134319
#> 1187        b_wt -5.2636288335 0.0215959645
#> 1188        b_wt -5.2580028923 0.0219867681
#> 1189        b_wt -5.2523769510 0.0223858598
#> 1190        b_wt -5.2467510098 0.0227940164
#> 1191        b_wt -5.2411250686 0.0232119313
#> 1192        b_wt -5.2354991274 0.0236380969
#> 1193        b_wt -5.2298731862 0.0240724760
#> 1194        b_wt -5.2242472450 0.0245150222
#> 1195        b_wt -5.2186213038 0.0249656803
#> 1196        b_wt -5.2129953625 0.0254257965
#> 1197        b_wt -5.2073694213 0.0258945673
#> 1198        b_wt -5.2017434801 0.0263711492
#> 1199        b_wt -5.1961175389 0.0268554394
#> 1200        b_wt -5.1904915977 0.0273473272
#> 1201        b_wt -5.1848656565 0.0278466948
#> 1202        b_wt -5.1792397153 0.0283552909
#> 1203        b_wt -5.1736137740 0.0288710582
#> 1204        b_wt -5.1679878328 0.0293937903
#> 1205        b_wt -5.1623618916 0.0299233338
#> 1206        b_wt -5.1567359504 0.0304595300
#> 1207        b_wt -5.1511100092 0.0310026972
#> 1208        b_wt -5.1454840680 0.0315533105
#> 1209        b_wt -5.1398581268 0.0321099354
#> 1210        b_wt -5.1342321855 0.0326723896
#> 1211        b_wt -5.1286062443 0.0332404880
#> 1212        b_wt -5.1229803031 0.0338140426
#> 1213        b_wt -5.1173543619 0.0343936678
#> 1214        b_wt -5.1117284207 0.0349787875
#> 1215        b_wt -5.1061024795 0.0355686355
#> 1216        b_wt -5.1004765383 0.0361630128
#> 1217        b_wt -5.0948505970 0.0367617190
#> 1218        b_wt -5.0892246558 0.0373645528
#> 1219        b_wt -5.0835987146 0.0379721978
#> 1220        b_wt -5.0779727734 0.0385835042
#> 1221        b_wt -5.0723468322 0.0391981766
#> 1222        b_wt -5.0667208910 0.0398160105
#> 1223        b_wt -5.0610949498 0.0404368011
#> 1224        b_wt -5.0554690085 0.0410604946
#> 1225        b_wt -5.0498430673 0.0416871563
#> 1226        b_wt -5.0442171261 0.0423160077
#> 1227        b_wt -5.0385911849 0.0429468444
#> 1228        b_wt -5.0329652437 0.0435794621
#> 1229        b_wt -5.0273393025 0.0442136571
#> 1230        b_wt -5.0217133613 0.0448493806
#> 1231        b_wt -5.0160874200 0.0454862903
#> 1232        b_wt -5.0104614788 0.0461240190
#> 1233        b_wt -5.0048355376 0.0467623666
#> 1234        b_wt -4.9992095964 0.0474011336
#> 1235        b_wt -4.9935836552 0.0480401223
#> 1236        b_wt -4.9879577140 0.0486790659
#> 1237        b_wt -4.9823317727 0.0493176965
#> 1238        b_wt -4.9767058315 0.0499558267
#> 1239        b_wt -4.9710798903 0.0505932704
#> 1240        b_wt -4.9654539491 0.0512298448
#> 1241        b_wt -4.9598280079 0.0518653111
#> 1242        b_wt -4.9542020667 0.0524991889
#> 1243        b_wt -4.9485761255 0.0531315550
#> 1244        b_wt -4.9429501842 0.0537622535
#> 1245        b_wt -4.9373242430 0.0543911351
#> 1246        b_wt -4.9316983018 0.0550180573
#> 1247        b_wt -4.9260723606 0.0556425866
#> 1248        b_wt -4.9204464194 0.0562645182
#> 1249        b_wt -4.9148204782 0.0568840486
#> 1250        b_wt -4.9091945370 0.0575010893
#> 1251        b_wt -4.9035685957 0.0581155633
#> 1252        b_wt -4.8979426545 0.0587274057
#> 1253        b_wt -4.8923167133 0.0593359760
#> 1254        b_wt -4.8866907721 0.0599416753
#> 1255        b_wt -4.8810648309 0.0605446553
#> 1256        b_wt -4.8754388897 0.0611449365
#> 1257        b_wt -4.8698129485 0.0617425561
#> 1258        b_wt -4.8641870072 0.0623374935
#> 1259        b_wt -4.8585610660 0.0629293441
#> 1260        b_wt -4.8529351248 0.0635188607
#> 1261        b_wt -4.8473091836 0.0641061883
#> 1262        b_wt -4.8416832424 0.0646914923
#> 1263        b_wt -4.8360573012 0.0652749579
#> 1264        b_wt -4.8304313600 0.0658566459
#> 1265        b_wt -4.8248054187 0.0664368785
#> 1266        b_wt -4.8191794775 0.0670161881
#> 1267        b_wt -4.8135535363 0.0675948792
#> 1268        b_wt -4.8079275951 0.0681732761
#> 1269        b_wt -4.8023016539 0.0687517235
#> 1270        b_wt -4.7966757127 0.0693308019
#> 1271        b_wt -4.7910497715 0.0699110348
#> 1272        b_wt -4.7854238302 0.0704928189
#> 1273        b_wt -4.7797978890 0.0710766046
#> 1274        b_wt -4.7741719478 0.0716628587
#> 1275        b_wt -4.7685460066 0.0722521164
#> 1276        b_wt -4.7629200654 0.0728459663
#> 1277        b_wt -4.7572941242 0.0734441805
#> 1278        b_wt -4.7516681830 0.0740473014
#> 1279        b_wt -4.7460422417 0.0746558807
#> 1280        b_wt -4.7404163005 0.0752704784
#> 1281        b_wt -4.7347903593 0.0758924059
#> 1282        b_wt -4.7291644181 0.0765230718
#> 1283        b_wt -4.7235384769 0.0771619170
#> 1284        b_wt -4.7179125357 0.0778095272
#> 1285        b_wt -4.7122865945 0.0784664884
#> 1286        b_wt -4.7066606532 0.0791333860
#> 1287        b_wt -4.7010347120 0.0798129220
#> 1288        b_wt -4.6954087708 0.0805049050
#> 1289        b_wt -4.6897828296 0.0812089790
#> 1290        b_wt -4.6841568884 0.0819257025
#> 1291        b_wt -4.6785309472 0.0826556254
#> 1292        b_wt -4.6729050059 0.0833992890
#> 1293        b_wt -4.6672790647 0.0841613184
#> 1294        b_wt -4.6616531235 0.0849385214
#> 1295        b_wt -4.6560271823 0.0857313671
#> 1296        b_wt -4.6504012411 0.0865403282
#> 1297        b_wt -4.6447752999 0.0873658636
#> 1298        b_wt -4.6391493587 0.0882098997
#> 1299        b_wt -4.6335234174 0.0890748800
#> 1300        b_wt -4.6278974762 0.0899579943
#> 1301        b_wt -4.6222715350 0.0908596127
#> 1302        b_wt -4.6166455938 0.0917800891
#> 1303        b_wt -4.6110196526 0.0927197612
#> 1304        b_wt -4.6053937114 0.0936823308
#> 1305        b_wt -4.5997677702 0.0946669204
#> 1306        b_wt -4.5941418289 0.0956717894
#> 1307        b_wt -4.5885158877 0.0966971805
#> 1308        b_wt -4.5828899465 0.0977433199
#> 1309        b_wt -4.5772640053 0.0988104178
#> 1310        b_wt -4.5716380641 0.0999041198
#> 1311        b_wt -4.5660121229 0.1010196123
#> 1312        b_wt -4.5603861817 0.1021566719
#> 1313        b_wt -4.5547602404 0.1033154184
#> 1314        b_wt -4.5491342992 0.1044959563
#> 1315        b_wt -4.5435083580 0.1056998759
#> 1316        b_wt -4.5378824168 0.1069302295
#> 1317        b_wt -4.5322564756 0.1081826074
#> 1318        b_wt -4.5266305344 0.1094570278
#> 1319        b_wt -4.5210045932 0.1107534943
#> 1320        b_wt -4.5153786519 0.1120719955
#> 1321        b_wt -4.5097527107 0.1134158701
#> 1322        b_wt -4.5041267695 0.1147842476
#> 1323        b_wt -4.4985008283 0.1161744532
#> 1324        b_wt -4.4928748871 0.1175863878
#> 1325        b_wt -4.4872489459 0.1190199363
#> 1326        b_wt -4.4816230047 0.1204749678
#> 1327        b_wt -4.4759970634 0.1219563544
#> 1328        b_wt -4.4703711222 0.1234594525
#> 1329        b_wt -4.4647451810 0.1249833581
#> 1330        b_wt -4.4591192398 0.1265278435
#> 1331        b_wt -4.4534932986 0.1280926632
#> 1332        b_wt -4.4478673574 0.1296785880
#> 1333        b_wt -4.4422414162 0.1312884559
#> 1334        b_wt -4.4366154749 0.1329175509
#> 1335        b_wt -4.4309895337 0.1345655231
#> 1336        b_wt -4.4253635925 0.1362320043
#> 1337        b_wt -4.4197376513 0.1379166081
#> 1338        b_wt -4.4141117101 0.1396212774
#> 1339        b_wt -4.4084857689 0.1413452532
#> 1340        b_wt -4.4028598277 0.1430857104
#> 1341        b_wt -4.3972338864 0.1448421612
#> 1342        b_wt -4.3916079452 0.1466141013
#> 1343        b_wt -4.3859820040 0.1484010110
#> 1344        b_wt -4.3803560628 0.1502053631
#> 1345        b_wt -4.3747301216 0.1520238544
#> 1346        b_wt -4.3691041804 0.1538552119
#> 1347        b_wt -4.3634782391 0.1556988410
#> 1348        b_wt -4.3578522979 0.1575541372
#> 1349        b_wt -4.3522263567 0.1594208626
#> 1350        b_wt -4.3466004155 0.1613001357
#> 1351        b_wt -4.3409744743 0.1631887390
#> 1352        b_wt -4.3353485331 0.1650860375
#> 1353        b_wt -4.3297225919 0.1669913959
#> 1354        b_wt -4.3240966506 0.1689041802
#> 1355        b_wt -4.3184707094 0.1708244708
#> 1356        b_wt -4.3128447682 0.1727514669
#> 1357        b_wt -4.3072188270 0.1746835631
#> 1358        b_wt -4.3015928858 0.1766201633
#> 1359        b_wt -4.2959669446 0.1785606834
#> 1360        b_wt -4.2903410034 0.1805045525
#> 1361        b_wt -4.2847150621 0.1824516145
#> 1362        b_wt -4.2790891209 0.1844007304
#> 1363        b_wt -4.2734631797 0.1863512450
#> 1364        b_wt -4.2678372385 0.1883026946
#> 1365        b_wt -4.2622112973 0.1902546381
#> 1366        b_wt -4.2565853561 0.1922066484
#> 1367        b_wt -4.2509594149 0.1941580951
#> 1368        b_wt -4.2453334736 0.1961086423
#> 1369        b_wt -4.2397075324 0.1980579982
#> 1370        b_wt -4.2340815912 0.2000058993
#> 1371        b_wt -4.2284556500 0.2019521102
#> 1372        b_wt -4.2228297088 0.2038961972
#> 1373        b_wt -4.2172037676 0.2058378009
#> 1374        b_wt -4.2115778264 0.2077771385
#> 1375        b_wt -4.2059518851 0.2097141383
#> 1376        b_wt -4.2003259439 0.2116487555
#> 1377        b_wt -4.1947000027 0.2135809723
#> 1378        b_wt -4.1890740615 0.2155103566
#> 1379        b_wt -4.1834481203 0.2174372597
#> 1380        b_wt -4.1778221791 0.2193619684
#> 1381        b_wt -4.1721962379 0.2212846007
#> 1382        b_wt -4.1665702966 0.2232052943
#> 1383        b_wt -4.1609443554 0.2251241973
#> 1384        b_wt -4.1553184142 0.2270411643
#> 1385        b_wt -4.1496924730 0.2289568830
#> 1386        b_wt -4.1440665318 0.2308715782
#> 1387        b_wt -4.1384405906 0.2327854844
#> 1388        b_wt -4.1328146494 0.2346988449
#> 1389        b_wt -4.1271887081 0.2366119189
#> 1390        b_wt -4.1215627669 0.2385251036
#> 1391        b_wt -4.1159368257 0.2404387113
#> 1392        b_wt -4.1103108845 0.2423530069
#> 1393        b_wt -4.1046849433 0.2442682541
#> 1394        b_wt -4.0990590021 0.2461847154
#> 1395        b_wt -4.0934330609 0.2481029807
#> 1396        b_wt -4.0878071196 0.2500233033
#> 1397        b_wt -4.0821811784 0.2519457770
#> 1398        b_wt -4.0765552372 0.2538706350
#> 1399        b_wt -4.0709292960 0.2557981031
#> 1400        b_wt -4.0653033548 0.2577283993
#> 1401        b_wt -4.0596774136 0.2596626141
#> 1402        b_wt -4.0540514723 0.2616002340
#> 1403        b_wt -4.0484255311 0.2635414061
#> 1404        b_wt -4.0427995899 0.2654863013
#> 1405        b_wt -4.0371736487 0.2674350826
#> 1406        b_wt -4.0315477075 0.2693882249
#> 1407        b_wt -4.0259217663 0.2713464557
#> 1408        b_wt -4.0202958251 0.2733091111
#> 1409        b_wt -4.0146698838 0.2752763192
#> 1410        b_wt -4.0090439426 0.2772482044
#> 1411        b_wt -4.0034180014 0.2792248882
#> 1412        b_wt -3.9977920602 0.2812073075
#> 1413        b_wt -3.9921661190 0.2831953981
#> 1414        b_wt -3.9865401778 0.2851887302
#> 1415        b_wt -3.9809142366 0.2871874252
#> 1416        b_wt -3.9752882953 0.2891916076
#> 1417        b_wt -3.9696623541 0.2912014052
#> 1418        b_wt -3.9640364129 0.2932184218
#> 1419        b_wt -3.9584104717 0.2952415877
#> 1420        b_wt -3.9527845305 0.2972709087
#> 1421        b_wt -3.9471585893 0.2993065482
#> 1422        b_wt -3.9415326481 0.3013486771
#> 1423        b_wt -3.9359067068 0.3033978871
#> 1424        b_wt -3.9302807656 0.3054555193
#> 1425        b_wt -3.9246548244 0.3075203678
#> 1426        b_wt -3.9190288832 0.3095926509
#> 1427        b_wt -3.9134029420 0.3116725950
#> 1428        b_wt -3.9077770008 0.3137604333
#> 1429        b_wt -3.9021510596 0.3158576396
#> 1430        b_wt -3.8965251183 0.3179644562
#> 1431        b_wt -3.8908991771 0.3200801050
#> 1432        b_wt -3.8852732359 0.3222048471
#> 1433        b_wt -3.8796472947 0.3243389450
#> 1434        b_wt -3.8740213535 0.3264826614
#> 1435        b_wt -3.8683954123 0.3286386277
#> 1436        b_wt -3.8627694711 0.3308053318
#> 1437        b_wt -3.8571435298 0.3329826034
#> 1438        b_wt -3.8515175886 0.3351706760
#> 1439        b_wt -3.8458916474 0.3373697728
#> 1440        b_wt -3.8402657062 0.3395806073
#> 1441        b_wt -3.8346397650 0.3418055611
#> 1442        b_wt -3.8290138238 0.3440422225
#> 1443        b_wt -3.8233878826 0.3462907172
#> 1444        b_wt -3.8177619413 0.3485511470
#> 1445        b_wt -3.8121360001 0.3508235891
#> 1446        b_wt -3.8065100589 0.3531096482
#> 1447        b_wt -3.8008841177 0.3554094797
#> 1448        b_wt -3.7952581765 0.3577212992
#> 1449        b_wt -3.7896322353 0.3600450042
#> 1450        b_wt -3.7840062941 0.3623804559
#> 1451        b_wt -3.7783803528 0.3647274778
#> 1452        b_wt -3.7727544116 0.3670881789
#> 1453        b_wt -3.7671284704 0.3694603564
#> 1454        b_wt -3.7615025292 0.3718429926
#> 1455        b_wt -3.7558765880 0.3742356698
#> 1456        b_wt -3.7502506468 0.3766379263
#> 1457        b_wt -3.7446247055 0.3790494863
#> 1458        b_wt -3.7389987643 0.3814713053
#> 1459        b_wt -3.7333728231 0.3839005024
#> 1460        b_wt -3.7277468819 0.3863363528
#> 1461        b_wt -3.7221209407 0.3887780870
#> 1462        b_wt -3.7164949995 0.3912248904
#> 1463        b_wt -3.7108690583 0.3936762183
#> 1464        b_wt -3.7052431170 0.3961307622
#> 1465        b_wt -3.6996171758 0.3985868609
#> 1466        b_wt -3.6939912346 0.4010434571
#> 1467        b_wt -3.6883652934 0.4034994553
#> 1468        b_wt -3.6827393522 0.4059537236
#> 1469        b_wt -3.6771134110 0.4084041165
#> 1470        b_wt -3.6714874698 0.4108492252
#> 1471        b_wt -3.6658615285 0.4132880024
#> 1472        b_wt -3.6602355873 0.4157191405
#> 1473        b_wt -3.6546096461 0.4181413087
#> 1474        b_wt -3.6489837049 0.4205530038
#> 1475        b_wt -3.6433577637 0.4229492873
#> 1476        b_wt -3.6377318225 0.4253314279
#> 1477        b_wt -3.6321058813 0.4276980076
#> 1478        b_wt -3.6264799400 0.4300476036
#> 1479        b_wt -3.6208539988 0.4323787894
#> 1480        b_wt -3.6152280576 0.4346879938
#> 1481        b_wt -3.6096021164 0.4369714002
#> 1482        b_wt -3.6039761752 0.4392310851
#> 1483        b_wt -3.5983502340 0.4414656591
#> 1484        b_wt -3.5927242928 0.4436737495
#> 1485        b_wt -3.5870983515 0.4458540022
#> 1486        b_wt -3.5814724103 0.4479993734
#> 1487        b_wt -3.5758464691 0.4501107264
#> 1488        b_wt -3.5702205279 0.4521894486
#> 1489        b_wt -3.5645945867 0.4542343511
#> 1490        b_wt -3.5589686455 0.4562442824
#> 1491        b_wt -3.5533427043 0.4582181309
#> 1492        b_wt -3.5477167630 0.4601444200
#> 1493        b_wt -3.5420908218 0.4620317394
#> 1494        b_wt -3.5364648806 0.4638793283
#> 1495        b_wt -3.5308389394 0.4656863530
#> 1496        b_wt -3.5252129982 0.4674520348
#> 1497        b_wt -3.5195870570 0.4691721341
#> 1498        b_wt -3.5139611158 0.4708411077
#> 1499        b_wt -3.5083351745 0.4724664457
#> 1500        b_wt -3.5027092333 0.4740477199
#> 1501        b_wt -3.4970832921 0.4755845670
#> 1502        b_wt -3.4914573509 0.4770766898
#> 1503        b_wt -3.4858314097 0.4785163228
#> 1504        b_wt -3.4802054685 0.4799061408
#> 1505        b_wt -3.4745795273 0.4812508613
#> 1506        b_wt -3.4689535860 0.4825505806
#> 1507        b_wt -3.4633276448 0.4838054640
#> 1508        b_wt -3.4577017036 0.4850157453
#> 1509        b_wt -3.4520757624 0.4861707472
#> 1510        b_wt -3.4464498212 0.4872813875
#> 1511        b_wt -3.4408238800 0.4883490622
#> 1512        b_wt -3.4351979387 0.4893743898
#> 1513        b_wt -3.4295719975 0.4903580517
#> 1514        b_wt -3.4239460563 0.4912981668
#> 1515        b_wt -3.4183201151 0.4921904562
#> 1516        b_wt -3.4126941739 0.4930442607
#> 1517        b_wt -3.4070682327 0.4938605920
#> 1518        b_wt -3.4014422915 0.4946405112
#> 1519        b_wt -3.3958163502 0.4953851267
#> 1520        b_wt -3.3901904090 0.4960907191
#> 1521        b_wt -3.3845644678 0.4967602432
#> 1522        b_wt -3.3789385266 0.4973990598
#> 1523        b_wt -3.3733125854 0.4980084915
#> 1524        b_wt -3.3676866442 0.4985898860
#> 1525        b_wt -3.3620607030 0.4991446140
#> 1526        b_wt -3.3564347617 0.4996686560
#> 1527        b_wt -3.3508088205 0.5001690284
#> 1528        b_wt -3.3451828793 0.5006480157
#> 1529        b_wt -3.3395569381 0.5011070374
#> 1530        b_wt -3.3339309969 0.5015475072
#> 1531        b_wt -3.3283050557 0.5019700657
#> 1532        b_wt -3.3226791145 0.5023741163
#> 1533        b_wt -3.3170531732 0.5027647546
#> 1534        b_wt -3.3114272320 0.5031432775
#> 1535        b_wt -3.3058012908 0.5035109453
#> 1536        b_wt -3.3001753496 0.5038689795
#> 1537        b_wt -3.2945494084 0.5042176422
#> 1538        b_wt -3.2889234672 0.5045586235
#> 1539        b_wt -3.2832975260 0.5048940202
#> 1540        b_wt -3.2776715847 0.5052247456
#> 1541        b_wt -3.2720456435 0.5055516462
#> 1542        b_wt -3.2664197023 0.5058754997
#> 1543        b_wt -3.2607937611 0.5061966977
#> 1544        b_wt -3.2551678199 0.5065163968
#> 1545        b_wt -3.2495418787 0.5068351121
#> 1546        b_wt -3.2439159375 0.5071531637
#> 1547        b_wt -3.2382899962 0.5074707848
#> 1548        b_wt -3.2326640550 0.5077881110
#> 1549        b_wt -3.2270381138 0.5081050770
#> 1550        b_wt -3.2214121726 0.5084215535
#> 1551        b_wt -3.2157862314 0.5087372520
#> 1552        b_wt -3.2101602902 0.5090517926
#> 1553        b_wt -3.2045343490 0.5093647052
#> 1554        b_wt -3.1989084077 0.5096750326
#> 1555        b_wt -3.1932824665 0.5099815586
#> 1556        b_wt -3.1876565253 0.5102837451
#> 1557        b_wt -3.1820305841 0.5105806241
#> 1558        b_wt -3.1764046429 0.5108711498
#> 1559        b_wt -3.1707787017 0.5111541999
#> 1560        b_wt -3.1651527605 0.5114263466
#> 1561        b_wt -3.1595268192 0.5116868997
#> 1562        b_wt -3.1539008780 0.5119350224
#> 1563        b_wt -3.1482749368 0.5121692280
#> 1564        b_wt -3.1426489956 0.5123879814
#> 1565        b_wt -3.1370230544 0.5125893336
#> 1566        b_wt -3.1313971132 0.5127664831
#> 1567        b_wt -3.1257711719 0.5129220340
#> 1568        b_wt -3.1201452307 0.5130542681
#> 1569        b_wt -3.1145192895 0.5131614554
#> 1570        b_wt -3.1088933483 0.5132418574
#> 1571        b_wt -3.1032674071 0.5132904831
#> 1572        b_wt -3.0976414659 0.5133027533
#> 1573        b_wt -3.0920155247 0.5132817724
#> 1574        b_wt -3.0863895834 0.5132258652
#> 1575        b_wt -3.0807636422 0.5131333863
#> 1576        b_wt -3.0751377010 0.5130027240
#> 1577        b_wt -3.0695117598 0.5128243126
#> 1578        b_wt -3.0638858186 0.5126002774
#> 1579        b_wt -3.0582598774 0.5123325211
#> 1580        b_wt -3.0526339362 0.5120197242
#> 1581        b_wt -3.0470079949 0.5116606324
#> 1582        b_wt -3.0413820537 0.5112538933
#> 1583        b_wt -3.0357561125 0.5107850827
#> 1584        b_wt -3.0301301713 0.5102660225
#> 1585        b_wt -3.0245042301 0.5096958964
#> 1586        b_wt -3.0188782889 0.5090739728
#> 1587        b_wt -3.0132523477 0.5083996066
#> 1588        b_wt -3.0076264064 0.5076674926
#> 1589        b_wt -3.0020004652 0.5068719918
#> 1590        b_wt -2.9963745240 0.5060225807
#> 1591        b_wt -2.9907485828 0.5051191341
#> 1592        b_wt -2.9851226416 0.5041616179
#> 1593        b_wt -2.9794967004 0.5031500888
#> 1594        b_wt -2.9738707592 0.5020754230
#> 1595        b_wt -2.9682448179 0.5009422072
#> 1596        b_wt -2.9626188767 0.4997561634
#> 1597        b_wt -2.9569929355 0.4985178512
#> 1598        b_wt -2.9513669943 0.4972279108
#> 1599        b_wt -2.9457410531 0.4958870623
#> 1600        b_wt -2.9401151119 0.4944835519
#> 1601        b_wt -2.9344891707 0.4930309800
#> 1602        b_wt -2.9288632294 0.4915310189
#> 1603        b_wt -2.9232372882 0.4899847895
#> 1604        b_wt -2.9176113470 0.4883934713
#> 1605        b_wt -2.9119854058 0.4867552461
#> 1606        b_wt -2.9063594646 0.4850668026
#> 1607        b_wt -2.9007335234 0.4833382276
#> 1608        b_wt -2.8951075822 0.4815709679
#> 1609        b_wt -2.8894816409 0.4797665027
#> 1610        b_wt -2.8838556997 0.4779263408
#> 1611        b_wt -2.8782297585 0.4760470121
#> 1612        b_wt -2.8726038173 0.4741324357
#> 1613        b_wt -2.8669778761 0.4721880335
#> 1614        b_wt -2.8613519349 0.4702154032
#> 1615        b_wt -2.8557259937 0.4682161441
#> 1616        b_wt -2.8501000524 0.4661918547
#> 1617        b_wt -2.8444741112 0.4641390795
#> 1618        b_wt -2.8388481700 0.4620649644
#> 1619        b_wt -2.8332222288 0.4599716905
#> 1620        b_wt -2.8275962876 0.4578607735
#> 1621        b_wt -2.8219703464 0.4557337039
#> 1622        b_wt -2.8163444051 0.4535912214
#> 1623        b_wt -2.8107184639 0.4514333826
#> 1624        b_wt -2.8050925227 0.4492646401
#> 1625        b_wt -2.7994665815 0.4470862827
#> 1626        b_wt -2.7938406403 0.4448995560
#> 1627        b_wt -2.7882146991 0.4427056606
#> 1628        b_wt -2.7825887579 0.4405051318
#> 1629        b_wt -2.7769628166 0.4382996921
#> 1630        b_wt -2.7713368754 0.4360910556
#> 1631        b_wt -2.7657109342 0.4338801348
#> 1632        b_wt -2.7600849930 0.4316677896
#> 1633        b_wt -2.7544590518 0.4294548262
#> 1634        b_wt -2.7488331106 0.4272422733
#> 1635        b_wt -2.7432071694 0.4250309862
#> 1636        b_wt -2.7375812281 0.4228215009
#> 1637        b_wt -2.7319552869 0.4206143157
#> 1638        b_wt -2.7263293457 0.4184098775
#> 1639        b_wt -2.7207034045 0.4162087313
#> 1640        b_wt -2.7150774633 0.4140120016
#> 1641        b_wt -2.7094515221 0.4118191779
#> 1642        b_wt -2.7038255809 0.4096304329
#> 1643        b_wt -2.6981996396 0.4074458983
#> 1644        b_wt -2.6925736984 0.4052656651
#> 1645        b_wt -2.6869477572 0.4030903348
#> 1646        b_wt -2.6813218160 0.4009199650
#> 1647        b_wt -2.6756958748 0.3987538543
#> 1648        b_wt -2.6700699336 0.3965919044
#> 1649        b_wt -2.6644439924 0.3944339921
#> 1650        b_wt -2.6588180511 0.3922799713
#> 1651        b_wt -2.6531921099 0.3901303928
#> 1652        b_wt -2.6475661687 0.3879844064
#> 1653        b_wt -2.6419402275 0.3858415735
#> 1654        b_wt -2.6363142863 0.3837016655
#> 1655        b_wt -2.6306883451 0.3815644477
#> 1656        b_wt -2.6250624039 0.3794297349
#> 1657        b_wt -2.6194364626 0.3772976514
#> 1658        b_wt -2.6138105214 0.3751673616
#> 1659        b_wt -2.6081845802 0.3730386364
#> 1660        b_wt -2.6025586390 0.3709112552
#> 1661        b_wt -2.5969326978 0.3687850063
#> 1662        b_wt -2.5913067566 0.3666597622
#> 1663        b_wt -2.5856808154 0.3645352866
#> 1664        b_wt -2.5800548741 0.3624112880
#> 1665        b_wt -2.5744289329 0.3602876378
#> 1666        b_wt -2.5688029917 0.3581642256
#> 1667        b_wt -2.5631770505 0.3560409602
#> 1668        b_wt -2.5575511093 0.3539177743
#> 1669        b_wt -2.5519251681 0.3517946155
#> 1670        b_wt -2.5462992269 0.3496714859
#> 1671        b_wt -2.5406732856 0.3475484139
#> 1672        b_wt -2.5350473444 0.3454254496
#> 1673        b_wt -2.5294214032 0.3433026676
#> 1674        b_wt -2.5237954620 0.3411802956
#> 1675        b_wt -2.5181695208 0.3390584361
#> 1676        b_wt -2.5125435796 0.3369372580
#> 1677        b_wt -2.5069176383 0.3348169488
#> 1678        b_wt -2.5012916971 0.3326977141
#> 1679        b_wt -2.4956657559 0.3305799368
#> 1680        b_wt -2.4900398147 0.3284641307
#> 1681        b_wt -2.4844138735 0.3263503416
#> 1682        b_wt -2.4787879323 0.3242388615
#> 1683        b_wt -2.4731619911 0.3221299932
#> 1684        b_wt -2.4675360498 0.3200240501
#> 1685        b_wt -2.4619101086 0.3179220393
#> 1686        b_wt -2.4562841674 0.3158241645
#> 1687        b_wt -2.4506582262 0.3137304675
#> 1688        b_wt -2.4450322850 0.3116412941
#> 1689        b_wt -2.4394063438 0.3095569915
#> 1690        b_wt -2.4337804026 0.3074779069
#> 1691        b_wt -2.4281544613 0.3054060219
#> 1692        b_wt -2.4225285201 0.3033403228
#> 1693        b_wt -2.4169025789 0.3012811065
#> 1694        b_wt -2.4112766377 0.2992286934
#> 1695        b_wt -2.4056506965 0.2971833949
#> 1696        b_wt -2.4000247553 0.2951461370
#> 1697        b_wt -2.3943988141 0.2931182140
#> 1698        b_wt -2.3887728728 0.2910984531
#> 1699        b_wt -2.3831469316 0.2890870934
#> 1700        b_wt -2.3775209904 0.2870843582
#> 1701        b_wt -2.3718950492 0.2850904541
#> 1702        b_wt -2.3662691080 0.2831070983
#> 1703        b_wt -2.3606431668 0.2811339954
#> 1704        b_wt -2.3550172256 0.2791702999
#> 1705        b_wt -2.3493912843 0.2772161104
#> 1706        b_wt -2.3437653431 0.2752715053
#> 1707        b_wt -2.3381394019 0.2733365426
#> 1708        b_wt -2.3325134607 0.2714136778
#> 1709        b_wt -2.3268875195 0.2695006873
#> 1710        b_wt -2.3212615783 0.2675973258
#> 1711        b_wt -2.3156356371 0.2657035336
#> 1712        b_wt -2.3100096958 0.2638192313
#> 1713        b_wt -2.3043837546 0.2619449085
#> 1714        b_wt -2.2987578134 0.2600816930
#> 1715        b_wt -2.2931318722 0.2582274730
#> 1716        b_wt -2.2875059310 0.2563820657
#> 1717        b_wt -2.2818799898 0.2545452731
#> 1718        b_wt -2.2762540486 0.2527168824
#> 1719        b_wt -2.2706281073 0.2508978273
#> 1720        b_wt -2.2650021661 0.2490875036
#> 1721        b_wt -2.2593762249 0.2472846500
#> 1722        b_wt -2.2537502837 0.2454889897
#> 1723        b_wt -2.2481243425 0.2437002384
#> 1724        b_wt -2.2424984013 0.2419181058
#> 1725        b_wt -2.2368724601 0.2401436579
#> 1726        b_wt -2.2312465188 0.2383752364
#> 1727        b_wt -2.2256205776 0.2366123088
#> 1728        b_wt -2.2199946364 0.2348545742
#> 1729        b_wt -2.2143686952 0.2331017338
#> 1730        b_wt -2.2087427540 0.2313536944
#> 1731        b_wt -2.2031168128 0.2296107754
#> 1732        b_wt -2.1974908715 0.2278716836
#> 1733        b_wt -2.1918649303 0.2261361571
#> 1734        b_wt -2.1862389891 0.2244039444
#> 1735        b_wt -2.1806130479 0.2226748050
#> 1736        b_wt -2.1749871067 0.2209488474
#> 1737        b_wt -2.1693611655 0.2192257662
#> 1738        b_wt -2.1637352243 0.2175049992
#> 1739        b_wt -2.1581092830 0.2157863873
#> 1740        b_wt -2.1524833418 0.2140697877
#> 1741        b_wt -2.1468574006 0.2123550748
#> 1742        b_wt -2.1412314594 0.2106425034
#> 1743        b_wt -2.1356055182 0.2089316796
#> 1744        b_wt -2.1299795770 0.2072224724
#> 1745        b_wt -2.1243536358 0.2055148619
#> 1746        b_wt -2.1187276945 0.2038088474
#> 1747        b_wt -2.1131017533 0.2021045018
#> 1748        b_wt -2.1074758121 0.2004022427
#> 1749        b_wt -2.1018498709 0.1987017732
#> 1750        b_wt -2.0962239297 0.1970032003
#> 1751        b_wt -2.0905979885 0.1953066486
#> 1752        b_wt -2.0849720473 0.1936122600
#> 1753        b_wt -2.0793461060 0.1919205009
#> 1754        b_wt -2.0737201648 0.1902317539
#> 1755        b_wt -2.0680942236 0.1885458750
#> 1756        b_wt -2.0624682824 0.1868630948
#> 1757        b_wt -2.0568423412 0.1851836569
#> 1758        b_wt -2.0512164000 0.1835078169
#> 1759        b_wt -2.0455904588 0.1818367233
#> 1760        b_wt -2.0399645175 0.1801702658
#> 1761        b_wt -2.0343385763 0.1785084764
#> 1762        b_wt -2.0287126351 0.1768516627
#> 1763        b_wt -2.0230866939 0.1752001382
#> 1764        b_wt -2.0174607527 0.1735543234
#> 1765        b_wt -2.0118348115 0.1719161089
#> 1766        b_wt -2.0062088703 0.1702843928
#> 1767        b_wt -2.0005829290 0.1686595006
#> 1768        b_wt -1.9949569878 0.1670417556
#> 1769        b_wt -1.9893310466 0.1654314787
#> 1770        b_wt -1.9837051054 0.1638298297
#> 1771        b_wt -1.9780791642 0.1622378274
#> 1772        b_wt -1.9724532230 0.1606544405
#> 1773        b_wt -1.9668272818 0.1590799533
#> 1774        b_wt -1.9612013405 0.1575146400
#> 1775        b_wt -1.9555753993 0.1559587640
#> 1776        b_wt -1.9499494581 0.1544144614
#> 1777        b_wt -1.9443235169 0.1528810611
#> 1778        b_wt -1.9386975757 0.1513579440
#> 1779        b_wt -1.9330716345 0.1498452939
#> 1780        b_wt -1.9274456933 0.1483432781
#> 1781        b_wt -1.9218197520 0.1468520600
#> 1782        b_wt -1.9161938108 0.1453747516
#> 1783        b_wt -1.9105678696 0.1439085206
#> 1784        b_wt -1.9049419284 0.1424534276
#> 1785        b_wt -1.8993159872 0.1410095131
#> 1786        b_wt -1.8936900460 0.1395767971
#> 1787        b_wt -1.8880641047 0.1381562422
#> 1788        b_wt -1.8824381635 0.1367488899
#> 1789        b_wt -1.8768122223 0.1353525910
#> 1790        b_wt -1.8711862811 0.1339672454
#> 1791        b_wt -1.8655603399 0.1325927322
#> 1792        b_wt -1.8599343987 0.1312289098
#> 1793        b_wt -1.8543084575 0.1298773679
#> 1794        b_wt -1.8486825162 0.1285370497
#> 1795        b_wt -1.8430565750 0.1272066753
#> 1796        b_wt -1.8374306338 0.1258859919
#> 1797        b_wt -1.8318046926 0.1245747296
#> 1798        b_wt -1.8261787514 0.1232726015
#> 1799        b_wt -1.8205528102 0.1219814243
#> 1800        b_wt -1.8149268690 0.1206986217
#> 1801        b_wt -1.8093009277 0.1194237134
#> 1802        b_wt -1.8036749865 0.1181563342
#> 1803        b_wt -1.7980490453 0.1168961090
#> 1804        b_wt -1.7924231041 0.1156430910
#> 1805        b_wt -1.7867971629 0.1143975011
#> 1806        b_wt -1.7811712217 0.1131575825
#> 1807        b_wt -1.7755452805 0.1119229274
#> 1808        b_wt -1.7699193392 0.1106931267
#> 1809        b_wt -1.7642933980 0.1094677720
#> 1810        b_wt -1.7586674568 0.1082469868
#> 1811        b_wt -1.7530415156 0.1070300003
#> 1812        b_wt -1.7474155744 0.1058159699
#> 1813        b_wt -1.7417896332 0.1046045191
#> 1814        b_wt -1.7361636920 0.1033952820
#> 1815        b_wt -1.7305377507 0.1021879047
#> 1816        b_wt -1.7249118095 0.1009822862
#> 1817        b_wt -1.7192858683 0.0997776937
#> 1818        b_wt -1.7136599271 0.0985738085
#> 1819        b_wt -1.7080339859 0.0973703749
#> 1820        b_wt -1.7024080447 0.0961671589
#> 1821        b_wt -1.6967821035 0.0949639356
#> 1822        b_wt -1.6911561622 0.0937604196
#> 1823        b_wt -1.6855302210 0.0925564958
#> 1824        b_wt -1.6799042798 0.0913520754
#> 1825        b_wt -1.6742783386 0.0901470977
#> 1826        b_wt -1.6686523974 0.0889415310
#> 1827        b_wt -1.6630264562 0.0877353017
#> 1828        b_wt -1.6574005150 0.0865284997
#> 1829        b_wt -1.6517745737 0.0853213131
#> 1830        b_wt -1.6461486325 0.0841138843
#> 1831        b_wt -1.6405226913 0.0829063865
#> 1832        b_wt -1.6348967501 0.0816990233
#> 1833        b_wt -1.6292708089 0.0804922237
#> 1834        b_wt -1.6236448677 0.0792863289
#> 1835        b_wt -1.6180189265 0.0780816611
#> 1836        b_wt -1.6123929852 0.0768785905
#> 1837        b_wt -1.6067670440 0.0756775136
#> 1838        b_wt -1.6011411028 0.0744789737
#> 1839        b_wt -1.5955151616 0.0732842216
#> 1840        b_wt -1.5898892204 0.0720931987
#> 1841        b_wt -1.5842632792 0.0709064302
#> 1842        b_wt -1.5786373379 0.0697244587
#> 1843        b_wt -1.5730113967 0.0685478423
#> 1844        b_wt -1.5673854555 0.0673780068
#> 1845        b_wt -1.5617595143 0.0662160211
#> 1846        b_wt -1.5561335731 0.0650616098
#> 1847        b_wt -1.5505076319 0.0639153871
#> 1848        b_wt -1.5448816907 0.0627779696
#> 1849        b_wt -1.5392557494 0.0616499757
#> 1850        b_wt -1.5336298082 0.0605343423
#> 1851        b_wt -1.5280038670 0.0594304431
#> 1852        b_wt -1.5223779258 0.0583382456
#> 1853        b_wt -1.5167519846 0.0572583350
#> 1854        b_wt -1.5111260434 0.0561912842
#> 1855        b_wt -1.5055001022 0.0551379879
#> 1856        b_wt -1.4998741609 0.0541023671
#> 1857        b_wt -1.4942482197 0.0530815856
#> 1858        b_wt -1.4886222785 0.0520761116
#> 1859        b_wt -1.4829963373 0.0510863895
#> 1860        b_wt -1.4773703961 0.0501128390
#> 1861        b_wt -1.4717444549 0.0491577144
#> 1862        b_wt -1.4661185137 0.0482224539
#> 1863        b_wt -1.4604925724 0.0473046444
#> 1864        b_wt -1.4548666312 0.0464045416
#> 1865        b_wt -1.4492406900 0.0455223699
#> 1866        b_wt -1.4436147488 0.0446583226
#> 1867        b_wt -1.4379888076 0.0438161403
#> 1868        b_wt -1.4323628664 0.0429938270
#> 1869        b_wt -1.4267369252 0.0421900088
#> 1870        b_wt -1.4211109839 0.0414046946
#> 1871        b_wt -1.4154850427 0.0406378624
#> 1872        b_wt -1.4098591015 0.0398896045
#> 1873        b_wt -1.4042331603 0.0391644517
#> 1874        b_wt -1.3986072191 0.0384573879
#> 1875        b_wt -1.3929812779 0.0377682235
#> 1876        b_wt -1.3873553367 0.0370967436
#> 1877        b_wt -1.3817293954 0.0364427086
#> 1878        b_wt -1.3761034542 0.0358074134
#> 1879        b_wt -1.3704775130 0.0351918267
#> 1880        b_wt -1.3648515718 0.0345925549
#> 1881        b_wt -1.3592256306 0.0340092425
#> 1882        b_wt -1.3535996894 0.0334415190
#> 1883        b_wt -1.3479737482 0.0328890004
#> 1884        b_wt -1.3423478069 0.0323538111
#> 1885        b_wt -1.3367218657 0.0318340716
#> 1886        b_wt -1.3310959245 0.0313279877
#> 1887        b_wt -1.3254699833 0.0308351278
#> 1888        b_wt -1.3198440421 0.0303550575
#> 1889        b_wt -1.3142181009 0.0298873406
#> 1890        b_wt -1.3085921597 0.0294344831
#> 1891        b_wt -1.3029662184 0.0289928789
#> 1892        b_wt -1.2973402772 0.0285620123
#> 1893        b_wt -1.2917143360 0.0281414650
#> 1894        b_wt -1.2860883948 0.0277308252
#> 1895        b_wt -1.2804624536 0.0273303813
#> 1896        b_wt -1.2748365124 0.0269405999
#> 1897        b_wt -1.2692105711 0.0265592810
#> 1898        b_wt -1.2635846299 0.0261860711
#> 1899        b_wt -1.2579586887 0.0258206290
#> 1900        b_wt -1.2523327475 0.0254626261
#> 1901        b_wt -1.2467068063 0.0251128308
#> 1902        b_wt -1.2410808651 0.0247704216
#> 1903        b_wt -1.2354549239 0.0244343678
#> 1904        b_wt -1.2298289826 0.0241044192
#> 1905        b_wt -1.2242030414 0.0237803392
#> 1906        b_wt -1.2185771002 0.0234619050
#> 1907        b_wt -1.2129511590 0.0231501770
#> 1908        b_wt -1.2073252178 0.0228436931
#> 1909        b_wt -1.2016992766 0.0225421649
#> 1910        b_wt -1.1960733354 0.0222454414
#> 1911        b_wt -1.1904473941 0.0219533824
#> 1912        b_wt -1.1848214529 0.0216661284
#> 1913        b_wt -1.1791955117 0.0213841602
#> 1914        b_wt -1.1735695705 0.0211064342
#> 1915        b_wt -1.1679436293 0.0208328620
#> 1916        b_wt -1.1623176881 0.0205633618
#> 1917        b_wt -1.1566917469 0.0202978578
#> 1918        b_wt -1.1510658056 0.0200368430
#> 1919        b_wt -1.1454398644 0.0197801336
#> 1920        b_wt -1.1398139232 0.0195271876
#> 1921        b_wt -1.1341879820 0.0192779519
#> 1922        b_wt -1.1285620408 0.0190323749
#> 1923        b_wt -1.1229360996 0.0187904066
#> 1924        b_wt -1.1173101584 0.0185528048
#> 1925        b_wt -1.1116842171 0.0183188148
#> 1926        b_wt -1.1060582759 0.0180882472
#> 1927        b_wt -1.1004323347 0.0178610496
#> 1928        b_wt -1.0948063935 0.0176371681
#> 1929        b_wt -1.0891804523 0.0174166929
#> 1930        b_wt -1.0835545111 0.0172001069
#> 1931        b_wt -1.0779285699 0.0169866112
#> 1932        b_wt -1.0723026286 0.0167761375
#> 1933        b_wt -1.0666766874 0.0165686147
#> 1934        b_wt -1.0610507462 0.0163639685
#> 1935        b_wt -1.0554248050 0.0161624724
#> 1936        b_wt -1.0497988638 0.0159640211
#> 1937        b_wt -1.0441729226 0.0157681368
#> 1938        b_wt -1.0385469814 0.0155747289
#> 1939        b_wt -1.0329210401 0.0153837044
#> 1940        b_wt -1.0272950989 0.0151949680
#> 1941        b_wt -1.0216691577 0.0150088600
#> 1942        b_wt -1.0160432165 0.0148248850
#> 1943        b_wt -1.0104172753 0.0146428217
#> 1944        b_wt -1.0047913341 0.0144625662
#> 1945        b_wt -0.9991653929 0.0142840143
#> 1946        b_wt -0.9935394516 0.0141071056
#> 1947        b_wt -0.9879135104 0.0139319977
#> 1948        b_wt -0.9822875692 0.0137582035
#> 1949        b_wt -0.9766616280 0.0135856211
#> 1950        b_wt -0.9710356868 0.0134141503
#> 1951        b_wt -0.9654097456 0.0132436927
#> 1952        b_wt -0.9597838043 0.0130742412
#> 1953        b_wt -0.9541578631 0.0129056859
#> 1954        b_wt -0.9485319219 0.0127377998
#> 1955        b_wt -0.9429059807 0.0125704999
#> 1956        b_wt -0.9372800395 0.0124037065
#> 1957        b_wt -0.9316540983 0.0122373435
#> 1958        b_wt -0.9260281571 0.0120713896
#> 1959        b_wt -0.9204022158 0.0119057067
#> 1960        b_wt -0.9147762746 0.0117402150
#> 1961        b_wt -0.9091503334 0.0115748630
#> 1962        b_wt -0.9035243922 0.0114096040
#> 1963        b_wt -0.8978984510 0.0112443962
#> 1964        b_wt -0.8922725098 0.0110791932
#> 1965        b_wt -0.8866465686 0.0109139586
#> 1966        b_wt -0.8810206273 0.0107486740
#> 1967        b_wt -0.8753946861 0.0105833260
#> 1968        b_wt -0.8697687449 0.0104179060
#> 1969        b_wt -0.8641428037 0.0102524035
#> 1970        b_wt -0.8585168625 0.0100868211
#> 1971        b_wt -0.8528909213 0.0099211859
#> 1972        b_wt -0.8472649801 0.0097555164
#> 1973        b_wt -0.8416390388 0.0095898356
#> 1974        b_wt -0.8360130976 0.0094241706
#> 1975        b_wt -0.8303871564 0.0092585733
#> 1976        b_wt -0.8247612152 0.0090930962
#> 1977        b_wt -0.8191352740 0.0089277798
#> 1978        b_wt -0.8135093328 0.0087626727
#> 1979        b_wt -0.8078833916 0.0085978265
#> 1980        b_wt -0.8022574503 0.0084332961
#> 1981        b_wt -0.7966315091 0.0082692693
#> 1982        b_wt -0.7910055679 0.0081057256
#> 1983        b_wt -0.7853796267 0.0079427304
#> 1984        b_wt -0.7797536855 0.0077803514
#> 1985        b_wt -0.7741277443 0.0076186577
#> 1986        b_wt -0.7685018031 0.0074577928
#> 1987        b_wt -0.7628758618 0.0072979530
#> 1988        b_wt -0.7572499206 0.0071390692
#> 1989        b_wt -0.7516239794 0.0069812155
#> 1990        b_wt -0.7459980382 0.0068244659
#> 1991        b_wt -0.7403720970 0.0066688946
#> 1992        b_wt -0.7347461558 0.0065148077
#> 1993        b_wt -0.7291202146 0.0063622295
#> 1994        b_wt -0.7234942733 0.0062111022
#> 1995        b_wt -0.7178683321 0.0060614961
#> 1996        b_wt -0.7122423909 0.0059134803
#> 1997        b_wt -0.7066164497 0.0057671223
#> 1998        b_wt -0.7009905085 0.0056229547
#> 1999        b_wt -0.6953645673 0.0054806491
#> 2000        b_wt -0.6897386261 0.0053402353
#> 2001        b_wt -0.6841126848 0.0052017704
#> 2002        b_wt -0.6784867436 0.0050653088
#> 2003        b_wt -0.6728608024 0.0049310487
#> 2004        b_wt -0.6672348612 0.0047993455
#> 2005        b_wt -0.6616089200 0.0046698242
#> 2006        b_wt -0.6559829788 0.0045425244
#> 2007        b_wt -0.6503570375 0.0044174831
#> 2008        b_wt -0.6447310963 0.0042947342
#> 2009        b_wt -0.6391051551 0.0041746724
#> 2010        b_wt -0.6334792139 0.0040572522
#> 2011        b_wt -0.6278532727 0.0039422196
#> 2012        b_wt -0.6222273315 0.0038295913
#> 2013        b_wt -0.6166013903 0.0037193809
#> 2014        b_wt -0.6109754490 0.0036115989
#> 2015        b_wt -0.6053495078 0.0035068353
#> 2016        b_wt -0.5997235666 0.0034045883
#> 2017        b_wt -0.5940976254 0.0033047750
#> 2018        b_wt -0.5884716842 0.0032073887
#> 2019        b_wt -0.5828457430 0.0031124195
#> 2020        b_wt -0.5772198018 0.0030199806
#> 2021        b_wt -0.5715938605 0.0029304408
#> 2022        b_wt -0.5659679193 0.0028432523
#> 2023        b_wt -0.5603419781 0.0027583895
#> 2024        b_wt -0.5547160369 0.0026758247
#> 2025        b_wt -0.5490900957 0.0025955276
#> 2026        b_wt -0.5434641545 0.0025177664
#> 2027        b_wt -0.5378382133 0.0024424735
#> 2028        b_wt -0.5322122720 0.0023693119
#> 2029        b_wt -0.5265863308 0.0022982396
#> 2030        b_wt -0.5209603896 0.0022292132
#> 2031        b_wt -0.5153344484 0.0021621877
#> 2032        b_wt -0.5097085072 0.0020975345
#> 2033        b_wt -0.5040825660 0.0020348435
#> 2034        b_wt -0.4984566248 0.0019739712
#> 2035        b_wt -0.4928306835 0.0019148664
#> 2036        b_wt -0.4872047423 0.0018574773
#> 2037        b_wt -0.4815788011 0.0018018107
#> 2038        b_wt -0.4759528599 0.0017481046
#> 2039        b_wt -0.4703269187 0.0016959172
#> 2040        b_wt -0.4647009775 0.0016451960
#> 2041        b_wt -0.4590750363 0.0015958889
#> 2042        b_wt -0.4534490950 0.0015479440
#> 2043        b_wt -0.4478231538 0.0015014611
#> 2044        b_wt -0.4421972126 0.0014563964
#> 2045        b_wt -0.4365712714 0.0014125077
#> 2046        b_wt -0.4309453302 0.0013697482
#> 2047        b_wt -0.4253193890 0.0013280722
#> 2048        b_wt -0.4196934478 0.0012874353
#> 2049       b_cyl -2.8967932590 0.0059334919
#> 2050       b_cyl -2.8934907910 0.0061466338
#> 2051       b_cyl -2.8901883230 0.0063635474
#> 2052       b_cyl -2.8868858549 0.0065845566
#> 2053       b_cyl -2.8835833869 0.0068099660
#> 2054       b_cyl -2.8802809189 0.0070391471
#> 2055       b_cyl -2.8769784509 0.0072721076
#> 2056       b_cyl -2.8736759828 0.0075088582
#> 2057       b_cyl -2.8703735148 0.0077494125
#> 2058       b_cyl -2.8670710468 0.0079942048
#> 2059       b_cyl -2.8637685788 0.0082434153
#> 2060       b_cyl -2.8604661107 0.0084965097
#> 2061       b_cyl -2.8571636427 0.0087535178
#> 2062       b_cyl -2.8538611747 0.0090144726
#> 2063       b_cyl -2.8505587067 0.0092794094
#> 2064       b_cyl -2.8472562386 0.0095488966
#> 2065       b_cyl -2.8439537706 0.0098229823
#> 2066       b_cyl -2.8406513026 0.0101012080
#> 2067       b_cyl -2.8373488346 0.0103836231
#> 2068       b_cyl -2.8340463665 0.0106702794
#> 2069       b_cyl -2.8307438985 0.0109612307
#> 2070       b_cyl -2.8274414305 0.0112572038
#> 2071       b_cyl -2.8241389625 0.0115580833
#> 2072       b_cyl -2.8208364944 0.0118634767
#> 2073       b_cyl -2.8175340264 0.0121734481
#> 2074       b_cyl -2.8142315584 0.0124880637
#> 2075       b_cyl -2.8109290904 0.0128073910
#> 2076       b_cyl -2.8076266223 0.0131323423
#> 2077       b_cyl -2.8043241543 0.0134625952
#> 2078       b_cyl -2.8010216863 0.0137978260
#> 2079       b_cyl -2.7977192182 0.0141381116
#> 2080       b_cyl -2.7944167502 0.0144835305
#> 2081       b_cyl -2.7911142822 0.0148341629
#> 2082       b_cyl -2.7878118142 0.0151911422
#> 2083       b_cyl -2.7845093461 0.0155538909
#> 2084       b_cyl -2.7812068781 0.0159221697
#> 2085       b_cyl -2.7779044101 0.0162960702
#> 2086       b_cyl -2.7746019421 0.0166756867
#> 2087       b_cyl -2.7712994740 0.0170611159
#> 2088       b_cyl -2.7679970060 0.0174537628
#> 2089       b_cyl -2.7646945380 0.0178527391
#> 2090       b_cyl -2.7613920700 0.0182579127
#> 2091       b_cyl -2.7580896019 0.0186693960
#> 2092       b_cyl -2.7547871339 0.0190873043
#> 2093       b_cyl -2.7514846659 0.0195117560
#> 2094       b_cyl -2.7481821979 0.0199444954
#> 2095       b_cyl -2.7448797298 0.0203842448
#> 2096       b_cyl -2.7415772618 0.0208310070
#> 2097       b_cyl -2.7382747938 0.0212849173
#> 2098       b_cyl -2.7349723258 0.0217461138
#> 2099       b_cyl -2.7316698577 0.0222147520
#> 2100       b_cyl -2.7283673897 0.0226929429
#> 2101       b_cyl -2.7250649217 0.0231789528
#> 2102       b_cyl -2.7217624537 0.0236729318
#> 2103       b_cyl -2.7184599856 0.0241750306
#> 2104       b_cyl -2.7151575176 0.0246854011
#> 2105       b_cyl -2.7118550496 0.0252043951
#> 2106       b_cyl -2.7085525816 0.0257340252
#> 2107       b_cyl -2.7052501135 0.0262724840
#> 2108       b_cyl -2.7019476455 0.0268199190
#> 2109       b_cyl -2.6986451775 0.0273764744
#> 2110       b_cyl -2.6953427095 0.0279422914
#> 2111       b_cyl -2.6920402414 0.0285179331
#> 2112       b_cyl -2.6887377734 0.0291051766
#> 2113       b_cyl -2.6854353054 0.0297021562
#> 2114       b_cyl -2.6821328373 0.0303089830
#> 2115       b_cyl -2.6788303693 0.0309257599
#> 2116       b_cyl -2.6755279013 0.0315525817
#> 2117       b_cyl -2.6722254333 0.0321902059
#> 2118       b_cyl -2.6689229652 0.0328399998
#> 2119       b_cyl -2.6656204972 0.0335000889
#> 2120       b_cyl -2.6623180292 0.0341705090
#> 2121       b_cyl -2.6590155612 0.0348512831
#> 2122       b_cyl -2.6557130931 0.0355424212
#> 2123       b_cyl -2.6524106251 0.0362448169
#> 2124       b_cyl -2.6491081571 0.0369592706
#> 2125       b_cyl -2.6458056891 0.0376839833
#> 2126       b_cyl -2.6425032210 0.0384188848
#> 2127       b_cyl -2.6392007530 0.0391638893
#> 2128       b_cyl -2.6358982850 0.0399188963
#> 2129       b_cyl -2.6325958170 0.0406848357
#> 2130       b_cyl -2.6292933489 0.0414618724
#> 2131       b_cyl -2.6259908809 0.0422483846
#> 2132       b_cyl -2.6226884129 0.0430441882
#> 2133       b_cyl -2.6193859449 0.0438490851
#> 2134       b_cyl -2.6160834768 0.0446628637
#> 2135       b_cyl -2.6127810088 0.0454863714
#> 2136       b_cyl -2.6094785408 0.0463192062
#> 2137       b_cyl -2.6061760728 0.0471600194
#> 2138       b_cyl -2.6028736047 0.0480085378
#> 2139       b_cyl -2.5995711367 0.0488644806
#> 2140       b_cyl -2.5962686687 0.0497275599
#> 2141       b_cyl -2.5929662007 0.0505984427
#> 2142       b_cyl -2.5896637326 0.0514763710
#> 2143       b_cyl -2.5863612646 0.0523603321
#> 2144       b_cyl -2.5830587966 0.0532500243
#> 2145       b_cyl -2.5797563286 0.0541451482
#> 2146       b_cyl -2.5764538605 0.0550454082
#> 2147       b_cyl -2.5731513925 0.0559512719
#> 2148       b_cyl -2.5698489245 0.0568619151
#> 2149       b_cyl -2.5665464564 0.0577766818
#> 2150       b_cyl -2.5632439884 0.0586953288
#> 2151       b_cyl -2.5599415204 0.0596176279
#> 2152       b_cyl -2.5566390524 0.0605433675
#> 2153       b_cyl -2.5533365843 0.0614729330
#> 2154       b_cyl -2.5500341163 0.0624056856
#> 2155       b_cyl -2.5467316483 0.0633413117
#> 2156       b_cyl -2.5434291803 0.0642797235
#> 2157       b_cyl -2.5401267122 0.0652208604
#> 2158       b_cyl -2.5368242442 0.0661646903
#> 2159       b_cyl -2.5335217762 0.0671118089
#> 2160       b_cyl -2.5302193082 0.0680618308
#> 2161       b_cyl -2.5269168401 0.0690147601
#> 2162       b_cyl -2.5236143721 0.0699707468
#> 2163       b_cyl -2.5203119041 0.0709299768
#> 2164       b_cyl -2.5170094361 0.0718926722
#> 2165       b_cyl -2.5137069680 0.0728601002
#> 2166       b_cyl -2.5104045000 0.0738318905
#> 2167       b_cyl -2.5071020320 0.0748083605
#> 2168       b_cyl -2.5037995640 0.0757899400
#> 2169       b_cyl -2.5004970959 0.0767770956
#> 2170       b_cyl -2.4971946279 0.0777703417
#> 2171       b_cyl -2.4938921599 0.0787721497
#> 2172       b_cyl -2.4905896919 0.0797816039
#> 2173       b_cyl -2.4872872238 0.0807993628
#> 2174       b_cyl -2.4839847558 0.0818261148
#> 2175       b_cyl -2.4806822878 0.0828625771
#> 2176       b_cyl -2.4773798198 0.0839097583
#> 2177       b_cyl -2.4740773517 0.0849711507
#> 2178       b_cyl -2.4707748837 0.0860451441
#> 2179       b_cyl -2.4674724157 0.0871325798
#> 2180       b_cyl -2.4641699477 0.0882343132
#> 2181       b_cyl -2.4608674796 0.0893512114
#> 2182       b_cyl -2.4575650116 0.0904849142
#> 2183       b_cyl -2.4542625436 0.0916395513
#> 2184       b_cyl -2.4509600756 0.0928126109
#> 2185       b_cyl -2.4476576075 0.0940049751
#> 2186       b_cyl -2.4443551395 0.0952175177
#> 2187       b_cyl -2.4410526715 0.0964511031
#> 2188       b_cyl -2.4377502034 0.0977080916
#> 2189       b_cyl -2.4344477354 0.0989925358
#> 2190       b_cyl -2.4311452674 0.1003010463
#> 2191       b_cyl -2.4278427994 0.1016343791
#> 2192       b_cyl -2.4245403313 0.1029932616
#> 2193       b_cyl -2.4212378633 0.1043783910
#> 2194       b_cyl -2.4179353953 0.1057928402
#> 2195       b_cyl -2.4146329273 0.1072397727
#> 2196       b_cyl -2.4113304592 0.1087151708
#> 2197       b_cyl -2.4080279912 0.1102195274
#> 2198       b_cyl -2.4047255232 0.1117532943
#> 2199       b_cyl -2.4014230552 0.1133168812
#> 2200       b_cyl -2.3981205871 0.1149139586
#> 2201       b_cyl -2.3948181191 0.1165461729
#> 2202       b_cyl -2.3915156511 0.1182092811
#> 2203       b_cyl -2.3882131831 0.1199034557
#> 2204       b_cyl -2.3849107150 0.1216288279
#> 2205       b_cyl -2.3816082470 0.1233854889
#> 2206       b_cyl -2.3783057790 0.1251775345
#> 2207       b_cyl -2.3750033110 0.1270048521
#> 2208       b_cyl -2.3717008429 0.1288634145
#> 2209       b_cyl -2.3683983749 0.1307531189
#> 2210       b_cyl -2.3650959069 0.1326738357
#> 2211       b_cyl -2.3617934389 0.1346254090
#> 2212       b_cyl -2.3584909708 0.1366122232
#> 2213       b_cyl -2.3551885028 0.1386325701
#> 2214       b_cyl -2.3518860348 0.1406829979
#> 2215       b_cyl -2.3485835668 0.1427632606
#> 2216       b_cyl -2.3452810987 0.1448731057
#> 2217       b_cyl -2.3419786307 0.1470122762
#> 2218       b_cyl -2.3386761627 0.1491854329
#> 2219       b_cyl -2.3353736947 0.1513896557
#> 2220       b_cyl -2.3320712266 0.1536222752
#> 2221       b_cyl -2.3287687586 0.1558830602
#> 2222       b_cyl -2.3254662906 0.1581717918
#> 2223       b_cyl -2.3221638225 0.1604882647
#> 2224       b_cyl -2.3188613545 0.1628375537
#> 2225       b_cyl -2.3155588865 0.1652158465
#> 2226       b_cyl -2.3122564185 0.1676212998
#> 2227       b_cyl -2.3089539504 0.1700538072
#> 2228       b_cyl -2.3056514824 0.1725132821
#> 2229       b_cyl -2.3023490144 0.1749996569
#> 2230       b_cyl -2.2990465464 0.1775186288
#> 2231       b_cyl -2.2957440783 0.1800655347
#> 2232       b_cyl -2.2924416103 0.1826392799
#> 2233       b_cyl -2.2891391423 0.1852398877
#> 2234       b_cyl -2.2858366743 0.1878673927
#> 2235       b_cyl -2.2825342062 0.1905218385
#> 2236       b_cyl -2.2792317382 0.1932096525
#> 2237       b_cyl -2.2759292702 0.1959250913
#> 2238       b_cyl -2.2726268022 0.1986676679
#> 2239       b_cyl -2.2693243341 0.2014374265
#> 2240       b_cyl -2.2660218661 0.2042344013
#> 2241       b_cyl -2.2627193981 0.2070586503
#> 2242       b_cyl -2.2594169301 0.2099170492
#> 2243       b_cyl -2.2561144620 0.2128026311
#> 2244       b_cyl -2.2528119940 0.2157153183
#> 2245       b_cyl -2.2495095260 0.2186550022
#> 2246       b_cyl -2.2462070580 0.2216215411
#> 2247       b_cyl -2.2429045899 0.2246153535
#> 2248       b_cyl -2.2396021219 0.2276416525
#> 2249       b_cyl -2.2362996539 0.2306939108
#> 2250       b_cyl -2.2329971859 0.2337717637
#> 2251       b_cyl -2.2296947178 0.2368748008
#> 2252       b_cyl -2.2263922498 0.2400025666
#> 2253       b_cyl -2.2230897818 0.2431555854
#> 2254       b_cyl -2.2197873138 0.2463369762
#> 2255       b_cyl -2.2164848457 0.2495409586
#> 2256       b_cyl -2.2131823777 0.2527668263
#> 2257       b_cyl -2.2098799097 0.2560138325
#> 2258       b_cyl -2.2065774416 0.2592811915
#> 2259       b_cyl -2.2032749736 0.2625692698
#> 2260       b_cyl -2.1999725056 0.2658791023
#> 2261       b_cyl -2.1966700376 0.2692060612
#> 2262       b_cyl -2.1933675695 0.2725492014
#> 2263       b_cyl -2.1900651015 0.2759075631
#> 2264       b_cyl -2.1867626335 0.2792801750
#> 2265       b_cyl -2.1834601655 0.2826670806
#> 2266       b_cyl -2.1801576974 0.2860678335
#> 2267       b_cyl -2.1768552294 0.2894792503
#> 2268       b_cyl -2.1735527614 0.2929003915
#> 2269       b_cyl -2.1702502934 0.2963303418
#> 2270       b_cyl -2.1669478253 0.2997682142
#> 2271       b_cyl -2.1636453573 0.3032137890
#> 2272       b_cyl -2.1603428893 0.3066661408
#> 2273       b_cyl -2.1570404213 0.3101235507
#> 2274       b_cyl -2.1537379532 0.3135853860
#> 2275       b_cyl -2.1504354852 0.3170510757
#> 2276       b_cyl -2.1471330172 0.3205201130
#> 2277       b_cyl -2.1438305492 0.3239923731
#> 2278       b_cyl -2.1405280811 0.3274673825
#> 2279       b_cyl -2.1372256131 0.3309446085
#> 2280       b_cyl -2.1339231451 0.3344239599
#> 2281       b_cyl -2.1306206771 0.3379054261
#> 2282       b_cyl -2.1273182090 0.3413890771
#> 2283       b_cyl -2.1240157410 0.3448754848
#> 2284       b_cyl -2.1207132730 0.3483649841
#> 2285       b_cyl -2.1174108050 0.3518577562
#> 2286       b_cyl -2.1141083369 0.3553543078
#> 2287       b_cyl -2.1108058689 0.3588552163
#> 2288       b_cyl -2.1075034009 0.3623611276
#> 2289       b_cyl -2.1042009329 0.3658739529
#> 2290       b_cyl -2.1008984648 0.3693943660
#> 2291       b_cyl -2.0975959968 0.3729227702
#> 2292       b_cyl -2.0942935288 0.3764601022
#> 2293       b_cyl -2.0909910607 0.3800073318
#> 2294       b_cyl -2.0876885927 0.3835654571
#> 2295       b_cyl -2.0843861247 0.3871381153
#> 2296       b_cyl -2.0810836567 0.3907252057
#> 2297       b_cyl -2.0777811886 0.3943269985
#> 2298       b_cyl -2.0744787206 0.3979445134
#> 2299       b_cyl -2.0711762526 0.4015787501
#> 2300       b_cyl -2.0678737846 0.4052306833
#> 2301       b_cyl -2.0645713165 0.4089055409
#> 2302       b_cyl -2.0612688485 0.4126012639
#> 2303       b_cyl -2.0579663805 0.4163178362
#> 2304       b_cyl -2.0546639125 0.4200559605
#> 2305       b_cyl -2.0513614444 0.4238162714
#> 2306       b_cyl -2.0480589764 0.4275993315
#> 2307       b_cyl -2.0447565084 0.4314112150
#> 2308       b_cyl -2.0414540404 0.4352473445
#> 2309       b_cyl -2.0381515723 0.4391074288
#> 2310       b_cyl -2.0348491043 0.4429915576
#> 2311       b_cyl -2.0315466363 0.4468997283
#> 2312       b_cyl -2.0282441683 0.4508318702
#> 2313       b_cyl -2.0249417002 0.4547936489
#> 2314       b_cyl -2.0216392322 0.4587785516
#> 2315       b_cyl -2.0183367642 0.4627860676
#> 2316       b_cyl -2.0150342962 0.4668156014
#> 2317       b_cyl -2.0117318281 0.4708664755
#> 2318       b_cyl -2.0084293601 0.4749383649
#> 2319       b_cyl -2.0051268921 0.4790341604
#> 2320       b_cyl -2.0018244241 0.4831480714
#> 2321       b_cyl -1.9985219560 0.4872790456
#> 2322       b_cyl -1.9952194880 0.4914259835
#> 2323       b_cyl -1.9919170200 0.4955877431
#> 2324       b_cyl -1.9886145520 0.4997636642
#> 2325       b_cyl -1.9853120839 0.5039540419
#> 2326       b_cyl -1.9820096159 0.5081547500
#> 2327       b_cyl -1.9787071479 0.5123645363
#> 2328       b_cyl -1.9754046798 0.5165821539
#> 2329       b_cyl -1.9721022118 0.5208063658
#> 2330       b_cyl -1.9687997438 0.5250361889
#> 2331       b_cyl -1.9654972758 0.5292704022
#> 2332       b_cyl -1.9621948077 0.5335068914
#> 2333       b_cyl -1.9588923397 0.5377445937
#> 2334       b_cyl -1.9555898717 0.5419824970
#> 2335       b_cyl -1.9522874037 0.5462196429
#> 2336       b_cyl -1.9489849356 0.5504548881
#> 2337       b_cyl -1.9456824676 0.5546868416
#> 2338       b_cyl -1.9423799996 0.5589151325
#> 2339       b_cyl -1.9390775316 0.5631391653
#> 2340       b_cyl -1.9357750635 0.5673584164
#> 2341       b_cyl -1.9324725955 0.5715724339
#> 2342       b_cyl -1.9291701275 0.5757801958
#> 2343       b_cyl -1.9258676595 0.5799810784
#> 2344       b_cyl -1.9225651914 0.5841757883
#> 2345       b_cyl -1.9192627234 0.5883642601
#> 2346       b_cyl -1.9159602554 0.5925464901
#> 2347       b_cyl -1.9126577874 0.5967225340
#> 2348       b_cyl -1.9093553193 0.6008917625
#> 2349       b_cyl -1.9060528513 0.6050544878
#> 2350       b_cyl -1.9027503833 0.6092117487
#> 2351       b_cyl -1.8994479153 0.6133638498
#> 2352       b_cyl -1.8961454472 0.6175111231
#> 2353       b_cyl -1.8928429792 0.6216539241
#> 2354       b_cyl -1.8895405112 0.6257921040
#> 2355       b_cyl -1.8862380432 0.6299264187
#> 2356       b_cyl -1.8829355751 0.6340576802
#> 2357       b_cyl -1.8796331071 0.6381862653
#> 2358       b_cyl -1.8763306391 0.6423125358
#> 2359       b_cyl -1.8730281711 0.6464368345
#> 2360       b_cyl -1.8697257030 0.6505592685
#> 2361       b_cyl -1.8664232350 0.6546803533
#> 2362       b_cyl -1.8631207670 0.6588004159
#> 2363       b_cyl -1.8598182989 0.6629196035
#> 2364       b_cyl -1.8565158309 0.6670380154
#> 2365       b_cyl -1.8532133629 0.6711557007
#> 2366       b_cyl -1.8499108949 0.6752724814
#> 2367       b_cyl -1.8466084268 0.6793882964
#> 2368       b_cyl -1.8433059588 0.6835029858
#> 2369       b_cyl -1.8400034908 0.6876162819
#> 2370       b_cyl -1.8367010228 0.6917278579
#> 2371       b_cyl -1.8333985547 0.6958373276
#> 2372       b_cyl -1.8300960867 0.6999435069
#> 2373       b_cyl -1.8267936187 0.7040460835
#> 2374       b_cyl -1.8234911507 0.7081445278
#> 2375       b_cyl -1.8201886826 0.7122381470
#> 2376       b_cyl -1.8168862146 0.7163262001
#> 2377       b_cyl -1.8135837466 0.7204079000
#> 2378       b_cyl -1.8102812786 0.7244803770
#> 2379       b_cyl -1.8069788105 0.7285439928
#> 2380       b_cyl -1.8036763425 0.7325979484
#> 2381       b_cyl -1.8003738745 0.7366412666
#> 2382       b_cyl -1.7970714065 0.7406729482
#> 2383       b_cyl -1.7937689384 0.7446919643
#> 2384       b_cyl -1.7904664704 0.7486933517
#> 2385       b_cyl -1.7871640024 0.7526792779
#> 2386       b_cyl -1.7838615344 0.7566486890
#> 2387       b_cyl -1.7805590663 0.7606005343
#> 2388       b_cyl -1.7772565983 0.7645337688
#> 2389       b_cyl -1.7739541303 0.7684468924
#> 2390       b_cyl -1.7706516623 0.7723340609
#> 2391       b_cyl -1.7673491942 0.7761988807
#> 2392       b_cyl -1.7640467262 0.7800403881
#> 2393       b_cyl -1.7607442582 0.7838576418
#> 2394       b_cyl -1.7574417902 0.7876497239
#> 2395       b_cyl -1.7541393221 0.7914145620
#> 2396       b_cyl -1.7508368541 0.7951463607
#> 2397       b_cyl -1.7475343861 0.7988498590
#> 2398       b_cyl -1.7442319180 0.8025242846
#> 2399       b_cyl -1.7409294500 0.8061688918
#> 2400       b_cyl -1.7376269820 0.8097829614
#> 2401       b_cyl -1.7343245140 0.8133637253
#> 2402       b_cyl -1.7310220459 0.8169061859
#> 2403       b_cyl -1.7277195779 0.8204157025
#> 2404       b_cyl -1.7244171099 0.8238916832
#> 2405       b_cyl -1.7211146419 0.8273335537
#> 2406       b_cyl -1.7178121738 0.8307407559
#> 2407       b_cyl -1.7145097058 0.8341096620
#> 2408       b_cyl -1.7112072378 0.8374364922
#> 2409       b_cyl -1.7079047698 0.8407267186
#> 2410       b_cyl -1.7046023017 0.8439798357
#> 2411       b_cyl -1.7012998337 0.8471953401
#> 2412       b_cyl -1.6979973657 0.8503727288
#> 2413       b_cyl -1.6946948977 0.8535072843
#> 2414       b_cyl -1.6913924296 0.8565966175
#> 2415       b_cyl -1.6880899616 0.8596459408
#> 2416       b_cyl -1.6847874936 0.8626547167
#> 2417       b_cyl -1.6814850256 0.8656223953
#> 2418       b_cyl -1.6781825575 0.8685484138
#> 2419       b_cyl -1.6748800895 0.8714266601
#> 2420       b_cyl -1.6715776215 0.8742562657
#> 2421       b_cyl -1.6682751535 0.8770419725
#> 2422       b_cyl -1.6649726854 0.8797831231
#> 2423       b_cyl -1.6616702174 0.8824790405
#> 2424       b_cyl -1.6583677494 0.8851290288
#> 2425       b_cyl -1.6550652814 0.8877252255
#> 2426       b_cyl -1.6517628133 0.8902685595
#> 2427       b_cyl -1.6484603453 0.8927631989
#> 2428       b_cyl -1.6451578773 0.8952083427
#> 2429       b_cyl -1.6418554093 0.8976031728
#> 2430       b_cyl -1.6385529412 0.8999468554
#> 2431       b_cyl -1.6352504732 0.9022294284
#> 2432       b_cyl -1.6319480052 0.9044541188
#> 2433       b_cyl -1.6286455372 0.9066244501
#> 2434       b_cyl -1.6253430691 0.9087395245
#> 2435       b_cyl -1.6220406011 0.9107984386
#> 2436       b_cyl -1.6187381331 0.9128002842
#> 2437       b_cyl -1.6154356650 0.9147327391
#> 2438       b_cyl -1.6121331970 0.9166019979
#> 2439       b_cyl -1.6088307290 0.9184108461
#> 2440       b_cyl -1.6055282610 0.9201583925
#> 2441       b_cyl -1.6022257929 0.9218437568
#> 2442       b_cyl -1.5989233249 0.9234660708
#> 2443       b_cyl -1.5956208569 0.9250105694
#> 2444       b_cyl -1.5923183889 0.9264870903
#> 2445       b_cyl -1.5890159208 0.9278975522
#> 2446       b_cyl -1.5857134528 0.9292412027
#> 2447       b_cyl -1.5824109848 0.9305173180
#> 2448       b_cyl -1.5791085168 0.9317252041
#> 2449       b_cyl -1.5758060487 0.9328478142
#> 2450       b_cyl -1.5725035807 0.9338991004
#> 2451       b_cyl -1.5692011127 0.9348799710
#> 2452       b_cyl -1.5658986447 0.9357899375
#> 2453       b_cyl -1.5625961766 0.9366285541
#> 2454       b_cyl -1.5592937086 0.9373953861
#> 2455       b_cyl -1.5559912406 0.9380716138
#> 2456       b_cyl -1.5526887726 0.9386752848
#> 2457       b_cyl -1.5493863045 0.9392062063
#> 2458       b_cyl -1.5460838365 0.9396642326
#> 2459       b_cyl -1.5427813685 0.9400492655
#> 2460       b_cyl -1.5394789005 0.9403596698
#> 2461       b_cyl -1.5361764324 0.9405800009
#> 2462       b_cyl -1.5328739644 0.9407274355
#> 2463       b_cyl -1.5295714964 0.9408021283
#> 2464       b_cyl -1.5262690284 0.9408042758
#> 2465       b_cyl -1.5229665603 0.9407341149
#> 2466       b_cyl -1.5196640923 0.9405888483
#> 2467       b_cyl -1.5163616243 0.9403568187
#> 2468       b_cyl -1.5130591563 0.9400537170
#> 2469       b_cyl -1.5097566882 0.9396799683
#> 2470       b_cyl -1.5064542202 0.9392360226
#> 2471       b_cyl -1.5031517522 0.9387223534
#> 2472       b_cyl -1.4998492841 0.9381350595
#> 2473       b_cyl -1.4965468161 0.9374661954
#> 2474       b_cyl -1.4932443481 0.9367295291
#> 2475       b_cyl -1.4899418801 0.9359256128
#> 2476       b_cyl -1.4866394120 0.9350550032
#> 2477       b_cyl -1.4833369440 0.9341182597
#> 2478       b_cyl -1.4800344760 0.9331104083
#> 2479       b_cyl -1.4767320080 0.9320267890
#> 2480       b_cyl -1.4734295399 0.9308790713
#> 2481       b_cyl -1.4701270719 0.9296677863
#> 2482       b_cyl -1.4668246039 0.9283934538
#> 2483       b_cyl -1.4635221359 0.9270565813
#> 2484       b_cyl -1.4602196678 0.9256511177
#> 2485       b_cyl -1.4569171998 0.9241751892
#> 2486       b_cyl -1.4536147318 0.9226384495
#> 2487       b_cyl -1.4503122638 0.9210413272
#> 2488       b_cyl -1.4470097957 0.9193842375
#> 2489       b_cyl -1.4437073277 0.9176675826
#> 2490       b_cyl -1.4404048597 0.9158842540
#> 2491       b_cyl -1.4371023917 0.9140348531
#> 2492       b_cyl -1.4337999236 0.9121272724
#> 2493       b_cyl -1.4304974556 0.9101618747
#> 2494       b_cyl -1.4271949876 0.9081390239
#> 2495       b_cyl -1.4238925196 0.9060590871
#> 2496       b_cyl -1.4205900515 0.9039140662
#> 2497       b_cyl -1.4172875835 0.9017069878
#> 2498       b_cyl -1.4139851155 0.8994442869
#> 2499       b_cyl -1.4106826475 0.8971264131
#> 2500       b_cyl -1.4073801794 0.8947538435
#> 2501       b_cyl -1.4040777114 0.8923270849
#> 2502       b_cyl -1.4007752434 0.8898376969
#> 2503       b_cyl -1.3974727754 0.8872911547
#> 2504       b_cyl -1.3941703073 0.8846926905
#> 2505       b_cyl -1.3908678393 0.8820430466
#> 2506       b_cyl -1.3875653713 0.8793430182
#> 2507       b_cyl -1.3842629032 0.8765934554
#> 2508       b_cyl -1.3809604352 0.8737862571
#> 2509       b_cyl -1.3776579672 0.8709290807
#> 2510       b_cyl -1.3743554992 0.8680261301
#> 2511       b_cyl -1.3710530311 0.8650785975
#> 2512       b_cyl -1.3677505631 0.8620877393
#> 2513       b_cyl -1.3644480951 0.8590548762
#> 2514       b_cyl -1.3611456271 0.8559732652
#> 2515       b_cyl -1.3578431590 0.8528518703
#> 2516       b_cyl -1.3545406910 0.8496939939
#> 2517       b_cyl -1.3512382230 0.8465012905
#> 2518       b_cyl -1.3479357550 0.8432754661
#> 2519       b_cyl -1.3446332869 0.8400182763
#> 2520       b_cyl -1.3413308189 0.8367253272
#> 2521       b_cyl -1.3380283509 0.8334053640
#> 2522       b_cyl -1.3347258829 0.8300609176
#> 2523       b_cyl -1.3314234148 0.8266939264
#> 2524       b_cyl -1.3281209468 0.8233063437
#> 2525       b_cyl -1.3248184788 0.8199001305
#> 2526       b_cyl -1.3215160108 0.8164738455
#> 2527       b_cyl -1.3182135427 0.8130341842
#> 2528       b_cyl -1.3149110747 0.8095830618
#> 2529       b_cyl -1.3116086067 0.8061223614
#> 2530       b_cyl -1.3083061387 0.8026539287
#> 2531       b_cyl -1.3050036706 0.7991794904
#> 2532       b_cyl -1.3017012026 0.7957006445
#> 2533       b_cyl -1.2983987346 0.7922203562
#> 2534       b_cyl -1.2950962666 0.7887401361
#> 2535       b_cyl -1.2917937985 0.7852614114
#> 2536       b_cyl -1.2884913305 0.7817855214
#> 2537       b_cyl -1.2851888625 0.7783139529
#> 2538       b_cyl -1.2818863945 0.7748491798
#> 2539       b_cyl -1.2785839264 0.7713911978
#> 2540       b_cyl -1.2752814584 0.7679407693
#> 2541       b_cyl -1.2719789904 0.7644985400
#> 2542       b_cyl -1.2686765223 0.7610650375
#> 2543       b_cyl -1.2653740543 0.7576412726
#> 2544       b_cyl -1.2620715863 0.7542287433
#> 2545       b_cyl -1.2587691183 0.7508256715
#> 2546       b_cyl -1.2554666502 0.7474319106
#> 2547       b_cyl -1.2521641822 0.7440471953
#> 2548       b_cyl -1.2488617142 0.7406711433
#> 2549       b_cyl -1.2455592462 0.7373038676
#> 2550       b_cyl -1.2422567781 0.7339449831
#> 2551       b_cyl -1.2389543101 0.7305922387
#> 2552       b_cyl -1.2356518421 0.7272446673
#> 2553       b_cyl -1.2323493741 0.7239012140
#> 2554       b_cyl -1.2290469060 0.7205607404
#> 2555       b_cyl -1.2257444380 0.7172220062
#> 2556       b_cyl -1.2224419700 0.7138830224
#> 2557       b_cyl -1.2191395020 0.7105421219
#> 2558       b_cyl -1.2158370339 0.7071978316
#> 2559       b_cyl -1.2125345659 0.7038486446
#> 2560       b_cyl -1.2092320979 0.7004930251
#> 2561       b_cyl -1.2059296299 0.6971280911
#> 2562       b_cyl -1.2026271618 0.6937514631
#> 2563       b_cyl -1.1993246938 0.6903626319
#> 2564       b_cyl -1.1960222258 0.6869600532
#> 2565       b_cyl -1.1927197578 0.6835422065
#> 2566       b_cyl -1.1894172897 0.6801076007
#> 2567       b_cyl -1.1861148217 0.6766517517
#> 2568       b_cyl -1.1828123537 0.6731734085
#> 2569       b_cyl -1.1795098857 0.6696732645
#> 2570       b_cyl -1.1762074176 0.6661501105
#> 2571       b_cyl -1.1729049496 0.6626028053
#> 2572       b_cyl -1.1696024816 0.6590302787
#> 2573       b_cyl -1.1663000136 0.6554268468
#> 2574       b_cyl -1.1629975455 0.6517935204
#> 2575       b_cyl -1.1596950775 0.6481318420
#> 2576       b_cyl -1.1563926095 0.6444411905
#> 2577       b_cyl -1.1530901414 0.6407210304
#> 2578       b_cyl -1.1497876734 0.6369709118
#> 2579       b_cyl -1.1464852054 0.6331846048
#> 2580       b_cyl -1.1431827374 0.6293656927
#> 2581       b_cyl -1.1398802693 0.6255160494
#> 2582       b_cyl -1.1365778013 0.6216356863
#> 2583       b_cyl -1.1332753333 0.6177246911
#> 2584       b_cyl -1.1299728653 0.6137832265
#> 2585       b_cyl -1.1266703972 0.6098052241
#> 2586       b_cyl -1.1233679292 0.6057963158
#> 2587       b_cyl -1.1200654612 0.6017582474
#> 2588       b_cyl -1.1167629932 0.5976915338
#> 2589       b_cyl -1.1134605251 0.5935967415
#> 2590       b_cyl -1.1101580571 0.5894744854
#> 2591       b_cyl -1.1068555891 0.5853194424
#> 2592       b_cyl -1.1035531211 0.5811382687
#> 2593       b_cyl -1.1002506530 0.5769323193
#> 2594       b_cyl -1.0969481850 0.5727024069
#> 2595       b_cyl -1.0936457170 0.5684493688
#> 2596       b_cyl -1.0903432490 0.5641740643
#> 2597       b_cyl -1.0870407809 0.5598723051
#> 2598       b_cyl -1.0837383129 0.5555506842
#> 2599       b_cyl -1.0804358449 0.5512101275
#> 2600       b_cyl -1.0771333769 0.5468515647
#> 2601       b_cyl -1.0738309088 0.5424759319
#> 2602       b_cyl -1.0705284408 0.5380838571
#> 2603       b_cyl -1.0672259728 0.5336734756
#> 2604       b_cyl -1.0639235048 0.5292495071
#> 2605       b_cyl -1.0606210367 0.5248128996
#> 2606       b_cyl -1.0573185687 0.5203646007
#> 2607       b_cyl -1.0540161007 0.5159055574
#> 2608       b_cyl -1.0507136327 0.5114363571
#> 2609       b_cyl -1.0474111646 0.5069568760
#> 2610       b_cyl -1.0441086966 0.5024701323
#> 2611       b_cyl -1.0408062286 0.4979770682
#> 2612       b_cyl -1.0375037605 0.4934786254
#> 2613       b_cyl -1.0342012925 0.4889757454
#> 2614       b_cyl -1.0308988245 0.4844692322
#> 2615       b_cyl -1.0275963565 0.4799601539
#> 2616       b_cyl -1.0242938884 0.4754501064
#> 2617       b_cyl -1.0209914204 0.4709400304
#> 2618       b_cyl -1.0176889524 0.4664308665
#> 2619       b_cyl -1.0143864844 0.4619235547
#> 2620       b_cyl -1.0110840163 0.4574193872
#> 2621       b_cyl -1.0077815483 0.4529200852
#> 2622       b_cyl -1.0044790803 0.4484260875
#> 2623       b_cyl -1.0011766123 0.4439383237
#> 2624       b_cyl -0.9978741442 0.4394577188
#> 2625       b_cyl -0.9945716762 0.4349851931
#> 2626       b_cyl -0.9912692082 0.4305227577
#> 2627       b_cyl -0.9879667402 0.4260721976
#> 2628       b_cyl -0.9846642721 0.4216330371
#> 2629       b_cyl -0.9813618041 0.4172061504
#> 2630       b_cyl -0.9780593361 0.4127924001
#> 2631       b_cyl -0.9747568681 0.4083926357
#> 2632       b_cyl -0.9714544000 0.4040097373
#> 2633       b_cyl -0.9681519320 0.3996449388
#> 2634       b_cyl -0.9648494640 0.3952971048
#> 2635       b_cyl -0.9615469960 0.3909669961
#> 2636       b_cyl -0.9582445279 0.3866553549
#> 2637       b_cyl -0.9549420599 0.3823629041
#> 2638       b_cyl -0.9516395919 0.3780934567
#> 2639       b_cyl -0.9483371239 0.3738471645
#> 2640       b_cyl -0.9450346558 0.3696225225
#> 2641       b_cyl -0.9417321878 0.3654201425
#> 2642       b_cyl -0.9384297198 0.3612406168
#> 2643       b_cyl -0.9351272518 0.3570845180
#> 2644       b_cyl -0.9318247837 0.3529566121
#> 2645       b_cyl -0.9285223157 0.3488556231
#> 2646       b_cyl -0.9252198477 0.3447799842
#> 2647       b_cyl -0.9219173796 0.3407301733
#> 2648       b_cyl -0.9186149116 0.3367066544
#> 2649       b_cyl -0.9153124436 0.3327098786
#> 2650       b_cyl -0.9120099756 0.3287456086
#> 2651       b_cyl -0.9087075075 0.3248109826
#> 2652       b_cyl -0.9054050395 0.3209046578
#> 2653       b_cyl -0.9021025715 0.3170270387
#> 2654       b_cyl -0.8988001035 0.3131785256
#> 2655       b_cyl -0.8954976354 0.3093595156
#> 2656       b_cyl -0.8921951674 0.3055768907
#> 2657       b_cyl -0.8888926994 0.3018261211
#> 2658       b_cyl -0.8855902314 0.2981063176
#> 2659       b_cyl -0.8822877633 0.2944178834
#> 2660       b_cyl -0.8789852953 0.2907612256
#> 2661       b_cyl -0.8756828273 0.2871367559
#> 2662       b_cyl -0.8723803593 0.2835526738
#> 2663       b_cyl -0.8690778912 0.2800026281
#> 2664       b_cyl -0.8657754232 0.2764863454
#> 2665       b_cyl -0.8624729552 0.2730042679
#> 2666       b_cyl -0.8591704872 0.2695568426
#> 2667       b_cyl -0.8558680191 0.2661445213
#> 2668       b_cyl -0.8525655511 0.2627770208
#> 2669       b_cyl -0.8492630831 0.2594458720
#> 2670       b_cyl -0.8459606151 0.2561515196
#> 2671       b_cyl -0.8426581470 0.2528944226
#> 2672       b_cyl -0.8393556790 0.2496750366
#> 2673       b_cyl -0.8360532110 0.2464946199
#> 2674       b_cyl -0.8327507430 0.2433620606
#> 2675       b_cyl -0.8294482749 0.2402688274
#> 2676       b_cyl -0.8261458069 0.2372153265
#> 2677       b_cyl -0.8228433389 0.2342019481
#> 2678       b_cyl -0.8195408709 0.2312290648
#> 2679       b_cyl -0.8162384028 0.2282987667
#> 2680       b_cyl -0.8129359348 0.2254185657
#> 2681       b_cyl -0.8096334668 0.2225800128
#> 2682       b_cyl -0.8063309987 0.2197833433
#> 2683       b_cyl -0.8030285307 0.2170287606
#> 2684       b_cyl -0.7997260627 0.2143164347
#> 2685       b_cyl -0.7964235947 0.2116491919
#> 2686       b_cyl -0.7931211266 0.2090326060
#> 2687       b_cyl -0.7898186586 0.2064585341
#> 2688       b_cyl -0.7865161906 0.2039269315
#> 2689       b_cyl -0.7832137226 0.2014377106
#> 2690       b_cyl -0.7799112545 0.1989907401
#> 2691       b_cyl -0.7766087865 0.1965893720
#> 2692       b_cyl -0.7733063185 0.1942368347
#> 2693       b_cyl -0.7700038505 0.1919256130
#> 2694       b_cyl -0.7667013824 0.1896553261
#> 2695       b_cyl -0.7633989144 0.1874255487
#> 2696       b_cyl -0.7600964464 0.1852358119
#> 2697       b_cyl -0.7567939784 0.1830896936
#> 2698       b_cyl -0.7534915103 0.1809880134
#> 2699       b_cyl -0.7501890423 0.1789242169
#> 2700       b_cyl -0.7468865743 0.1768976095
#> 2701       b_cyl -0.7435841063 0.1749074613
#> 2702       b_cyl -0.7402816382 0.1729530085
#> 2703       b_cyl -0.7369791702 0.1710377260
#> 2704       b_cyl -0.7336767022 0.1691603708
#> 2705       b_cyl -0.7303742342 0.1673155936
#> 2706       b_cyl -0.7270717661 0.1655024816
#> 2707       b_cyl -0.7237692981 0.1637201033
#> 2708       b_cyl -0.7204668301 0.1619675102
#> 2709       b_cyl -0.7171643621 0.1602477889
#> 2710       b_cyl -0.7138618940 0.1585582966
#> 2711       b_cyl -0.7105594260 0.1568949774
#> 2712       b_cyl -0.7072569580 0.1552568365
#> 2713       b_cyl -0.7039544900 0.1536428787
#> 2714       b_cyl -0.7006520219 0.1520521105
#> 2715       b_cyl -0.6973495539 0.1504870461
#> 2716       b_cyl -0.6940470859 0.1489444197
#> 2717       b_cyl -0.6907446179 0.1474213884
#> 2718       b_cyl -0.6874421498 0.1459170073
#> 2719       b_cyl -0.6841396818 0.1444303461
#> 2720       b_cyl -0.6808372138 0.1429604902
#> 2721       b_cyl -0.6775347457 0.1415093148
#> 2722       b_cyl -0.6742322777 0.1400736076
#> 2723       b_cyl -0.6709298097 0.1386515303
#> 2724       b_cyl -0.6676273417 0.1372422782
#> 2725       b_cyl -0.6643248736 0.1358450689
#> 2726       b_cyl -0.6610224056 0.1344591426
#> 2727       b_cyl -0.6577199376 0.1330857603
#> 2728       b_cyl -0.6544174696 0.1317222063
#> 2729       b_cyl -0.6511150015 0.1303673793
#> 2730       b_cyl -0.6478125335 0.1290206485
#> 2731       b_cyl -0.6445100655 0.1276814057
#> 2732       b_cyl -0.6412075975 0.1263490655
#> 2733       b_cyl -0.6379051294 0.1250243428
#> 2734       b_cyl -0.6346026614 0.1237052177
#> 2735       b_cyl -0.6313001934 0.1223910661
#> 2736       b_cyl -0.6279977254 0.1210814194
#> 2737       b_cyl -0.6246952573 0.1197758280
#> 2738       b_cyl -0.6213927893 0.1184738616
#> 2739       b_cyl -0.6180903213 0.1171757645
#> 2740       b_cyl -0.6147878533 0.1158802479
#> 2741       b_cyl -0.6114853852 0.1145869587
#> 2742       b_cyl -0.6081829172 0.1132955622
#> 2743       b_cyl -0.6048804492 0.1120057398
#> 2744       b_cyl -0.6015779812 0.1107172072
#> 2745       b_cyl -0.5982755131 0.1094297739
#> 2746       b_cyl -0.5949730451 0.1081428965
#> 2747       b_cyl -0.5916705771 0.1068563438
#> 2748       b_cyl -0.5883681091 0.1055699011
#> 2749       b_cyl -0.5850656410 0.1042833697
#> 2750       b_cyl -0.5817631730 0.1029965470
#> 2751       b_cyl -0.5784607050 0.1017091332
#> 2752       b_cyl -0.5751582370 0.1004210686
#> 2753       b_cyl -0.5718557689 0.0991322529
#> 2754       b_cyl -0.5685533009 0.0978426057
#> 2755       b_cyl -0.5652508329 0.0965520668
#> 2756       b_cyl -0.5619483648 0.0952605377
#> 2757       b_cyl -0.5586458968 0.0939678928
#> 2758       b_cyl -0.5553434288 0.0926743523
#> 2759       b_cyl -0.5520409608 0.0913799816
#> 2760       b_cyl -0.5487384927 0.0900848712
#> 2761       b_cyl -0.5454360247 0.0887891368
#> 2762       b_cyl -0.5421335567 0.0874929027
#> 2763       b_cyl -0.5388310887 0.0861964227
#> 2764       b_cyl -0.5355286206 0.0849000138
#> 2765       b_cyl -0.5322261526 0.0836039416
#> 2766       b_cyl -0.5289236846 0.0823084996
#> 2767       b_cyl -0.5256212166 0.0810140087
#> 2768       b_cyl -0.5223187485 0.0797210169
#> 2769       b_cyl -0.5190162805 0.0784301950
#> 2770       b_cyl -0.5157138125 0.0771417837
#> 2771       b_cyl -0.5124113445 0.0758562516
#> 2772       b_cyl -0.5091088764 0.0745740915
#> 2773       b_cyl -0.5058064084 0.0732958195
#> 2774       b_cyl -0.5025039404 0.0720226458
#> 2775       b_cyl -0.4992014724 0.0707554490
#> 2776       b_cyl -0.4958990043 0.0694942537
#> 2777       b_cyl -0.4925965363 0.0682396805
#> 2778       b_cyl -0.4892940683 0.0669923633
#> 2779       b_cyl -0.4859916003 0.0657529473
#> 2780       b_cyl -0.4826891322 0.0645235070
#> 2781       b_cyl -0.4793866642 0.0633046794
#> 2782       b_cyl -0.4760841962 0.0620962033
#> 2783       b_cyl -0.4727817282 0.0608987481
#> 2784       b_cyl -0.4694792601 0.0597129800
#> 2785       b_cyl -0.4661767921 0.0585395598
#> 2786       b_cyl -0.4628743241 0.0573815108
#> 2787       b_cyl -0.4595718561 0.0562386612
#> 2788       b_cyl -0.4562693880 0.0551104874
#> 2789       b_cyl -0.4529669200 0.0539975774
#> 2790       b_cyl -0.4496644520 0.0529004994
#> 2791       b_cyl -0.4463619839 0.0518197993
#> 2792       b_cyl -0.4430595159 0.0507593639
#> 2793       b_cyl -0.4397570479 0.0497177264
#> 2794       b_cyl -0.4364545799 0.0486942070
#> 2795       b_cyl -0.4331521118 0.0476891966
#> 2796       b_cyl -0.4298496438 0.0467030540
#> 2797       b_cyl -0.4265471758 0.0457361046
#> 2798       b_cyl -0.4232447078 0.0447928430
#> 2799       b_cyl -0.4199422397 0.0438702920
#> 2800       b_cyl -0.4166397717 0.0429677651
#> 2801       b_cyl -0.4133373037 0.0420853909
#> 2802       b_cyl -0.4100348357 0.0412232620
#> 2803       b_cyl -0.4067323676 0.0403814350
#> 2804       b_cyl -0.4034298996 0.0395646484
#> 2805       b_cyl -0.4001274316 0.0387685569
#> 2806       b_cyl -0.3968249636 0.0379926084
#> 2807       b_cyl -0.3935224955 0.0372366720
#> 2808       b_cyl -0.3902200275 0.0365005860
#> 2809       b_cyl -0.3869175595 0.0357841592
#> 2810       b_cyl -0.3836150915 0.0350919981
#> 2811       b_cyl -0.3803126234 0.0344188376
#> 2812       b_cyl -0.3770101554 0.0337643508
#> 2813       b_cyl -0.3737076874 0.0331282105
#> 2814       b_cyl -0.3704052194 0.0325100701
#> 2815       b_cyl -0.3671027513 0.0319099111
#> 2816       b_cyl -0.3638002833 0.0313308769
#> 2817       b_cyl -0.3604978153 0.0307684108
#> 2818       b_cyl -0.3571953473 0.0302220910
#> 2819       b_cyl -0.3538928792 0.0296914883
#> 2820       b_cyl -0.3505904112 0.0291761671
#> 2821       b_cyl -0.3472879432 0.0286762762
#> 2822       b_cyl -0.3439854752 0.0281936439
#> 2823       b_cyl -0.3406830071 0.0277246559
#> 2824       b_cyl -0.3373805391 0.0272688681
#> 2825       b_cyl -0.3340780711 0.0268258393
#> 2826       b_cyl -0.3307756030 0.0263951321
#> 2827       b_cyl -0.3274731350 0.0259770213
#> 2828       b_cyl -0.3241706670 0.0255723679
#> 2829       b_cyl -0.3208681990 0.0251784791
#> 2830       b_cyl -0.3175657309 0.0247949526
#> 2831       b_cyl -0.3142632629 0.0244213943
#> 2832       b_cyl -0.3109607949 0.0240574188
#> 2833       b_cyl -0.3076583269 0.0237033779
#> 2834       b_cyl -0.3043558588 0.0233595032
#> 2835       b_cyl -0.3010533908 0.0230238792
#> 2836       b_cyl -0.2977509228 0.0226961692
#> 2837       b_cyl -0.2944484548 0.0223760452
#> 2838       b_cyl -0.2911459867 0.0220631875
#> 2839       b_cyl -0.2878435187 0.0217579696
#> 2840       b_cyl -0.2845410507 0.0214602403
#> 2841       b_cyl -0.2812385827 0.0211686761
#> 2842       b_cyl -0.2779361146 0.0208829973
#> 2843       b_cyl -0.2746336466 0.0206029296
#> 2844       b_cyl -0.2713311786 0.0203282045
#> 2845       b_cyl -0.2680287106 0.0200591517
#> 2846       b_cyl -0.2647262425 0.0197953956
#> 2847       b_cyl -0.2614237745 0.0195360384
#> 2848       b_cyl -0.2581213065 0.0192808343
#> 2849       b_cyl -0.2548188385 0.0190295406
#> 2850       b_cyl -0.2515163704 0.0187819173
#> 2851       b_cyl -0.2482139024 0.0185381812
#> 2852       b_cyl -0.2449114344 0.0182978496
#> 2853       b_cyl -0.2416089664 0.0180603290
#> 2854       b_cyl -0.2383064983 0.0178253927
#> 2855       b_cyl -0.2350040303 0.0175928162
#> 2856       b_cyl -0.2317015623 0.0173623776
#> 2857       b_cyl -0.2283990943 0.0171341252
#> 2858       b_cyl -0.2250966262 0.0169075930
#> 2859       b_cyl -0.2217941582 0.0166824122
#> 2860       b_cyl -0.2184916902 0.0164583798
#> 2861       b_cyl -0.2151892221 0.0162352978
#> 2862       b_cyl -0.2118867541 0.0160129730
#> 2863       b_cyl -0.2085842861 0.0157912729
#> 2864       b_cyl -0.2052818181 0.0155698815
#> 2865       b_cyl -0.2019793500 0.0153486021
#> 2866       b_cyl -0.1986768820 0.0151272808
#> 2867       b_cyl -0.1953744140 0.0149057727
#> 2868       b_cyl -0.1920719460 0.0146839423
#> 2869       b_cyl -0.1887694779 0.0144615341
#> 2870       b_cyl -0.1854670099 0.0142384830
#> 2871       b_cyl -0.1821645419 0.0140147260
#> 2872       b_cyl -0.1788620739 0.0137901921
#> 2873       b_cyl -0.1755596058 0.0135648237
#> 2874       b_cyl -0.1722571378 0.0133385769
#> 2875       b_cyl -0.1689546698 0.0131112099
#> 2876       b_cyl -0.1656522018 0.0128829131
#> 2877       b_cyl -0.1623497337 0.0126537301
#> 2878       b_cyl -0.1590472657 0.0124236992
#> 2879       b_cyl -0.1557447977 0.0121928743
#> 2880       b_cyl -0.1524423297 0.0119613245
#> 2881       b_cyl -0.1491398616 0.0117290214
#> 2882       b_cyl -0.1458373936 0.0114962720
#> 2883       b_cyl -0.1425349256 0.0112632149
#> 2884       b_cyl -0.1392324576 0.0110300021
#> 2885       b_cyl -0.1359299895 0.0107967994
#> 2886       b_cyl -0.1326275215 0.0105637951
#> 2887       b_cyl -0.1293250535 0.0103313609
#> 2888       b_cyl -0.1260225855 0.0100996729
#> 2889       b_cyl -0.1227201174 0.0098689626
#> 2890       b_cyl -0.1194176494 0.0096394706
#> 2891       b_cyl -0.1161151814 0.0094114460
#> 2892       b_cyl -0.1128127134 0.0091852320
#> 2893       b_cyl -0.1095102453 0.0089615641
#> 2894       b_cyl -0.1062077773 0.0087403453
#> 2895       b_cyl -0.1029053093 0.0085218532
#> 2896       b_cyl -0.0996028412 0.0083063665
#> 2897       b_cyl -0.0963003732 0.0080941645
#> 2898       b_cyl -0.0929979052 0.0078857728
#> 2899       b_cyl -0.0896954372 0.0076820996
#> 2900       b_cyl -0.0863929691 0.0074827198
#> 2901       b_cyl -0.0830905011 0.0072878935
#> 2902       b_cyl -0.0797880331 0.0070978737
#> 2903       b_cyl -0.0764855651 0.0069129048
#> 2904       b_cyl -0.0731830970 0.0067336912
#> 2905       b_cyl -0.0698806290 0.0065610420
#> 2906       b_cyl -0.0665781610 0.0063942400
#> 2907       b_cyl -0.0632756930 0.0062334654
#> 2908       b_cyl -0.0599732249 0.0060788838
#> 2909       b_cyl -0.0566707569 0.0059306457
#> 2910       b_cyl -0.0533682889 0.0057895821
#> 2911       b_cyl -0.0500658209 0.0056561511
#> 2912       b_cyl -0.0467633528 0.0055294495
#> 2913       b_cyl -0.0434608848 0.0054095322
#> 2914       b_cyl -0.0401584168 0.0052964356
#> 2915       b_cyl -0.0368559488 0.0051901773
#> 2916       b_cyl -0.0335534807 0.0050916121
#> 2917       b_cyl -0.0302510127 0.0050007068
#> 2918       b_cyl -0.0269485447 0.0049165108
#> 2919       b_cyl -0.0236460767 0.0048389365
#> 2920       b_cyl -0.0203436086 0.0047678785
#> 2921       b_cyl -0.0170411406 0.0047032129
#> 2922       b_cyl -0.0137386726 0.0046456810
#> 2923       b_cyl -0.0104362046 0.0045947807
#> 2924       b_cyl -0.0071337365 0.0045496416
#> 2925       b_cyl -0.0038312685 0.0045100492
#> 2926       b_cyl -0.0005288005 0.0044757751
#> 2927       b_cyl  0.0027736675 0.0044465774
#> 2928       b_cyl  0.0060761356 0.0044229419
#> 2929       b_cyl  0.0093786036 0.0044040763
#> 2930       b_cyl  0.0126810716 0.0043892767
#> 2931       b_cyl  0.0159835397 0.0043782440
#> 2932       b_cyl  0.0192860077 0.0043706718
#> 2933       b_cyl  0.0225884757 0.0043662478
#> 2934       b_cyl  0.0258909437 0.0043650932
#> 2935       b_cyl  0.0291934118 0.0043664023
#> 2936       b_cyl  0.0324958798 0.0043696691
#> 2937       b_cyl  0.0357983478 0.0043745671
#> 2938       b_cyl  0.0391008158 0.0043807714
#> 2939       b_cyl  0.0424032839 0.0043879586
#> 2940       b_cyl  0.0457057519 0.0043958408
#> 2941       b_cyl  0.0490082199 0.0044038854
#> 2942       b_cyl  0.0523106879 0.0044117658
#> 2943       b_cyl  0.0556131560 0.0044191879
#> 2944       b_cyl  0.0589156240 0.0044258663
#> 2945       b_cyl  0.0622180920 0.0044315243
#> 2946       b_cyl  0.0655205600 0.0044355008
#> 2947       b_cyl  0.0688230281 0.0044377448
#> 2948       b_cyl  0.0721254961 0.0044380611
#> 2949       b_cyl  0.0754279641 0.0044362359
#> 2950       b_cyl  0.0787304321 0.0044320685
#> 2951       b_cyl  0.0820329002 0.0044253719
#> 2952       b_cyl  0.0853353682 0.0044152254
#> 2953       b_cyl  0.0886378362 0.0044021294
#> 2954       b_cyl  0.0919403042 0.0043859667
#> 2955       b_cyl  0.0952427723 0.0043666311
#> 2956       b_cyl  0.0985452403 0.0043440309
#> 2957       b_cyl  0.1018477083 0.0043180227
#> 2958       b_cyl  0.1051501763 0.0042877960
#> 2959       b_cyl  0.1084526444 0.0042541147
#> 2960       b_cyl  0.1117551124 0.0042169695
#> 2961       b_cyl  0.1150575804 0.0041763646
#> 2962       b_cyl  0.1183600484 0.0041323183
#> 2963       b_cyl  0.1216625165 0.0040847260
#> 2964       b_cyl  0.1249649845 0.0040330818
#> 2965       b_cyl  0.1282674525 0.0039781925
#> 2966       b_cyl  0.1315699205 0.0039201452
#> 2967       b_cyl  0.1348723886 0.0038590379
#> 2968       b_cyl  0.1381748566 0.0037949787
#> 2969       b_cyl  0.1414773246 0.0037279209
#> 2970       b_cyl  0.1447797927 0.0036577066
#> 2971       b_cyl  0.1480822607 0.0035850282
#> 2972       b_cyl  0.1513847287 0.0035100392
#> 2973       b_cyl  0.1546871967 0.0034328989
#> 2974       b_cyl  0.1579896648 0.0033537720
#> 2975       b_cyl  0.1612921328 0.0032726980
#> 2976       b_cyl  0.1645946008 0.0031897945
#> 2977       b_cyl  0.1678970688 0.0031055515
#> 2978       b_cyl  0.1711995369 0.0030201512
#> 2979       b_cyl  0.1745020049 0.0029337769
#> 2980       b_cyl  0.1778044729 0.0028466123
#> 2981       b_cyl  0.1811069409 0.0027588060
#> 2982       b_cyl  0.1844094090 0.0026706123
#> 2983       b_cyl  0.1877118770 0.0025822974
#> 2984       b_cyl  0.1910143450 0.0024940372
#> 2985       b_cyl  0.1943168130 0.0024060042
#> 2986       b_cyl  0.1976192811 0.0023183676
#> 2987       b_cyl  0.2009217491 0.0022313934
#> 2988       b_cyl  0.2042242171 0.0021453225
#> 2989       b_cyl  0.2075266851 0.0020602271
#> 2990       b_cyl  0.2108291532 0.0019762501
#> 2991       b_cyl  0.2141316212 0.0018935287
#> 2992       b_cyl  0.2174340892 0.0018121942
#> 2993       b_cyl  0.2207365572 0.0017326206
#> 2994       b_cyl  0.2240390253 0.0016549174
#> 2995       b_cyl  0.2273414933 0.0015790215
#> 2996       b_cyl  0.2306439613 0.0015050293
#> 2997       b_cyl  0.2339464293 0.0014330304
#> 2998       b_cyl  0.2372488974 0.0013631083
#> 2999       b_cyl  0.2405513654 0.0012957196
#> 3000       b_cyl  0.2438538334 0.0012307825
#> 3001       b_cyl  0.2471563014 0.0011681630
#> 3002       b_cyl  0.2504587695 0.0011079097
#> 3003       b_cyl  0.2537612375 0.0010500650
#> 3004       b_cyl  0.2570637055 0.0009946658
#> 3005       b_cyl  0.2603661736 0.0009422195
#> 3006       b_cyl  0.2636686416 0.0008924487
#> 3007       b_cyl  0.2669711096 0.0008452035
#> 3008       b_cyl  0.2702735776 0.0008004921
#> 3009       b_cyl  0.2735760457 0.0007583186
#> 3010       b_cyl  0.2768785137 0.0007186828
#> 3011       b_cyl  0.2801809817 0.0006821103
#> 3012       b_cyl  0.2834834497 0.0006481638
#> 3013       b_cyl  0.2867859178 0.0006167139
#> 3014       b_cyl  0.2900883858 0.0005877404
#> 3015       b_cyl  0.2933908538 0.0005612204
#> 3016       b_cyl  0.2966933218 0.0005371280
#> 3017       b_cyl  0.2999957899 0.0005159791
#> 3018       b_cyl  0.3032982579 0.0004972289
#> 3019       b_cyl  0.3066007259 0.0004807856
#> 3020       b_cyl  0.3099031939 0.0004666110
#> 3021       b_cyl  0.3132056620 0.0004546648
#> 3022       b_cyl  0.3165081300 0.0004449051
#> 3023       b_cyl  0.3198105980 0.0004378109
#> 3024       b_cyl  0.3231130660 0.0004327842
#> 3025       b_cyl  0.3264155341 0.0004297718
#> 3026       b_cyl  0.3297180021 0.0004287228
#> 3027       b_cyl  0.3330204701 0.0004295850
#> 3028       b_cyl  0.3363229381 0.0004323390
#> 3029       b_cyl  0.3396254062 0.0004372941
#> 3030       b_cyl  0.3429278742 0.0004439533
#> 3031       b_cyl  0.3462303422 0.0004522561
#> 3032       b_cyl  0.3495328102 0.0004621404
#> 3033       b_cyl  0.3528352783 0.0004735428
#> 3034       b_cyl  0.3561377463 0.0004864530
#> 3035       b_cyl  0.3594402143 0.0005010131
#> 3036       b_cyl  0.3627426823 0.0005168407
#> 3037       b_cyl  0.3660451504 0.0005338632
#> 3038       b_cyl  0.3693476184 0.0005520062
#> 3039       b_cyl  0.3726500864 0.0005711938
#> 3040       b_cyl  0.3759525545 0.0005914009
#> 3041       b_cyl  0.3792550225 0.0006126252
#> 3042       b_cyl  0.3825574905 0.0006345975
#> 3043       b_cyl  0.3858599585 0.0006572329
#> 3044       b_cyl  0.3891624266 0.0006804453
#> 3045       b_cyl  0.3924648946 0.0007041471
#> 3046       b_cyl  0.3957673626 0.0007282721
#> 3047       b_cyl  0.3990698306 0.0007527125
#> 3048       b_cyl  0.4023722987 0.0007773091
#> 3049       b_cyl  0.4056747667 0.0008019694
#> 3050       b_cyl  0.4089772347 0.0008266005
#> 3051       b_cyl  0.4122797027 0.0008511096
#> 3052       b_cyl  0.4155821708 0.0008753666
#> 3053       b_cyl  0.4188846388 0.0008992181
#> 3054       b_cyl  0.4221871068 0.0009226074
#> 3055       b_cyl  0.4254895748 0.0009454445
#> 3056       b_cyl  0.4287920429 0.0009676407
#> 3057       b_cyl  0.4320945109 0.0009891091
#> 3058       b_cyl  0.4353969789 0.0010096469
#> 3059       b_cyl  0.4386994469 0.0010291273
#> 3060       b_cyl  0.4420019150 0.0010475812
#> 3061       b_cyl  0.4453043830 0.0010649351
#> 3062       b_cyl  0.4486068510 0.0010811191
#> 3063       b_cyl  0.4519093190 0.0010960666
#> 3064       b_cyl  0.4552117871 0.0011095115
#> 3065       b_cyl  0.4585142551 0.0011214216
#> 3066       b_cyl  0.4618167231 0.0011318915
#> 3067       b_cyl  0.4651191911 0.0011408781
#> 3068       b_cyl  0.4684216592 0.0011483434
#> 3069       b_cyl  0.4717241272 0.0011542542
#> 3070       b_cyl  0.4750265952 0.0011583115
#> 3071       b_cyl  0.4783290632 0.0011606167
#> 3072       b_cyl  0.4816315313 0.0011612989
# }
```
