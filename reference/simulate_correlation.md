# Data Simulation

Simulate data with specific characteristics.

## Usage

``` r
simulate_correlation(n = 100, r = 0.5, mean = 0, sd = 1, names = NULL, ...)

simulate_ttest(n = 100, d = 0.5, names = NULL, ...)

simulate_difference(n = 100, d = 0.5, names = NULL, ...)
```

## Arguments

- n:

  The number of observations to be generated.

- r:

  A value or vector corresponding to the desired correlation
  coefficients.

- mean:

  A value or vector corresponding to the mean of the variables.

- sd:

  A value or vector corresponding to the SD of the variables.

- names:

  A character vector of desired variable names.

- ...:

  Arguments passed to or from other methods.

- d:

  A value or vector corresponding to the desired difference between the
  groups.

## Examples

``` r

# Correlation --------------------------------
data <- simulate_correlation(r = 0.5)
plot(data$V1, data$V2)

cor.test(data$V1, data$V2)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  data$V1 and data$V2
#> t = 5.7155, df = 98, p-value = 1.18e-07
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.3366433 0.6341398
#> sample estimates:
#> cor 
#> 0.5 
#> 
summary(lm(V2 ~ V1, data = data))
#> 
#> Call:
#> lm(formula = V2 ~ V1, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.07226 -0.65675  0.06411  0.55903  1.91733 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -1.630e-17  8.704e-02   0.000        1    
#> V1           5.000e-01  8.748e-02   5.715 1.18e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.8704 on 98 degrees of freedom
#> Multiple R-squared:   0.25,  Adjusted R-squared:  0.2423 
#> F-statistic: 32.67 on 1 and 98 DF,  p-value: 1.18e-07
#> 

# Specify mean and SD
data <- simulate_correlation(r = 0.5, n = 50, mean = c(0, 1), sd = c(0.7, 1.7))
cor.test(data$V1, data$V2)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  data$V1 and data$V2
#> t = 4, df = 48, p-value = 0.000218
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.2574879 0.6832563
#> sample estimates:
#> cor 
#> 0.5 
#> 
round(c(mean(data$V1), sd(data$V1)), 1)
#> [1] 0.0 0.7
round(c(mean(data$V2), sd(data$V2)), 1)
#> [1] 1.0 1.7
summary(lm(V2 ~ V1, data = data))
#> 
#> Call:
#> lm(formula = V2 ~ V1, data = data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.9102 -1.1127  0.1926  1.0654  4.0502 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   1.0000     0.2104   4.754 1.86e-05 ***
#> V1            1.2143     0.3036   4.000 0.000218 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.487 on 48 degrees of freedom
#> Multiple R-squared:   0.25,  Adjusted R-squared:  0.2344 
#> F-statistic:    16 on 1 and 48 DF,  p-value: 0.000218
#> 

# Generate multiple variables
cor_matrix <- matrix(
  c(
    1.0, 0.2, 0.4,
    0.2, 1.0, 0.3,
    0.4, 0.3, 1.0
  ),
  nrow = 3
)

data <- simulate_correlation(r = cor_matrix, names = c("y", "x1", "x2"))
cor(data)
#>      y  x1  x2
#> y  1.0 0.2 0.4
#> x1 0.2 1.0 0.3
#> x2 0.4 0.3 1.0
summary(lm(y ~ x1, data = data))
#> 
#> Call:
#> lm(formula = y ~ x1, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.47132 -0.58008 -0.01947  0.71625  1.87046 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)  
#> (Intercept) -2.775e-17  9.848e-02   0.000    1.000  
#> x1           2.000e-01  9.897e-02   2.021    0.046 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9848 on 98 degrees of freedom
#> Multiple R-squared:   0.04,  Adjusted R-squared:  0.0302 
#> F-statistic: 4.083 on 1 and 98 DF,  p-value: 0.04604
#> 

# t-test --------------------------------
data <- simulate_ttest(n = 30, d = 0.3)
plot(data$V1, data$V0)

round(c(mean(data$V1), sd(data$V1)), 1)
#> [1] 0 1
diff(t.test(data$V1 ~ data$V0)$estimate)
#> mean in group 1 
#>     -0.01258664 
summary(lm(V1 ~ V0, data = data))
#> 
#> Call:
#> lm(formula = V1 ~ V0, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.12175 -0.65540 -0.00629  0.64596  2.13434 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)
#> (Intercept)  0.006293   0.261668   0.024    0.981
#> V01         -0.012587   0.370055  -0.034    0.973
#> 
#> Residual standard error: 1.013 on 28 degrees of freedom
#> Multiple R-squared:  4.132e-05,  Adjusted R-squared:  -0.03567 
#> F-statistic: 0.001157 on 1 and 28 DF,  p-value: 0.9731
#> 
summary(glm(V0 ~ V1, data = data, family = "binomial"))
#> 
#> Call:
#> glm(formula = V0 ~ V1, family = "binomial", data = data)
#> 
#> Coefficients:
#>               Estimate Std. Error z value Pr(>|z|)
#> (Intercept)  6.204e-17  3.652e-01   0.000    1.000
#> V1          -1.313e-02  3.730e-01  -0.035    0.972
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 41.589  on 29  degrees of freedom
#> Residual deviance: 41.588  on 28  degrees of freedom
#> AIC: 45.588
#> 
#> Number of Fisher Scoring iterations: 3
#> 

# Difference --------------------------------
data <- simulate_difference(n = 30, d = 0.3)
plot(data$V1, data$V0)

round(c(mean(data$V1), sd(data$V1)), 1)
#> [1] 0 1
diff(t.test(data$V1 ~ data$V0)$estimate)
#> mean in group 1 
#>             0.3 
summary(lm(V1 ~ V0, data = data))
#> 
#> Call:
#> lm(formula = V1 ~ V0, data = data)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -1.834 -0.677  0.000  0.677  1.834 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept)  -0.1500     0.2562  -0.586    0.563
#> V01           0.3000     0.3623   0.828    0.415
#> 
#> Residual standard error: 0.9922 on 28 degrees of freedom
#> Multiple R-squared:  0.0239, Adjusted R-squared:  -0.01096 
#> F-statistic: 0.6857 on 1 and 28 DF,  p-value: 0.4146
#> 
summary(glm(V0 ~ V1, data = data, family = "binomial"))
#> 
#> Call:
#> glm(formula = V0 ~ V1, family = "binomial", data = data)
#> 
#> Coefficients:
#>               Estimate Std. Error z value Pr(>|z|)
#> (Intercept) -4.569e-17  3.696e-01   0.000    1.000
#> V1           3.251e-01  3.877e-01   0.839    0.402
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 41.589  on 29  degrees of freedom
#> Residual deviance: 40.865  on 28  degrees of freedom
#> AIC: 44.865
#> 
#> Number of Fisher Scoring iterations: 4
#> 
```
