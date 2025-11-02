# Get Started with Bayesian Analysis

This vignette can be referred to by citing the package:

- Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019). *bayestestR:
  Describing Effects and their Uncertainty, Existence and Significance
  within the Bayesian Framework*. Journal of Open Source Software,
  4(40), 1541. <https://doi.org/10.21105/joss.01541>

------------------------------------------------------------------------

## Why use the Bayesian Framework?

The Bayesian framework for statistics is quickly gaining in popularity
among scientists, associated with the general shift towards **open and
honest science**. Reasons to prefer this approach are:

- **reliability** (Etz & Vandekerckhove, 2016)
- **accuracy** (in noisy data and small samples) (Kruschke, Aguinis, &
  Joo, 2012)
- the possibility of introducing **prior knowledge** into the analysis
  (Andrews & Baguley, 2013; Kruschke et al., 2012)
- critically, **intuitive nature of results** and their
  **straightforward interpretation** (Kruschke, 2010; Wagenmakers et
  al., 2018)

In general, the frequentist approach has been associated with the focus
on the null hypothesis testing, and the misuse of *p*-values has been
shown to critically contribute to the reproducibility crisis in social
and psychological sciences (Chambers, Feredoes, Muthukumaraswamy, &
Etchells, 2014; Szucs & Ioannidis, 2016). There is an emerging consensus
that the generalization of the Bayesian approach is *one* way of
overcoming these issues (Benjamin et al., 2018; Etz & Vandekerckhove,
2016).

Once we agree that the Bayesian framework is the right way to go, you
might wonder *what* exactly is this framework.

**What’s all the fuss about?**

## What is the Bayesian Framework?

Adopting the Bayesian framework is more of a shift in the paradigm than
a change in the methodology. Indeed, all the common statistical
procedures (*t*-tests, correlations, ANOVAs, regressions, etc.) can be
achieved using the Bayesian framework. The key difference is that in the
**frequentist framework** (the “classical” approach to statistics, with
*p* and *t* values, as well as some weird *degrees of freedom*), **the
effects are fixed** (but unknown) and **data are random**. In other
words, it assumes that the unknown parameter has a **unique** value that
we are trying to estimate/guess using our sample data. On the other
hand, in the **Bayesian framework**, instead of estimating the “true
effect”, the probability of different effects *given the observed data*
is computed, resulting in a **distribution** of possible values for the
parameters, called the **posterior distribution**.

The uncertainty in Bayesian inference can be summarized, for instance,
by the **median** of the distribution, as well as a range of values of
the posterior distribution that includes the 95% most probable values
(the 95% **credible interval**). *Cum grano salis*, these are considered
the counterparts to the point-estimate and confidence interval in a
frequentist framework. To illustrate the difference of interpretation,
the Bayesian framework allows to say *“given the observed data, the
effect has 95% probability of falling within this range”*, while the
frequentist (less intuitive) alternative would be *“when repeatedly
computing confidence intervals from data of this sort, there is a 95%
probability that the effect falls within a given range”*. In essence,
the Bayesian sampling algorithms (such as MCMC sampling) return a
probability distribution (*the posterior*) of an effect that is
compatible with the observed data. Thus, an effect can be described by
[characterizing its posterior
distribution](https://easystats.github.io/bayestestR/articles/guidelines.html)
in relation to its centrality (point-estimates), uncertainty, as well as
its existence and significance

In other words, putting the maths behind it aside for a moment, we can
say that:

- The frequentist approach tries to estimate the **real effect**. For
  instance, the “real” value of the correlation between *x* and *y*.
  Hence, the frequentist models return a **point-estimate** (i.e., a
  **single** value and not a distribution) of the “real” correlation
  (e.g., r = 0.42) estimated under a number of obscure assumptions (at a
  minimum, considering that the data is sampled at random from a
  “parent”, usually normal distribution).

- **The Bayesian framework assumes no such thing**. The data are what
  they are. Based on the observed data (and a **prior** belief about the
  result), the Bayesian sampling algorithm (**MCMC** sampling is one
  example) returns a probability distribution (called **the posterior**)
  of the effect that is compatible with the observed data. For the
  correlation between *x* and *y*, it will return a **distribution**
  that says, for example, “the most probable effect is 0.42, but this
  data is also compatible with correlations of 0.12 and 0.74 with
  certain probabilities”.

- To characterize statistical significance of our effects, we do not
  need *p*-values, or any other such indices. We simply *describe* the
  posterior distribution of the effect. For example, we can report the
  median, the [89% Credible
  Interval](https://easystats.github.io/bayestestR/articles/credible_interval.html)
  or [other
  indices](https://easystats.github.io/bayestestR/articles/guidelines.html).

![Accurate depiction of a regular Bayesian user estimating a credible
interval.](../reference/figures/bayesianMaster.jpg)

Accurate depiction of a regular Bayesian user estimating a credible
interval.

*Note: Altough the very purpose of this package is to advocate for the
use of Bayesian statistics, please note that there are serious arguments
supporting frequentist indices (see for instance [this
thread](https://discourse.datamethods.org/t/language-for-communicating-frequentist-results-about-treatment-effects/934/16)).
As always, the world is not black and white (p \< .001).*

**So… how does it work?**

## A simple example

### `bayestestR` installation

You can install `bayestestR` along with the whole
[**easystats**](https://github.com/easystats/easystats) suite by running
the following:

``` r

install.packages("remotes")
remotes::install_github("easystats/easystats")
```

Let’s also install and load the
[`rstanarm`](https://mc-stan.org/rstanarm/), that allows fitting
Bayesian models, as well as
[`bayestestR`](https://github.com/easystats/bayestestR), to describe
them.

``` r

install.packages("rstanarm")
library(rstanarm)
```

### Traditional linear regression

Let’s start by fitting a simple frequentist linear regression (the
[`lm()`](https://rdrr.io/r/stats/lm.html) function stands for *linear
model*) between two numeric variables, `Sepal.Length` and `Petal.Length`
from the famous
[`iris`](https://en.wikipedia.org/wiki/Iris_flower_data_set) dataset,
included by default in R.

``` r

model <- lm(Sepal.Length ~ Petal.Length, data = iris)
summary(model)
```


    Call:
    lm(formula = Sepal.Length ~ Petal.Length, data = iris)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -1.2468 -0.2966 -0.0152  0.2768  1.0027 

    Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    4.3066     0.0784    54.9   <2e-16 ***
    Petal.Length   0.4089     0.0189    21.6   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.41 on 148 degrees of freedom
    Multiple R-squared:  0.76,  Adjusted R-squared:  0.758 
    F-statistic:  469 on 1 and 148 DF,  p-value: <2e-16

This analysis suggests that there is a statistically **significant**
(whatever that means) and **positive** (with a coefficient of `0.41`)
linear relationship between the two variables.

Fitting and interpreting the frequentist models is so easy that it is
obvious that people use it instead of the Bayesian framework… right?

**Not anymore.**

### Bayesian linear regression

``` r

model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris)
posteriors <- describe_posterior(model)
# for a nicer table
print_md(posteriors, digits = 2)
```

| Parameter    | Median | 95% CI         | pd   | ROPE            | % in ROPE | Rhat  | ESS  |
|:-------------|-------:|:---------------|:-----|:----------------|----------:|:------|:-----|
| (Intercept)  |   4.30 | \[4.15, 4.46\] | 100% | \[-0.08, 0.08\] |        0% | 1.000 | 4057 |
| Petal.Length |   0.41 | \[0.37, 0.45\] | 100% | \[-0.08, 0.08\] |        0% | 1.000 | 4115 |

Summary of Posterior Distribution {.table}

**That’s it!**

You just fitted a Bayesian version of the model by simply using the
[`stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)
function instead of [`lm()`](https://rdrr.io/r/stats/lm.html) and
described the posterior distributions of the parameters!

The conclusion we draw, for this example, are very similar. The effect
(*the median of the effect’s posterior distribution*) is about `0.41`,
and it can be also be considered as *significant* in the Bayesian sense
(more on that later).

**So, ready to learn more?**

Check out the [**next
tutorial**](https://easystats.github.io/bayestestR/articles/example1.html)!

And, if you want even more, you can check out other articles describing
all the functionality the package has to offer!

<https://easystats.github.io/bayestestR/articles/>

## References

Andrews, M., & Baguley, T. (2013). Prior approval: The growth of
bayesian methods in psychology. *British Journal of Mathematical and
Statistical Psychology*, *66*(1), 1–7.

Benjamin, D. J., Berger, J. O., Johannesson, M., Nosek, B. A.,
Wagenmakers, E.-J., Berk, R., et al.others. (2018). Redefine statistical
significance. *Nature Human Behaviour*, *2*(1), 6.

Chambers, C. D., Feredoes, E., Muthukumaraswamy, S. D., & Etchells, P.
(2014). Instead of ’playing the game’ it is time to change the rules:
Registered reports at AIMS neuroscience and beyond. *AIMS Neuroscience*,
*1*(1), 4–17.

Etz, A., & Vandekerckhove, J. (2016). A bayesian perspective on the
reproducibility project: psychology. *PloS One*, *11*(2), e0149794.

Kruschke, J. K. (2010). What to believe: Bayesian methods for data
analysis. *Trends in Cognitive Sciences*, *14*(7), 293–300.

Kruschke, J. K., Aguinis, H., & Joo, H. (2012). The time has come:
Bayesian methods for data analysis in the organizational sciences.
*Organizational Research Methods*, *15*(4), 722–752.

Szucs, D., & Ioannidis, J. P. (2016). Empirical assessment of published
effect sizes and power in the recent cognitive neuroscience and
psychology literature. *BioRxiv*, 071530.

Wagenmakers, E.-J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., Love,
J., et al.others. (2018). Bayesian inference for psychology. Part i:
Theoretical advantages and practical ramifications. *Psychonomic
Bulletin & Review*, *25*(1), 35–57.
