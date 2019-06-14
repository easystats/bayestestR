---
title: 'Understanding and Describing Bayesian Models with bayestestR'
tags:
  - R
  - Bayesian statistics
  - rstan
  - eaystats
  - posterior distribution
  - Region of practical equivalence
  - ROPE
  - probability of direction
  - Bayes factor
authors:
  - name: Dominique Makowski
    orcid: 0000-0001-5375-9967
    affiliation: 1
  - name: Mattan S. Ben-Shachar
    orcid: 0000-0002-4287-4801
    affiliation: 2
  - name: Daniel Lüdecke
    orcid: 0000-0002-8895-3206
    affiliation: 3
affiliations:
  - name: Nanyang Technological University, Singapore
    index: 1
  - name: Ben-Gurion University of the Negev, Israel
    index: 2
  - name: University Medical Center Hamburg-Eppendorf, Germany
    index: 3
date: 09 June 2019
bibliography: paper.bib
---

# Introduction

The Bayesian framework for statistics in quickly gaining in popularity among scientists, associated with the general shift towards open and honest science. Reasons to prefer this approach are reliability, accuracy (in noisy data and small samples), the possibility of introducing prior knowledge into the analysis and, critically, results intuitiveness and their straightforward interpretation [@andrews2013prior; @etz2016bayesian; @kruschke2010believe; @kruschke2012time; @wagenmakers2018bayesian]. 

In general, the frequentist approach has been associated with the focus on null hypothesis testing, and the misuse of *p*-values has been shown to critically contribute to the reproducibility crisis of psychological science [@chambers2014instead; @szucs2016empirical]. There is a general agreement that the generalization of the Bayesian approach is one way of overcoming these issues [@benjamin2018redefine; @etz2016bayesian].

Importantly, adopting the Bayesian framework is more of a shift in the paradigm than a change in the methodology. Indeed, all the common statistical procedures (t-tests, correlations, ANOVAs, regressions, ...) can be achieved using the Bayesian framework. One of the core difference is that in the *frequentist* view, the effects are fixed (but unknown) and data are random. On the contrary, the Bayesian inference process computes the probability of different effects *given the observed data*. Instead of having one estimated value of the "true effect", this probabilistic approach gives a distribution of values, called the *posterior distribution*. 

Bayesian’s uncertainty can be summarized, for instance, by giving the *median* of the distribution, as well as a range of values on the posterior distribution that includes the 95% most probable values (the 95\% *Credible* Interval). To illustrate the difference of interpretation, the Bayesian framework allows to say *"given the observed data, the effect has 95% probability of falling within this range"*, while the frequentist less straightforward alternative (the 95\% *Confidence* Interval) would be *"there is a 95\% probability that when computing a confidence interval from data of this sort, the effect falls within this range"*. 

In other words, frequentists try to estimate the *"real effect"*. For instance, the "real" value of the correlation between *x* and *y*. Hence, frequentist models return a "point-estimate" (*i.e.*, a single value) of the "real" correlation (*e.g.*, r = 0.42) estimated under a number of assumptions (*e.g.*, considering that the data is sampled at random from a "parent", usually normal distribution). On the contrary, Bayesian users assume no such thing. The data are what they are. Based on this observed data (and a *prior* belief about the result), the Bayesian sampling algorithm (*e.g.*, the MCMC sampling) returns a probability distribution (*the posterior*) of the effect that is compatible with the observed data. For the correlation between *x* and *y*, it will return a distribution that says, for example, "the most probable effect is 0.42, but this data is also compatible with correlations of 0.12 and 0.74". To characterize the effects, Bayesian users simply describe the posterior distribution of the effect, by reporting for example the median, the [89% *Credible* Interval](https://easystats.github.io/bayestestR/articles/credible_interval.html) or [other indices](https://easystats.github.io/bayestestR/articles/guidelines.html).

The `bayestestR` package provides tools to compute and report these indices from a variety of models objects, including popular modeling packages such as `rstanarm` [@goodrich2018rstanarm], `brms`[@burkner2017brms] or `BayesFactor`[@morey2014bayesfactor]. The main functions are described below.


# Features


# Licensing and Availability

**bayestestR** is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (https://github.com/easystats/bayestestR), with a corresponding issue tracker for bug-reporting and feature enhancements. In the spirit of open science and research, we encourage interaction with our package through requests/tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

We would like to thank the [easystats team](https://github.com/orgs/easystats/people), all other contributors as well as the users.

# References