% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostic_posterior.R
\name{diagnostic_posterior}
\alias{diagnostic_posterior}
\alias{diagnostic_posterior.default}
\alias{diagnostic_posterior.stanreg}
\alias{diagnostic_posterior.brmsfit}
\title{Posteriors Sampling Diagnostic}
\usage{
diagnostic_posterior(posterior, ...)

\method{diagnostic_posterior}{default}(posterior, diagnostic = c("ESS", "Rhat"), ...)

\method{diagnostic_posterior}{stanreg}(
  posterior,
  diagnostic = "all",
  effects = c("fixed", "random", "all"),
  component = c("location", "all", "conditional", "smooth_terms", "sigma",
    "distributional", "auxiliary"),
  parameters = NULL,
  ...
)

\method{diagnostic_posterior}{brmsfit}(
  posterior,
  diagnostic = "all",
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL,
  ...
)
}
\arguments{
\item{posterior}{A \code{stanreg}, \code{stanfit}, \code{brmsfit}, or \code{blavaan} object.}

\item{...}{Currently not used.}

\item{diagnostic}{Diagnostic metrics to compute.  Character (vector) or list
with one or more of these options: \code{"ESS"}, \code{"Rhat"}, \code{"MCSE"} or \code{"all"}.}

\item{effects}{Should variables for fixed effects (\code{"fixed"}), random effects
(\code{"random"}) or both (\code{"all"}) be returned? Only applies to mixed models. May
be abbreviated.}

\item{component}{Which type of parameters to return, such as parameters for
the conditional model, the zero-inflated part of the model, the dispersion
term, the instrumental variables or marginal effects be returned? Applies to
models with zero-inflated and/or dispersion formula, or to models with
instrumental variables (so called fixed-effects regressions), or models with
marginal effects (from \strong{mfx}). See details in section \emph{Model Components}
.May be abbreviated. Note that the \emph{conditional} component also refers to the
\emph{count} or \emph{mean} component - names may differ, depending on the modeling
package. There are three convenient shortcuts (not applicable to \emph{all} model
classes):
\itemize{
\item \code{component = "all"} returns all possible parameters.
\item If \code{component = "location"}, location parameters such as \code{conditional},
\code{zero_inflated}, \code{smooth_terms}, or \code{instruments} are returned (everything
that are fixed or random effects - depending on the \code{effects} argument -
but no auxiliary parameters).
\item For \code{component = "distributional"} (or \code{"auxiliary"}), components like
\code{sigma}, \code{dispersion}, \code{beta} or \code{precision} (and other auxiliary
parameters) are returned.
}}

\item{parameters}{Regular expression pattern that describes the parameters that
should be returned.}
}
\description{
Extract diagnostic metrics (Effective Sample Size (\code{ESS}), \code{Rhat} and Monte
Carlo Standard Error \code{MCSE}).
}
\details{
\strong{Effective Sample (ESS)} should be as large as possible, although for
most applications, an effective sample size greater than 1000 is sufficient
for stable estimates (\emph{Bürkner, 2017}). The ESS corresponds to the number of
independent samples with the same estimation power as the N autocorrelated
samples. It is is a measure of "how much independent information there is
in autocorrelated chains" (\emph{Kruschke 2015, p182-3}).

\strong{Rhat} should be the closest to 1. It should not be larger than 1.1
(\emph{Gelman and Rubin, 1992}) or 1.01 (\emph{Vehtari et al., 2019}). The split
Rhat statistic quantifies the consistency of an ensemble of Markov chains.

\strong{Monte Carlo Standard Error (MCSE)} is another measure of accuracy of the
chains. It is defined as standard deviation of the chains divided by their
effective sample size (the formula for \code{mcse()} is from Kruschke 2015, p.
187). The MCSE "provides a quantitative suggestion of how big the estimation
noise is".
}
\examples{
\dontshow{if (require("rstanarm") && require("brms")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
# rstanarm models
# -----------------------------------------------
model <- suppressWarnings(
  rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
)
diagnostic_posterior(model)

# brms models
# -----------------------------------------------
model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
diagnostic_posterior(model)
}
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation
using multiple sequences. Statistical science, 7(4), 457-472.
\item Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., and Bürkner, P. C.
(2019). Rank-normalization, folding, and localization: An improved Rhat
for assessing convergence of MCMC. arXiv preprint arXiv:1903.08008.
\item Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R,
JAGS, and Stan. Academic Press.
}
}
