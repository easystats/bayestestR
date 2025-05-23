% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effective_sample.R
\name{effective_sample}
\alias{effective_sample}
\alias{effective_sample.brmsfit}
\title{Effective Sample Size (ESS)}
\usage{
effective_sample(model, ...)

\method{effective_sample}{brmsfit}(
  model,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  ...
)
}
\arguments{
\item{model}{A \code{stanreg}, \code{stanfit}, \code{brmsfit}, \code{blavaan}, or \code{MCMCglmm} object.}

\item{...}{Currently not used.}

\item{effects}{Should variables for fixed effects (\code{"fixed"}), random effects
(\code{"random"}) or both (\code{"all"}) be returned? Only applies to mixed models. May
be abbreviated.

For models of from packages \strong{brms} or \strong{rstanarm} there are additional
options:
\itemize{
\item \code{"fixed"} returns fixed effects.
\item \code{"random_variance"} return random effects parameters (variance and
correlation components, e.g. those parameters that start with \code{sd_} or
\code{cor_}).
\item \code{"grouplevel"} returns random effects group level estimates, i.e. those
parameters that start with \code{r_}.
\item \code{"random"} returns both \code{"random_variance"} and \code{"grouplevel"}.
\item \code{"all"} returns fixed effects and random effects variances.
\item \code{"full"} returns all parameters.
}}

\item{component}{Which type of parameters to return, such as parameters for
the conditional model, the zero-inflated part of the model, the dispersion
term, etc. See details in section \emph{Model Components}. May be abbreviated.
Note that the \emph{conditional} component also refers to the \emph{count} or \emph{mean}
component - names may differ, depending on the modeling package. There are
three convenient shortcuts (not applicable to \emph{all} model classes):
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

\item{parameters}{Regular expression pattern that describes the parameters
that should be returned. Meta-parameters (like \code{lp__} or \code{prior_}) are
filtered by default, so only parameters that typically appear in the
\code{summary()} are returned. Use \code{parameters} to select specific parameters
for the output.}
}
\value{
A data frame with two columns: Parameter name and effective sample size (ESS).
}
\description{
Effective Sample Size (ESS) is a measure of how much independent information
there is in autocorrelated chains. It is used to assess the quality of MCMC
samples. A higher ESS indicates more reliable estimates. For most
applications, an effective sample size greater than 1,000 is sufficient for
stable estimates (Bürkner, 2017). This function returns the effective sample
size (ESS) for various Bayesian model objects. For \code{brmsfit} objects, the
returned ESS corresponds to the bulk-ESS (and the tail-ESS is also returned).
}
\details{
\itemize{
\item \strong{Effective Sample (ESS)} should be as large as possible, altough
for most applications, an effective sample size greater than 1,000 is
sufficient for stable estimates (Bürkner, 2017). The ESS corresponds to the
number of independent samples with the same estimation power as the N
autocorrelated samples. It is is a measure of \dQuote{how much independent
information there is in autocorrelated chains} (\emph{Kruschke 2015, p182-3}).
\item \strong{Bulk-ESS} is useful as a diagnostic for the sampling efficiency in
the bulk of the posterior. It is defined as the effective sample size for
rank normalized values using split chains. It can be interpreted as the
reliability of indices of central tendency (mean, median, etc.).
\item \strong{Tail-ESS} is useful as a diagnostic for the sampling efficiency in
the tails of the posterior. It is defined as the minimum of the effective
sample sizes for 5\% and 95\% quantiles. It can be interpreted as the
reliability of indices that depend on the tails of the distribution (e.g.,
credible intervals, tail probabilities, etc.).
}
}
\section{Model components}{


Possible values for the \code{component} argument depend on the model class.
Following are valid options:
\itemize{
\item \code{"all"}: returns all model components, applies to all models, but will only
have an effect for models with more than just the conditional model
component.
\item \code{"conditional"}: only returns the conditional component, i.e. "fixed
effects" terms from the model. Will only have an effect for models with
more than just the conditional model component.
\item \code{"smooth_terms"}: returns smooth terms, only applies to GAMs (or similar
models that may contain smooth terms).
\item \code{"zero_inflated"} (or \code{"zi"}): returns the zero-inflation component.
\item \code{"location"}: returns location parameters such as \code{conditional},
\code{zero_inflated}, or \code{smooth_terms} (everything that are fixed or random
effects - depending on the \code{effects} argument - but no auxiliary
parameters).
\item \code{"distributional"} (or \code{"auxiliary"}): components like \code{sigma},
\code{dispersion}, \code{beta} or \code{precision} (and other auxiliary parameters) are
returned.
}

For models of class \code{brmsfit} (package \strong{brms}), even more options are
possible for the \code{component} argument, which are not all documented in detail
here. See also \href{https://easystats.github.io/insight/reference/find_parameters.BGGM.html}{\code{?insight::find_parameters}}.
}

\examples{
\dontshow{if (all(insight::check_if_installed(c("rstanarm", "brms", "posterior"), quietly = TRUE))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
model <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt + gear,
  data = mtcars,
  chains = 2,
  iter = 200,
  refresh = 0
))
effective_sample(model)

model <- suppressWarnings(brms::brm(
  mpg ~ wt,
  data = mtcars,
  chains = 2,
  iter = 200,
  refresh = 0
))
effective_sample(model)
}
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS,
and Stan. Academic Press.
\item Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models
using Stan. Journal of Statistical Software, 80(1), 1-28
\item Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
(2021). Rank-normalization, folding, and localization: An improved R-hat
for assessing convergence of MCMC. Bayesian Analysis, 16(2), 667-718.
}
}
