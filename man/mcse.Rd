% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcse.R
\name{mcse}
\alias{mcse}
\alias{mcse.stanreg}
\title{Monte-Carlo Standard Error (MCSE)}
\usage{
mcse(model, ...)

\method{mcse}{stanreg}(
  model,
  effects = c("fixed", "random", "all"),
  component = c("location", "all", "conditional", "smooth_terms", "sigma",
    "distributional", "auxiliary"),
  parameters = NULL,
  ...
)
}
\arguments{
\item{model}{A \code{stanreg}, \code{stanfit}, \code{brmsfit}, \code{blavaan}, or \code{MCMCglmm} object.}

\item{...}{Currently not used.}

\item{effects}{Should results for fixed effects, random effects or both be
returned? Only applies to mixed models. May be abbreviated.}

\item{component}{Should results for all parameters, parameters for the
conditional model or the zero-inflated part of the model be returned? May
be abbreviated. Only applies to \pkg{brms}-models.}

\item{parameters}{Regular expression pattern that describes the parameters
that should be returned. Meta-parameters (like \code{lp__} or \code{prior_}) are
filtered by default, so only parameters that typically appear in the
\code{summary()} are returned. Use \code{parameters} to select specific parameters
for the output.}
}
\description{
This function returns the Monte Carlo Standard Error (MCSE).
}
\details{
\strong{Monte Carlo Standard Error (MCSE)} is another measure of
accuracy of the chains. It is defined as standard deviation of the chains
divided by their effective sample size (the formula for \code{mcse()} is
from Kruschke 2015, p. 187). The MCSE \dQuote{provides a quantitative
suggestion of how big the estimation noise is}.
}
\examples{
\dontshow{if (require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
library(bayestestR)

model <- suppressWarnings(
  rstanarm::stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
)
mcse(model)
}
\dontshow{\}) # examplesIf}
}
\references{
Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
}
