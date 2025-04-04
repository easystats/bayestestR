% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_priors.R
\name{simulate_prior}
\alias{simulate_prior}
\alias{simulate_prior.brmsfit}
\title{Returns Priors of a Model as Empirical Distributions}
\usage{
simulate_prior(model, n = 1000, ...)

\method{simulate_prior}{brmsfit}(
  model,
  n = 1000,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{model}{A \code{stanreg}, \code{stanfit}, \code{brmsfit}, \code{blavaan}, or \code{MCMCglmm} object.}

\item{n}{Size of the simulated prior distributions.}

\item{...}{Currently not used.}

\item{effects}{Should results for fixed effects (\code{"fixed"}, the default),
random effects (\code{"random"}) or both ("\verb{all"}) be returned? Only applies to
mixed models. May be abbreviated.}

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

\item{verbose}{Toggle off warnings.}
}
\description{
Transforms priors information to actual distributions.
}
\examples{
\donttest{
library(bayestestR)
if (require("rstanarm")) {
  model <- suppressWarnings(
    stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
  )
  simulate_prior(model)
}
}
}
\seealso{
\code{\link[=unupdate]{unupdate()}} for directly sampling from the prior
distribution (useful for complex priors and designs).
}
