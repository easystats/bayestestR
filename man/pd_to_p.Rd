% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_pd_to_p.R
\name{pd_to_p}
\alias{pd_to_p}
\alias{pd_to_p.numeric}
\alias{p_to_pd}
\alias{convert_p_to_pd}
\alias{convert_pd_to_p}
\title{Convert between Probability of Direction (pd) and p-value.}
\usage{
pd_to_p(pd, ...)

\method{pd_to_p}{numeric}(pd, direction = "two-sided", verbose = TRUE, ...)

p_to_pd(p, direction = "two-sided", ...)

convert_p_to_pd(p, direction = "two-sided", ...)

convert_pd_to_p(pd, ...)
}
\arguments{
\item{pd}{A Probability of Direction (pd) value (between 0 and 1). Can also
be a data frame with a column named \code{pd}, \code{p_direction}, or \code{PD}, as returned
by \code{\link[=p_direction]{p_direction()}}. In this case, the column is converted to p-values and
the new data frame is returned.}

\item{...}{Arguments passed to or from other methods.}

\item{direction}{What type of p-value is requested or provided. Can be
\code{"two-sided"} (default, two tailed) or \code{"one-sided"} (one tailed).}

\item{verbose}{Toggle off warnings.}

\item{p}{A p-value.}
}
\value{
A p-value or a data frame with a p-value column.
}
\description{
Enables a conversion between Probability of Direction (pd) and p-value.
}
\details{
Conversion is done using the following equation (see \emph{Makowski et al., 2019}):

When \code{direction = "two-sided"}

\ifelse{html}{\out{p = 2 * (1 - p<sub>d</sub>)}}{\eqn{p = 2 \times (1 - p_d)}}

When \code{direction = "one-sided"}

\ifelse{html}{\out{p = 1 - p<sub>d</sub>}}{\eqn{p = 1 - p_d}}

Note that this conversion is only valid when the lowest possible values of pd
is 0.5 - i.e., when the posterior represents continuous parameter space (see
\code{\link[=p_direction]{p_direction()}}). If any pd < 0.5 are detected, they are converted to a p
of 1, and a warning is given.
}
\examples{
pd_to_p(pd = 0.95)
pd_to_p(pd = 0.95, direction = "one-sided")

}
\references{
Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D. (2019).
\emph{Indices of Effect Existence and Significance in the Bayesian Framework}.
Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
}
