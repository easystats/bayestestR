#' Effective Sample Size (ESS)
#'
#' This function returns the effective sample size (ESS).
#'
#' @param model A \code{stanreg}, \code{stanfit}, or \code{brmsfit} object.
#' @param ... Currently not used.
#' @inheritParams hdi
#'
#' @return A data frame with two columns: Parameter name and effective sample size (ESS).
#'
#' @details \strong{Effective Sample (ESS)} should be as large as possible, altough for most applications, an effective sample size greater than 1,000 is sufficient for stable estimates (Bürkner, 2017). The ESS corresponds to the number of independent samples with the same estimation power as the N autocorrelated samples. It is is a measure of \dQuote{how much independent information there is in autocorrelated chains} (\emph{Kruschke 2015, p182-3}).
#'
#' @references \itemize{
#'   \item Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#'   \item Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. Journal of Statistical Software, 80(1), 1-28
#' }
#'
#' @examples
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#' effective_sample(model)
#' @export
effective_sample <- function(model, ...) {
  UseMethod("effective_sample")
}


#' @rdname effective_sample
#' @export
effective_sample.brmsfit <- function(model, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  pars <-
    insight::get_parameters(
      model,
      effects = effects,
      component = component,
      parameters = parameters
    )

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' required for this function to work. Please install it.")
  }

  s <- rstan::summary(model$fit)$summary
  s <- s[make.names(rownames(s)) %in% colnames(pars), ]

  data.frame(
    Parameter = rownames(s),
    ESS = round(s[, "n_eff"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}



#' @rdname effective_sample
#' @export
effective_sample.stanreg <- function(model, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  # check arguments
  effects <- match.arg(effects)

  pars <-
    insight::get_parameters(
      model,
      effects = effects,
      parameters = parameters
    )

  s <- as.data.frame(summary(model))
  s <- s[rownames(s) %in% colnames(pars), ]

  data.frame(
    Parameter = rownames(s),
    ESS = s[["n_eff"]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
