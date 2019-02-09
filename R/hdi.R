#' Highest Density Interval (HDI)
#'
#' Compute the Highest Density Interval (HDI) of a posterior distribution, i.e., the interval which contains all points within the interval have a higher probability density than points outside the interval. The HDI is used in the context of Bayesian posterior characterisation as Credible Interval (CI).
#'
#' @details By default, hdi() returns the 90\% intervals (\code{ci = 0.9}), deemed to be more stable than, for instance, 95\% intervals (Kruschke, 2015).
#'
#' @param posterior Vector representing a posterior distribution. Can also be a `stanreg` or `brmsfit` model.
#' @param ci Value or vector of HDI probability (between 0 and 1) to be estimated. Named Credible Interval (CI) for consistency.
#' @param verbose Toggle off warnings.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' hdi(posterior, ci = .90)
#' hdi(posterior, ci = c(.80, .90, .95))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' hdi(model)
#' hdi(model, ci = c(.80, .90, .95))
#'
#' # Will fail until get_predictors is implemented.
#' # library(brms)
#' # model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' # hdi(model)
#' # hdi(model, ci = c(.80, .90, .95))
#' }
#'
#' @author All credits go to \href{https://rdrr.io/cran/ggdistribute/src/R/stats.R}{ggdistribute}.
#' @references Kruschke, J. (2015). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#' @export
hdi <- function(posterior, ci = .90, verbose = TRUE) {
  UseMethod("hdi")
}


#' @export
hdi.numeric <- function(posterior, ci = .90, verbose = TRUE) {
  hdi_values <- lapply(ci, function(i) {
    .hdi(posterior, ci = i, verbose = verbose)
  })

  return(flatten_list(hdi_values))
}








#' @importFrom insight get_parameters
#' @keywords internal
.hdi_models <- function(posterior, ci = .90, verbose = TRUE) {
  list <- sapply(insight::get_parameters(posterior), hdi, ci = ci, verbose = verbose, simplify = FALSE)
  return(flatten_list(list, name = "Parameter"))
}

#' @export
hdi.stanreg <- .hdi_models

#' @export
hdi.brmsfit <- .hdi_models













#' @keywords internal
.hdi <- function(x, ci = .90, verbose = TRUE) {

  if (ci > 1) {
    if (verbose) {
      warning("HDI: `ci` should be less than 1, returning NaNs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }


  if (ci == 1) {
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = min(x),
      "CI_high" = max(x)
    ))
  }

  if (anyNA(x)) {
    if (verbose) {
      warning("HDI: the posterior contains NaNs, returning NaNs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  N <- length(x)
  if (N < 3) {
    if (verbose) {
      warning("HDI: the posterior is too short, returning NaNs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  x_sorted <- sort(x)
  window_size <- floor(ci * length(x_sorted))

  if (window_size < 2) {
    if (verbose) {
      warning("HDI: `ci` is too small or x does not contain enough data points, returning NaNs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  lower <- seq_len(N - window_size)
  upper <- window_size + lower

  # vectorized difference between edges of cumulative distribution based on scan_length. Values are arranged from left to right scanning.
  window_width_diff <- x_sorted[upper] - x_sorted[lower]

  # find minimum of width differences, check for multiple minima
  min_i <- which(window_width_diff == min(window_width_diff))
  n_candies <- length(min_i)

  if (n_candies > 1) {
    if (any(diff(sort(min_i)) != 1)) {
      if (verbose) {
        warning("HDI: Identical densities found along different segments of the distribution, choosing rightmost.")
      }
      min_i <- max(min_i)
    } else {
      min_i <- floor(mean(min_i))
    }
  }

  # get values based on minimum
  out <- data.frame(
    "CI" = ci * 100,
    "CI_low" = x_sorted[min_i],
    "CI_high" = x_sorted[upper[min_i]]
  )
  # class(out) <- c("CI", class(out))
  return(out)
}
