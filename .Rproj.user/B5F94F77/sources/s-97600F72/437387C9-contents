#' Highest Density Interval (HDI)
#'
#' Compute the Highest Density Interval (HDI) of a posterior distribution, i.e., the interval which contains all points within the interval have a higher probability density than points outside the interval. The HDI is used in the context of Bayesian posterior characterisation as Credible Interval (CI).
#'
#' @details By default, hdi() returns the 90\% intervals, deemed to be more stable than, for instance, 95\% intervals (Kruschke, 2015).
#'
#' @param posterior vector representing a posterior distribution.
#' @param CI the credible interval, value or vector between 0 and 100, indicating the probability that is to be estimated.
#' @param verbose toggle off warnings.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' hdi(posterior, CI = 90)
#' hdi(posterior, CI = c(80, 90, 95))
#' @author All credits go to \href{https://rdrr.io/cran/ggdistribute/src/R/stats.R}{ggdistribute}.
#' @references Kruschke, J. (2015). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#' @export
hdi <- function(posterior, CI = 90, verbose = TRUE) {
  if (length(CI) > 1) {
    hdi_values <- list()
    for (CI_value in CI) {
      hdi_values[[paste0("CI_", CI_value)]] <- .hdi(posterior, CI = CI_value, verbose = verbose)
    }
    return(hdi_values)
  } else {
    return(.hdi(posterior, CI = CI, verbose = verbose))
  }
}











#' @keywords internal
.hdi <- function(x, CI = 90, verbose = TRUE) {
  CI <- CI / 100
  if (CI > 1) {
    if (verbose) {
      warning("HDI: `CI` should be less than 100, returning NaNs.")
    }
    return(c(NA, NA))
  }


  if (CI == 1) {
    return(c(min(x), max(x)))
  }

  if (anyNA(x)) {
    if (verbose) {
      warning("HDI: the posterior contains NaNs, returning NaNs.")
    }
    return(c(NA, NA))
  }

  N <- length(x)
  if (N < 3) {
    if (verbose) {
      warning("HDI: the posterior is too short, returning NaNs.")
    }
    return(c(NA, NA))
  }

  x_sorted <- sort(x)
  window_size <- floor(CI * length(x_sorted))

  if (window_size < 2) {
    if (verbose) {
      warning("HDI: `CI` is too small or x does not contain enough data points, returning NaNs.")
    }
    return(c(NA, NA))
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
  c(x_sorted[min_i], x_sorted[upper[min_i]])
}
