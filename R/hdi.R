#' Highest Density Interval (HDI)
#'
#' Compute the Highest Density Interval (HDI) of a posterior distribution, i.e., the interval which contains all points within the interval have a higher probability density than points outside the interval.
#'
#' @details By default, hdi() returns the 90\% intervals, deemed to be more stable than, for instance, 95\% intervals (Kruschke 2015).
#'
#' @param posterior vector representing a posterior distribution.
#' @param prob value or vector between 0 and 1, indicating the interval that is to be estimated.
#' @param verbose toggle off warnings.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' hdi(posterior, prob = 0.9)
#' hdi(posterior, prob = c(0.8, 0.9, 0.95))
#' @author All credits go to \href{https://rdrr.io/cran/ggdistribute/src/R/stats.R}{ggdistribute}.
#'
#' @export
hdi <- function(posterior, prob = 0.90, verbose = TRUE) {
  if (length(prob) > 1) {
    hdi_values <- list()
    for (prob_value in prob) {
      hdi_values[[as.character(prob_value)]] <- .hdi(posterior, prob = prob_value, verbose = verbose)
    }
    return(hdi_values)
  } else {
    return(.hdi(posterior, prob = prob, verbose = verbose))
  }
}











#' @keywords internal
.hdi <- function(x, prob = 0.90, verbose = TRUE) {
  if (prob > 1) {
    if (verbose) {
      warning("HDI: `prob` should be less than 1, returning NaNs.")
    }
    return(c(NA, NA))
  }


  if(prob == 1){
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
  window_size <- floor(prob * length(x_sorted))

  if (window_size < 2) {
    if (verbose) {
      warning("HDI: `prob` is too small or x does not contain enough data points, returning NaNs.")
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
