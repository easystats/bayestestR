#' Practical Significance (ps)
#'
#' Compute the probability of \strong{Practical Significance} (\strong{\emph{ps}}), which can be conceptualized as a unidirectional equivalence test. It returns the probability that effect is above a given threshold corresponding to a negligible effect. Mathematically, it is defined as the proportion of the posterior distribution of the median sign above the threshold.
#'
#' @inheritParams rope
#' @param threshold The threshold value that separates significant from negligible effect. If \code{"default"}, the range is set to \code{0.1} if input is a vector, and based on \code{\link[=rope_range]{rope_range()}} if a Bayesian model is provided.
#'
#' @return Values between 0.5 and 1 corresponding to the probability of practical significance (ps).
#'
#' @examples
#' library(bayestestR)
#'
#' # Simulate a posterior distribution of mean 1 and SD 1
#' # ----------------------------------------------------
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' p_significance(posterior)
#'
#' @export
p_significance <- function(x, ...) {
  UseMethod("p_significance")
}




#' @rdname p_significance
#' @export
p_significance.numeric <- function(x, threshold = "default", ...) {

  if (all(threshold == "default")) {
    threshold <- 0.1
  } else if (!all(is.numeric(threshold))) {
    stop("`range` should be 'default' or a numeric value (e.g., 0.1).")
  }

  psig <- max(
    c(
      length(x[x > abs(threshold)]) / length(x), # ps positive
      length(x[x < -abs(threshold)]) / length(x) # ps negative
    )
  )

  attr(psig, "threshold") <- threshold
  attr(psig, "data") <- x

  class(psig) <- unique(c("p_significance", "see_p_significance", class(psig)))

  psig
}
