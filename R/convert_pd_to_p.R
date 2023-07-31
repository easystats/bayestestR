#' Convert between Probability of Direction (pd) and p-value.
#'
#' Enables a conversion between Probability of Direction (pd) and p-value.
#'
#' @param pd A Probability of Direction (pd) value (between 0 and 1).
#' @param p A p-value.
#' @param direction What type of p-value is requested or provided. Can be
#'   `"two-sided"` (default, two tailed) or `"one-sided"` (one tailed).
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#' Conversion is done using the following equation (see Makowski et al., 2019):
#' \cr\cr
#' When `direction = "two-sided"` -
#' \cr\cr
#' \deqn{p = 2 \times (1 - p_d)}{p = 2 * (1 - pd)}
#' When `direction = "one-sided"` -
#' \cr\cr
#' \deqn{p = 1 - p_d}{p = 1 - pd}
#' \cr\cr
#' Note that this conversion is only valid when the lowest possible values of pd
#' is 0.5 - i.e., when the posterior represents continuous parameter space (see
#' [p_direction]). If any pd < 0.5 are detected, they are converted to a p of 1,
#' and a warning is given.
#'
#' @references
#' Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D. (2019).
#' *Indices of Effect Existence and Significance in the Bayesian Framework*.
#' Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
#'
#' @examples
#' pd_to_p(pd = 0.95)
#' pd_to_p(pd = 0.95, direction = "one-sided")
#'
#' @export
pd_to_p <- function(pd, direction = "two-sided", verbose = TRUE, ...) {
  p <- 1 - pd
  if (.get_direction(direction) == 0) {
    p <- 2 * p
  }

  less_than_0.5 <- pd < 0.5
  if (any(less_than_0.5)) {
    if (verbose) {
      insight::format_warning(paste(
        "pd values smaller than 0.5 detected.",
        "pd-to-p conversion assumes a continious parameter space;",
        "see help('p_direction') for more info."
      ))
    }
    p[less_than_0.5] <- 1
  }

  p
}


#' @rdname pd_to_p
#' @export
p_to_pd <- function(p, direction = "two-sided", ...) {
  if (.get_direction(direction) == 0) {
    p <- p / 2
  }
  (1 - p)
}



#' @rdname pd_to_p
#' @export
convert_p_to_pd <- p_to_pd

#' @rdname pd_to_p
#' @export
convert_pd_to_p <- pd_to_p
