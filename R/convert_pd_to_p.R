#' Convert between Probability of Direction (pd) and p-value.
#'
#' Enables a conversion between Probability of Direction (pd) and p-value.
#'
#' @param pd A Probability of Direction (pd) value (between 0 and 1).
#' @param p A p-value.
#' @param direction What type of p-value is requested or provided. Can be \code{"two-sided"} (default, two tailed) or \code{"one-sided"} (one tailed).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' pd_to_p(pd = 0.95)
#' pd_to_p(pd = 0.95, direction = "one-sided")
#' @export
pd_to_p <- function(pd, direction = "two-sided", ...) {
  p <- (1 - pd)
  if (.get_direction(direction) == 0) {
    p <- 2 * p
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
