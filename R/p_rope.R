#' Probability of not being in ROPE
#'
#' Compute the proportion of the posterior distribution that doesn't lie within a region of practical equivalence (ROPE). It is equivalent to running \code{rope(..., ci = 1)}.
#'
#' @inheritParams rope
#'
#' @examples
#' library(bayestestR)
#'
#' rope(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' @export
p_rope <- function(x, ...) {
  UseMethod("p_rope")
}


#' @method as.double p_rope
#' @export
as.double.p_rope <- function(x, ...) {
  x
}



#' @rdname p_rope
#' @export
p_rope.default <- function(x, ...) {
  NULL
}


#' @rdname p_rope
#' @export
p_rope.numeric <- function(x, range = "default", ...) {
  rope(x, range = range, ci = 1, ...)$ROPE_Percentage
}