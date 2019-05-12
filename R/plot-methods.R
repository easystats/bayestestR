#' @export
plot.equivalence_test <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from equivalence-test. Please install it.")
  }
  NextMethod()
}


#' @export
plot.p_direction <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from p_direction(). Please install it.")
  }
  NextMethod()
}


#' @export
plot.rope <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot ROPE. Please install it.")
  }
  NextMethod()
}
