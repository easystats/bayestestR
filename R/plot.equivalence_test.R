#' @export
plot.equivalence_test_br <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from equivalence-test. Please install it.")
  }
  NextMethod()
}
