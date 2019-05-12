#' @export
plot.equivalence_test_br <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("package see needed.")
  }
  NextMethod()
}
