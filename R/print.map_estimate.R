#' @export
print.map_estimate <- function(x, ...) {
  orig_x <- x
  if (inherits(x, "data.frame")) {
    print.data.frame(x)
  } else {
    cat(sprintf("MAP = %.2f", x))
  }
  invisible(orig_x)
}