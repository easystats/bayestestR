#' @export
print.p_rope <- function(x, digits = 2, ...) {
  orig_x <- x
  print.data.frame(x, digits = digits, ...) # TODO
  invisible(orig_x)
}

