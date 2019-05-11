#' @keywords internal
.select_nums <- function(x) {
  x[, unlist(lapply(x, is.numeric))]
}
