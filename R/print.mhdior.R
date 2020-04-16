#' @export
print.mhdior <- function(x, digits = 2, ...) {
  orig_x <- x
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else if ("data.frame" %in% class(x)) {
    insight::print_color("# Max HDI inside/outside ROPE (MHDIOR)\n\n", "blue")
    print_data_frame(x, digits = digits)
  } else {
    cat(sprintf("MHDIOR = %.*f%%", digits, x * 100))
  }
  invisible(orig_x)
}
