#' @export
print.point_estimate <- function(x, digits = 2, ...) {
  orig_x <- x
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else if ("data.frame" %in% class(x)) {
    insight::print_color("# Point Estimates\n\n", "blue")
    print_data_frame(x, digits = digits)
  } else {
    print(unclass(x))
  }
  invisible(orig_x)
}
