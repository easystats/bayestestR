#' @export
print.p_rope <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else if ("data.frame" %in% class(x)) {
    insight::print_color("# ROPE-based p-value\n\n", "blue")
    print_data_frame(x, digits = digits)
  } else {
    cat(sprintf("p (ROPE) = %.*f%%", digits, x * 100))
  }
}
