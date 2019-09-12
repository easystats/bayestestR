#' @export
print.p_map <- function(x, digits = 3, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else if ("data.frame" %in% class(x)) {
    insight::print_color("# MAP-based p-value\n\n", "blue")
    print_data_frame(x, digits = digits)
  } else {
    cat(sprintf("p (MAP) = %.*f", digits, x))
  }
}
