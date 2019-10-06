#' @export
print.p_direction <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else if ("data.frame" %in% class(x)) {
    .print_pd(x, digits, ...)
  } else {
    cat(sprintf("pd = %s%%", insight::format_value(x * 100, digits = digits)))
  }
}

#' @keywords internal
.print_pd <- function(x, digits, ...) {
  insight::print_color("# Probability of Direction (pd)\n\n", "blue")
  x$Parameter <- as.character(x$Parameter)
  x$pd <- sprintf("%s%%", insight::format_value(x$pd * 100, digits = digits))
  print_data_frame(x, digits = digits)
}
