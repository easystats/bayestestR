#' @export
print.p_significance <- function(x, digits = 2, ...) {
  orig_x <- x
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else if ("data.frame" %in% class(x)) {
    .print_ps(x, digits, ...)
  } else {
    cat(sprintf(
      "ps [%s] = %s%%",
      insight::format_value(attributes(x)$threshold, digits = digits),
      insight::format_value(x * 100, digits = digits)
    ))
  }
  invisible(orig_x)
}

#' @keywords internal
.print_ps <- function(x, digits, ...) {
  insight::print_color(sprintf(
    "# Probability of Significance (ps [%s])\n\n",
    insight::format_value(attributes(x)$threshold, digits = digits)
  ), "blue")
  x$Parameter <- as.character(x$Parameter)
  x$ps <- sprintf("%s%%", insight::format_value(x$ps * 100, digits = digits))
  print_data_frame(x, digits = digits)
}
