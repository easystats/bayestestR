# print.point_estimate <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else if ("data.frame" %in% class(x)) {
#     insight::print_color("# Point Estimates\n\n", "blue")
#     print_data_frame(x, digits = digits)
#   } else {
#     print(unclass(x))
#   }
#   invisible(orig_x)
# }


#' @export
format.point_estimate <- function(x, cp, digits = 2, format = "text", ...) {
  out <- insight::format_table(x, digits = 2, format = format, ...)
  if (!is.null(cp) && !all(is.na(match(cp$Parameter, out$Parameter)))) {
    out <- insight::print_parameters(cp, out, keep_parameter_column = FALSE, remove_empty_column = TRUE)
  }
  out
}


#' @export
print.point_estimate <- function(x, digits = 2, ...) {
  cp <- attr(x, "clean_parameters")
  cat(insight::export_table(format(x, cp = cp, digits = digits, format = "text", ...)))
  invisible(x)
}
