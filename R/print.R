#' @importFrom insight export_table
#' @export
print.describe_posterior <- function(x, digits = 2, ...) {
  cp <- attr(x, "clean_parameters")
  cat(insight::export_table(format(x, cp = cp, digits = digits, format = "text", ...)))
  invisible(x)
}


#' @export
print.point_estimate <- print.describe_posterior






# print.describe_posterior <- function(x, digits = 3, ...) {
#   print_data_frame(format(x, digits = digits, ...), digits = digits, ...)
#   invisible(x)
# }
