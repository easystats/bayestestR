# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md


#' @importFrom insight export_table
#' @export
print_md.describe_posterior <- function(x, digits = 2, ...) {
  cp <- attr(x, "clean_parameters")
  cat(insight::export_table(format(x, cp = cp, digits = digits, format = "md", ...)))
  invisible(x)
}

#' @export
print_md.point_estimate <- print_md.describe_posterior
