# Reexports models ------------------------

#' @importFrom insight print_html
#' @export
insight::print_html


#' @importFrom insight export_table
#' @export
print_html.describe_posterior <- function(x, digits = 2, caption = NULL, ...) {
  cp <- attr(x, "clean_parameters")
  cat(insight::export_table(format(x, cp = cp, digits = digits, format = "html", ...), caption = caption))
  invisible(x)
}

#' @export
print_html.point_estimate <- print_html.describe_posterior
