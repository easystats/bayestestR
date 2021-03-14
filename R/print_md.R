# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md


#' @importFrom insight export_table
#' @export
print_md.describe_posterior <- function(x, digits = 2, caption = NULL, ...) {
  cp <- attr(x, "clean_parameters")
  cat(insight::export_table(
    format(x, cp = cp, digits = digits, format = "md", ...),
    caption = caption,
    format = "md"
  ))
  invisible(x)
}


#' @export
print_md.point_estimate <- print_md.describe_posterior


#' @export
print_md.bayestestR_hdi <- function(x, digits = 2, ...) {
  .print_md_ci(x = x, digits = digits, caption = "Highest Density Interval", ci_string = "HDI", ...)
}


#' @export
print_md.bayestestR_eti <- function(x, digits = 2, ...) {
  .print_md_ci(x = x, digits = digits, caption = "Equal-Tailed Interval", ci_string = "ETI", ...)
}





# util ---------------


.print_md_ci <- function(x, digits = 2, caption = "Highest Density Interval", ci_string = "HDI", ...) {
  cp <- attr(x, "clean_parameters")
  formatted_table <- format(x, cp = cp, digits = digits, format = "md", ci_string = ci_string, ...)

  # in case we have no multiple components, just use "Highest Density Interval" as caption
  if (length(formatted_table) == 1) {
    attr(formatted_table[[1]], "table_caption") <- c(caption, "blue")
  }

  cat(insight::export_table(
    formatted_table,
    caption = caption,
    format = "md"
  ))
  invisible(x)
}
