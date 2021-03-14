# Reexports models ------------------------

#' @importFrom insight print_html
#' @export
insight::print_html


#' @importFrom insight export_table
#' @export
print_html.describe_posterior <- function(x, digits = 2, caption = NULL, ...) {
  cp <- attr(x, "clean_parameters")
  insight::export_table(
    format(x, cp = cp, digits = digits, format = "html", ...),
    caption = caption,
    format = "html"
  )
  invisible(x)
}


#' @export
print_html.point_estimate <- print_html.describe_posterior

#' @export
print_html.p_rope <- print_html.describe_posterior

#' @export
print_html.bayestestR_hdi <- function(x, digits = 2, ...) {
  .print_html_ci(x = x, digits = digits, caption = "Highest Density Interval", ci_string = "HDI", ...)
}

#' @export
print_html.bayestestR_eti <- function(x, digits = 2, ...) {
  .print_html_ci(x = x, digits = digits, caption = "Equal-Tailed Interval", ci_string = "ETI", ...)
}

#' @export
print_html.bayestestR_si <- function(x, digits = 2, ...) {
  .print_html_ci(x = x, digits = digits, caption = "Support Interval", ci_string = "SI", ...)
}





# util ---------------


.print_html_ci <- function(x, digits = 2, caption = "Highest Density Interval", ci_string = "HDI", ...) {
  cp <- attr(x, "clean_parameters")
  formatted_table <- format(x, cp = cp, digits = digits, format = "html", ci_string = ci_string, ...)

  # in case we have no multiple components, just use "Highest Density Interval" as caption
  if (length(formatted_table) == 1) {
    attr(formatted_table[[1]], "table_caption") <- c(caption, "blue")
  }

  insight::export_table(
    formatted_table,
    caption = caption,
    format = "html"
  )
  invisible(x)
}
