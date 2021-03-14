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
}


#' @export
print_html.point_estimate <- print_html.describe_posterior


#' @export
print_html.p_rope <- function(x, digits = 2, ...) {
  cp <- attr(x, "clean_parameters")
  caption <- sprintf("Proportion of samples inside the ROPE [%.*f, %.*f]",
                     digits, x$ROPE_low[1], digits, x$ROPE_high[1])

  x$ROPE_low <- x$ROPE_high <- NULL
  formatted_table <- format(x, cp = cp, digits = digits, format = "html", ci_string = "ROPE", ...)

  if (length(formatted_table) == 1) {
    attr(formatted_table[[1]], "table_caption") <- caption
    attr(formatted_table[[1]], "table_subtitle") <- NULL
  }

  insight::export_table(formatted_table, format = "html")
}


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
    attr(formatted_table[[1]], "table_caption") <- caption
    attr(formatted_table[[1]], "table_subtitle") <- NULL
  }

  insight::export_table(
    formatted_table,
    caption = caption,
    format = "html"
  )
}
