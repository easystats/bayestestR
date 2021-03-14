#' @importFrom insight export_table
#' @export
print.describe_posterior <- function(x, digits = 2, caption = NULL, ...) {
  cp <- attr(x, "clean_parameters")
  formatted_table <- format(x, cp = cp, digits = digits, format = "text", ...)
  cat(insight::export_table(
    formatted_table,
    caption = caption
  ))
  invisible(x)
}


#' @export
print.point_estimate <- print.describe_posterior


#' @export
print.p_rope <- function(x, digits = 2, ...) {
  cp <- attr(x, "clean_parameters")
  caption <- sprintf("# Proportion of samples inside the ROPE [%.*f, %.*f]",
                     digits, x$ROPE_low[1], digits, x$ROPE_high[1])

  x$ROPE_low <- x$ROPE_high <- NULL

  formatted_table <- format(x, cp = cp, digits = digits, format = "text", ci_string = "ROPE", ...)
  attr(formatted_table[[1]], "table_caption") <- c(caption, "blue")

  cat(insight::export_table(formatted_table))
  invisible(x)
}


#' @export
print.bayestestR_hdi <- function(x, digits = 2, ...) {
  .print_ci(x = x, digits = digits, caption = "# Highest Density Interval", ci_string = "HDI", ...)
}


#' @export
print.bayestestR_eti <- function(x, digits = 2, ...) {
  .print_ci(x = x, digits = digits, caption = "# Equal-Tailed Interval", ci_string = "ETI", ...)
}


#' @export
print.bayestestR_si <- function(x, digits = 2, ...) {
  .print_ci(x = x, digits = digits, caption = "# Support Interval", ci_string = "SI", ...)
}





# util ---------------------

.print_ci <- function(x, digits = 2, caption = "# Highest Density Interval", ci_string = "HDI", ...) {
  cp <- attr(x, "clean_parameters")
  formatted_table <- format(x, cp = cp, digits = digits, format = "text", ci_string = ci_string, ...)

  # in case we have no multiple components, just use "Highest Density Interval" as caption
  if (length(formatted_table) == 1) {
    attr(formatted_table[[1]], "table_caption") <- c(caption, "blue")
  } else {
    cat(insight::print_color(paste0(caption, "\n\n"), "blue"))
  }

  cat(insight::export_table(formatted_table))
  invisible(x)
}
