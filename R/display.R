#' @title Print tables in different output formats
#' @name display.describe_posterior
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'
#' @param object,x An object returned by one of the package's function, for
#' example [`describe_posterior()`], [`point_estimate()`], or [`eti()`].
#' @param format String, indicating the output format. Can be `"markdown"`
#' `"html"`, or `"tt"`. `format = "tt"` creates a `tinytable` object, which is
#' either printed as markdown or HTML table, depending on the environment. See
#' [`insight::export_table()`] for details.
#' @param ... Arguments passed down to `print_html()` or `print_md()` (e.g.,
#' `digits`), or to `insight::export_table()`.
#'
#' @return If `format = "markdown"`, the return value will be a character
#' vector in markdown-table format. If `format = "html"`, an object of
#' class `gt_tbl`. If `format = "tt"`, an object of class `tinytable`.
#'
#' @details `display()` is useful when the table-output from functions, which is
#' usually printed as formatted text-table to console, should be formatted for
#' pretty table-rendering in markdown documents, or if knitted from rmarkdown
#' to PDF or Word files. See
#' [vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
#' for examples.
#'
#' @examplesIf all(insight::check_if_installed(c("tinytable", "gt"), quietly = TRUE))
#' \donttest{
#' d <- data.frame(replicate(4, rnorm(20)))
#' result <- describe_posterior(d)
#'
#' # markdown format
#' display(result)
#'
#' # gt HTML
#' display(result, format = "html")
#'
#' # tinytable
#' display(result, format = "tt")
#' }
#' @export
display.describe_posterior <- function(object, format = "markdown", ...) {
  insight::validate_argument(format, c("md", "markdown", "html", "tt"))
  if (format %in% c("html", "tt")) {
    print_html(object, backend = format, ...)
  } else {
    print_md(object, ...)
  }
}

#' @export
display.point_estimate <- display.describe_posterior

#' @export
display.map_estimate <- display.describe_posterior

#' @export
display.p_direction <- display.describe_posterior

#' @export
display.p_map <- display.describe_posterior

#' @export
display.p_rope <- display.describe_posterior

#' @export
display.p_significance <- display.describe_posterior

#' @export
display.bayestestR_hdi <- display.describe_posterior

#' @export
display.bayestestR_eti <- display.describe_posterior

#' @export
display.bayestestR_si <- display.describe_posterior

#' @export
display.bayesfactor_models <- display.describe_posterior

#' @export
display.bayesfactor_restricted <- display.describe_posterior

#' @export
display.bayesfactor_parameters <- display.describe_posterior

#' @export
display.bayesfactor_inclusion <- display.describe_posterior


# we allow exporting HTML format based on "gt" or "tinytable"
.check_format_backend <- function(...) {
  dots <- list(...)
  if (identical(dots$backend, "tt")) {
    "tt"
  } else {
    "html"
  }
}
