#' @export
display.describe_posterior <- function(object, format = "markdown", ...) {
  insight::validate_argument(format, c("md", "markdown", "html"))
  if (format == "html") {
    print_html(object, ...)
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
display.bayesfactor_inclusion <- display.describe_posterior
