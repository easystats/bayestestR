#' @importFrom insight format_table print_parameters
#' @export
format.describe_posterior <- function(x,
                                      cp,
                                      digits = 2,
                                      format = "text",
                                      ci_string = "CI",
                                      caption = NULL,
                                      subtitles = NULL,
                                      ...) {

  # reshape CI
  if (is.data.frame(x) && .n_unique(x$CI) > 1) {
    att <- attributes(x)
    x <- reshape_ci(x)
    attributes(x) <- utils::modifyList(att, attributes(x))
  }

  # format columns and values of data frame
  out <- insight::format_table(x, digits = 2, format = format, ...)

  # different CI-types as column names?
  if (ci_string != "CI" && any(grepl("CI$", colnames(out)))) {
    colnames(out) <- gsub("(.*)CI$", paste0("\\1", ci_string), colnames(out))
  }

  # match and split at components
  if (!is.null(cp) && !all(is.na(match(cp$Parameter, out$Parameter)))) {
    out <- insight::print_parameters(
      cp,
      out,
      keep_parameter_column = FALSE,
      remove_empty_column = TRUE,
      titles = caption,
      subtitles = subtitles,
      format = format
    )
  } else {
    attr(out, "table_caption") <- caption
    attr(out, "table_subtitle") <- subtitles
  }
  out
}


#' @export
format.point_estimate <- format.describe_posterior

#' @export
format.p_rope <- format.describe_posterior

#' @export
format.p_direction <- format.describe_posterior

#' @export
format.mhdior <- format.describe_posterior

#' @export
format.bayestestR_hdi <- format.describe_posterior

#' @export
format.bayestestR_eti <- format.describe_posterior

#' @export
format.bayestestR_si <- format.describe_posterior
