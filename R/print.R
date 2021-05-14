#' @importFrom insight export_table format_value
#' @export
print.describe_posterior <- function(x, digits = 2, caption = "Summary of Posterior Distribution", ...) {
  .print_default(x = x, digits = digits, caption = caption, ...)
}


#' @export
print.point_estimate <- function(x, digits = 2, caption = "Point Estimate", ...) {
  .print_default(x = x, digits = digits, caption = caption, ...)
}


#' @export
print.p_direction <- function(x, digits = 2, caption = "Probability of Direction", ...) {
  .print_default(x = x, digits = digits, caption = caption, ...)
}


#' @export
print.p_map <- function(x, digits = 2, caption = "MAP-based p-value", ...) {
  .print_default(x = x, digits = digits, caption = caption, ...)
}


#' @export
print.map_estimate <- function(x, digits = 2, caption = "MAP Estimate", ...) {
  .print_default(x = x, digits = digits, caption = caption, ...)
}


#' @export
print.p_rope <- function(x, digits = 2, ...) {
  caption <- sprintf("Proportion of samples inside the ROPE [%.*f, %.*f]",
                     digits, x$ROPE_low[1], digits, x$ROPE_high[1])
  x$ROPE_low <- x$ROPE_high <- NULL
  .print_default(x = x, digits = digits, caption = caption, ci_string = "ROPE", ...)
}


#' @export
print.p_significance <- function(x, digits = 2, ...) {
  caption <- sprintf("Practical Significance (threshold: %s)",
                     insight::format_value(attributes(x)$threshold, digits = digits))
  .print_default(x = x, digits = digits, caption = caption, ...)
}


#' @export
print.bayestestR_hdi <- function(x, digits = 2, caption = "Highest Density Interval", ...) {
  .print_default(x = x, digits = digits, caption = caption, ci_string = "HDI", ...)
}


#' @export
print.bayestestR_eti <- function(x, digits = 2, caption = "Equal-Tailed Interval", ...) {
  .print_default(x = x, digits = digits, caption = caption, ci_string = "ETI", ...)
}


#' @export
print.bayestestR_si <- function(x, digits = 2, caption = "Support Interval", ...) {
  .print_default(x = x, digits = digits, caption = caption, ci_string = "SI", ...)
}




# special handling for bayes factors ------------------


#' @export
print.bayesfactor_models <- function(x,
                                     digits = 3,
                                     log = FALSE,
                                     show_names = TRUE,
                                     caption = "Bayes Factors for Model Comparison",
                                     ...) {
  show_names <- show_names & !attr(x, "unsupported_models")
  .print_bf_default(x = x, digits = digits, log = log, show_names = show_names, caption = caption, align = c("llr"), ...)
}


#' @export
print.bayesfactor_inclusion <- function(x,
                                        digits = 3,
                                        log = FALSE,
                                        caption = "Inclusion Bayes Factors (Model Averaged)",
                                        ...) {
  .print_bf_default(x = x, digits = digits, log = log, caption = caption, ...)
}


#' @export
print.bayesfactor_restricted <- function(x,
                                         digits = 3,
                                         log = FALSE,
                                         caption = "Bayes Factor (Order-Restriction)",
                                         ...) {
  .print_bf_default(x = x, digits = digits, log = log, caption = caption, ...)
}


#' @export
print.bayesfactor_parameters <- function(x, digits = 3, log = FALSE, ...) {

  # retrieve information with cleaned parameter names
  cp <- attr(x, "clean_parameters")

  # format data frame and columns
  formatted_table <- format(
    x,
    cp = cp,
    digits = digits,
    log = log,
    format = "text",
    ...
  )

  cat(insight::export_table(formatted_table, format = "text"))
  invisible(x)
}





# util ---------------------


.print_default <- function(x, digits = 2, caption = NULL, subtitles = NULL, ci_string = "CI", ...) {

  # retrieve information with cleaned parameter names
  cp <- attr(x, "clean_parameters")

  # format data frame and columns
  formatted_table <- format(
    x,
    cp = cp,
    digits = digits,
    format = "text",
    ci_string = ci_string,
    caption = caption,
    subtitles = subtitles,
    ...
  )

  # check if we have a 1x1 data frame (i.e. a numeric input)
  if (is.data.frame(formatted_table) && nrow(formatted_table) == 1 && ncol(formatted_table) == 1) {

    # print for numeric

    caption <- attr(formatted_table, "table_caption")

    # if we have no useful column name and a caption, use caption
    if (!is.null(caption) && !grepl(paste0(ci_string, "$"), colnames(formatted_table))) {
      cat(paste0(caption, ": "))
    } else {
      cat(paste0(colnames(formatted_table), ": "))
    }
    cat(formatted_table[1, 1])
  } else {

    # print for data frame

    cat(insight::export_table(
      formatted_table,
      caption = caption
    ))
  }

  invisible(x)
}



.print_bf_default <- function(x,
                              digits = 3,
                              log = FALSE,
                              caption = NULL,
                              align = NULL,
                              ...) {

  # format data frame and columns
  formatted_table <- format(
    x,
    digits = digits,
    log = log,
    format = "text",
    caption = caption,
    ... # pass show_names
  )

  cat(insight::export_table(
    formatted_table,
    sep = " ",
    header = NULL,
    format = "text",
    align = align,
  ))

  invisible(x)
}
