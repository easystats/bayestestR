#' @importFrom insight format_table print_parameters
#' @export
format.describe_posterior <- function(x, cp, digits = 2, format = "text", ...) {
  out <- insight::format_table(x, digits = 2, format = format, ...)
  if (!is.null(cp) && !all(is.na(match(cp$Parameter, out$Parameter)))) {
    out <- insight::print_parameters(cp, out, keep_parameter_column = FALSE, remove_empty_column = TRUE)
  }
  out
}


#' @importFrom insight export_table
#' @export
print.describe_posterior <- function(x, digits = 2, ...) {
  cp <- attr(x, "clean_parameters")
  cat(insight::export_table(format(x, cp = cp, digits = digits, format = "text", ...)))
  invisible(x)
}


#' @export
print.point_estimate <- print.describe_posterior


#' @export
format.point_estimate <- format.describe_posterior





# print.describe_posterior <- function(x, digits = 3, ...) {
#   print_data_frame(format(x, digits = digits, ...), digits = digits, ...)
#   invisible(x)
# }


# format.describe_posterior <- function(x, digits = 3, ...) {
#   if ("data_plot" %in% class(x)) {
#     return(as.data.frame(x), digits = digits)
#   }
#
#   if ("CI" %in% colnames(x)) {
#     is_SI <- !is.null(attributes(x)$ci_method) && tolower(attributes(x)$ci_method) == "si"
#
#     ci <- unique(x$CI) * 100
#     if (length(ci) > 1) {
#       x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = x$CI, digits = digits, width = "auto")
#
#       if (is_SI) {
#         x$CI <- paste0("BF = ", gsub("% CI", " SI", x$CI))
#         colnames(x)[colnames(x) == "CI"] <- "SI"
#       }
#     } else {
#       x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = "auto")
#
#       if (is.null(ci)) {
#         if (is_SI) colnames(x)[colnames(x) == "CI"] <- "SI"
#       } else {
#         if (is_SI) {
#           colnames(x)[colnames(x) == "CI"] <- sprintf("BF = %.5g SI", ci)
#         } else {
#           colnames(x)[colnames(x) == "CI"] <- sprintf("%.5g%% CI", ci)
#         }
#       }
#     }
#
#     x <- .remove_column(x, c("CI_low", "CI_high"))
#   }
#
#
#   if ("ROPE_CI" %in% colnames(x)) {
#     rci <- unique(x$ROPE_CI)
#     if (length(rci) > 1) {
#       x$ROPE_CI <- insight::format_ci(x$ROPE_low, x$ROPE_high, ci = rci / 100, digits = digits, width = "auto")
#     } else {
#       x$ROPE_CI <- insight::format_ci(x$ROPE_low, x$ROPE_high, ci = NULL, digits = digits, width = "auto")
#       colnames(x)[colnames(x) == "ROPE_CI"] <- sprintf("%.5g%% ROPE", rci)
#     }
#     x <- .remove_column(x, c("ROPE_low", "ROPE_high"))
#   }
#
#   x <- insight::format_table(x, digits = digits, ...)
#   x
# }
