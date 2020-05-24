#' @importFrom insight format_table format_ci
#' @export
print.describe_posterior <- function(x, digits = 3, ...) {
  orig_x <- x
  if ("data_plot" %in% class(x)) {
    cat(insight::format_table(as.data.frame(x), digits = digits))
  } else {
    insight::print_color("# Description of Posterior Distributions\n\n", "blue")

    if ("CI" %in% colnames(x)) {
      is_SI <- !is.null(attributes(x)$ci_method) && tolower(attributes(x)$ci_method) == "si"

      ci <- unique(x$CI)
      if (length(ci) > 1) {
        x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = round(x$CI,3)/100, digits = digits, width = "auto")

        if (is_SI) {
          x$CI <- paste0("BF = ",gsub("% CI", " SI", x$CI))
          colnames(x)[colnames(x) == "CI"] <- "SI"
        }
      } else {
        x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = "auto")

        if (is.null(ci)) {
          if (is_SI) colnames(x)[colnames(x) == "CI"] <- "SI"
        } else {
          if (is_SI) {
            colnames(x)[colnames(x) == "CI"] <- sprintf("BF = %.5g SI", ci)
          } else {
            colnames(x)[colnames(x) == "CI"] <- sprintf("%.5g%% CI", ci)
          }
        }
      }

      x <- .remove_column(x, c("CI_low", "CI_high"))
    }


    if ("ROPE_CI" %in% colnames(x)) {
      rci <- unique(x$ROPE_CI)
      if (length(rci) > 1) {
        x$ROPE_CI <- insight::format_ci(x$ROPE_low, x$ROPE_high, ci = rci / 100, digits = digits, width = "auto")
      } else {
        x$ROPE_CI <- insight::format_ci(x$ROPE_low, x$ROPE_high, ci = NULL, digits = digits, width = "auto")
        colnames(x)[colnames(x) == "ROPE_CI"] <- sprintf("%.5g%% ROPE", rci)
      }
      x <- .remove_column(x, c("ROPE_low", "ROPE_high"))
    }

    if ("ROPE_Percentage" %in% colnames(x)) {
      x$ROPE_Percentage <- 100 * x$ROPE_Percentage
      colnames(x)[colnames(x) == "ROPE_Percentage"] <- "% in ROPE"
    }

    print_data_frame(x, digits = digits)
  }
  invisible(orig_x)
}
