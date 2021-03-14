# old print methods --------------------


# print.describe_posterior <- function(x, digits = 3, ...) {
#   print_data_frame(format(x, digits = digits, ...), digits = digits, ...)
#   invisible(x)
# }



# print.bayestestR_hdi <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else {
#     .print_hdi(x, digits, title = "Highest Density Interval", ci_string = "HDI", ...)
#   }
#   invisible(orig_x)
# }



# print.bayestestR_eti <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else {
#     .print_hdi(x, digits, title = "Equal-Tailed Interval", ci_string = "ETI", ...)
#   }
#   invisible(orig_x)
# }



# print.bayestestR_si <- function(x, digits = 2, ...) {
#   orig_x <- x
#   .print_hdi(x, digits, title = "Support Interval", ci_string = "SI", ...)
#   invisible(orig_x)
# }



# print.bayestestR_ci <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else {
#     .print_hdi(x, digits, title = "Credible Interval", ci_string = "CI", ...)
#   }
#   invisible(orig_x)
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



# .print_hdi <- function(x, digits, title, ci_string, ...) {
#   insight::print_color(sprintf(
#     "# %s%s\n\n",
#     title,
#     ifelse(all(x$CI[1] == x$CI), "", "s")
#   ), "blue")
#
#   ci <- unique(x$CI)
#   x$HDI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = "auto", missing = "NA")
#
#   if (length(ci) == 1) {
#     xsub <- .remove_column(x, c("CI", "CI_low", "CI_high"))
#     colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", 100 * ci, ci_string)
#     if (inherits(x, "bayestestR_si")) colnames(xsub)[ncol(xsub)] <- sprintf("BF = %.5g %s", ci, ci_string)
#     print_data_frame(xsub, digits = digits)
#   } else {
#     for (i in ci) {
#       xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
#       xsub <- .remove_column(xsub, c("CI", "CI_low", "CI_high"))
#       colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", 100 * i, ci_string)
#       if (inherits(x, "bayestestR_si")) colnames(xsub)[ncol(xsub)] <- sprintf("BF = %.5g %s", i, ci_string)
#       print_data_frame(xsub, digits = digits)
#       cat("\n")
#     }
#   }
# }



# print.point_estimate <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else if ("data.frame" %in% class(x)) {
#     insight::print_color("# Point Estimates\n\n", "blue")
#     print_data_frame(x, digits = digits)
#   } else {
#     print(unclass(x))
#   }
#   invisible(orig_x)
# }



# print.p_direction <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else if ("data.frame" %in% class(x)) {
#     .print_pd(x, digits, ...)
#   } else {
#     cat(sprintf("pd = %s%%", insight::format_value(x * 100, digits = digits)))
#   }
#   invisible(orig_x)
# }
#
# .print_pd <- function(x, digits, ...) {
#   insight::print_color("# Probability of Direction (pd)\n\n", "blue")
#   x$Parameter <- as.character(x$Parameter)
#   x$pd <- sprintf("%s%%", insight::format_value(x$pd * 100, digits = digits))
#   print_data_frame(x, digits = digits)
# }
