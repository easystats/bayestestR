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
#     x <- datawizard::data_remove(x, c("CI_low", "CI_high"))
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
#     x <- datawizard::data_remove(x, c("ROPE_low", "ROPE_high"))
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
#     xsub <- datawizard::data_remove(x, c("CI", "CI_low", "CI_high"))
#     colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", 100 * ci, ci_string)
#     if (inherits(x, "bayestestR_si")) colnames(xsub)[ncol(xsub)] <- sprintf("BF = %.5g %s", ci, ci_string)
#     print_data_frame(xsub, digits = digits)
#   } else {
#     for (i in ci) {
#       xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
#       xsub <- datawizard::data_remove(xsub, c("CI", "CI_low", "CI_high"))
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







# print.p_map <- function(x, digits = 3, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else if ("data.frame" %in% class(x)) {
#     insight::print_color("# MAP-based p-value\n\n", "blue")
#     print_data_frame(x, digits = digits)
#   } else {
#     cat(sprintf("p (MAP) = %.*f", digits, x))
#   }
#   invisible(orig_x)
# }



# print.p_significance <- function(x, digits = 2, ...) {
#   orig_x <- x
#   if ("data_plot" %in% class(x)) {
#     print(as.data.frame(x))
#   } else if ("data.frame" %in% class(x)) {
#     .print_ps(x, digits, ...)
#   } else {
#     cat(sprintf(
#       "ps [%s] = %s%%",
#       insight::format_value(attributes(x)$threshold, digits = digits),
#       insight::format_value(x * 100, digits = digits)
#     ))
#   }
#   invisible(orig_x)
# }
#
# .print_ps <- function(x, digits, ...) {
#   insight::print_color(sprintf(
#     "# Probability of Significance (ps [%s])\n\n",
#     insight::format_value(attributes(x)$threshold, digits = digits)
#   ), "blue")
#   x$Parameter <- as.character(x$Parameter)
#   x$ps <- sprintf("%s%%", insight::format_value(x$ps * 100, digits = digits))
#   print_data_frame(x, digits = digits)
# }


# print.map_estimate <- function(x, ...) {
#   orig_x <- x
#   if (inherits(x, "data.frame")) {
#     print.data.frame(x)
#   } else {
#     cat(sprintf("MAP = %.2f", x))
#   }
#   invisible(orig_x)
# }




# print.bayesfactor_parameters <- function(x, digits = 3, log = FALSE, ...) {
#   null <- attr(x, "hypothesis")
#   direction <- attr(x, "direction")
#
#   # format table
#   BFE <- as.data.frame(x)
#   if (log) {
#     BFE$BF <- log(BFE$BF)
#   }
#   BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")
#
#   caption <- c(sprintf(
#     "# Bayes Factor (%s)\n\n",
#     if (length(null) == 1) "Savage-Dickey density ratio" else "Null-Interval"
#   ), "blue")
#
#   footer <- list(
#     c("* Evidence Against The Null: "),
#     c(sprintf("[%s]", paste0(round(null, digits), collapse = ", ")), "cyan"),
#     if (direction) c("\n*                 Direction: "),
#     if (direction < 0) c("Left-Sided test", "cyan"),
#     if (direction > 0) c("Right-Sided test", "cyan"),
#     if (log) c("\n\nBayes Factors are on the log-scale.\n", "red")
#   )
#
#   {
#     insight::print_color(caption[1], caption[2])
#     print_data_frame(BFE, digits = digits)
#     lapply(footer, function(txt) {
#       if (length(txt) == 2) {
#         insight::print_color(txt[1], txt[2])
#       } else {
#         cat(txt)
#       }
#       NULL
#     })
#   }
#
#
#   invisible(x)
# }
#




# print.bayesfactor_inclusion <- function(x, digits = 3, log = FALSE, ...) {
#   priorOdds <- attr(x, "priorOdds")
#   matched <- attr(x, "matched")
#
#   # format table
#   BFE <- as.data.frame(x)
#   if (log) {
#     BFE$BF <- log(BFE$BF)
#   }
#   BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")
#   BFE <- cbind(rownames(BFE), BFE)
#   colnames(BFE) <- c("", "Pr(prior)", "Pr(posterior)", "Inclusion BF")
#
#
#   # footer
#   footer <- list(
#     c("\n* Compared among: "),
#     c(if (matched) "matched models only" else "all models", "cyan"),
#     c("\n*    Priors odds: "),
#     c(if (!is.null(priorOdds)) "custom" else "uniform-equal", "cyan"),
#     if (log) c("\n\nBayes Factors are on the log-scale.", "red")
#   )
#
#   cat(insight::export_table(
#     BFE,
#     digits = digits, sep = " ", header = NULL,
#     caption = c("# Inclusion Bayes Factors (Model Averaged)", "blue"),
#     footer = footer
#   ))
#
#   invisible(x)
# }



# print.bayesfactor_restricted <- function(x, digits = 3, log = FALSE, ...) {
#   BFE <- as.data.frame(x)
#
#   # Format
#   if (log) {
#     BFE$BF <- log(BFE$BF)
#   }
#   BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")
#   colnames(BFE) <- c("Hypothesis", "P(Prior)", "P(Posterior)", "BF")
#
#   # footer
#   footer <- list(
#     c("\n* Bayes factors for the restricted model vs. the un-restricted model.\n"),
#     if (log) c("\nBayes Factors are on the log-scale.\n", "red")
#   )
#
#
#   cat(insight::export_table(
#     BFE,
#     digits = digits, sep = " ", header = NULL,
#     caption = c("# Bayes Factor (Order-Restriction)", "blue"),
#     footer = footer
#   ))
#
#
#   invisible(x)
# }
