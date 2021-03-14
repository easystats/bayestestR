#' @export
print.bayestestR_eti <- function(x, digits = 2, ...) {
  orig_x <- x
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Equal-Tailed Interval", ci_string = "ETI", ...)
  }
  invisible(orig_x)
}

#' @export
print.bayestestR_si <- function(x, digits = 2, ...) {
  orig_x <- x
  .print_hdi(x, digits, title = "Support Interval", ci_string = "SI", ...)
  invisible(orig_x)
}

#' @export
print.bayestestR_ci <- function(x, digits = 2, ...) {
  orig_x <- x
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Credible Interval", ci_string = "CI", ...)
  }
  invisible(orig_x)
}


.print_hdi <- function(x, digits, title, ci_string, ...) {
  insight::print_color(sprintf(
    "# %s%s\n\n",
    title,
    ifelse(all(x$CI[1] == x$CI), "", "s")
  ), "blue")

  ci <- unique(x$CI)
  x$HDI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = "auto", missing = "NA")

  if (length(ci) == 1) {
    xsub <- .remove_column(x, c("CI", "CI_low", "CI_high"))
    colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", 100 * ci, ci_string)
    if (inherits(x, "bayestestR_si")) colnames(xsub)[ncol(xsub)] <- sprintf("BF = %.5g %s", ci, ci_string)
    print_data_frame(xsub, digits = digits)
  } else {
    for (i in ci) {
      xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
      xsub <- .remove_column(xsub, c("CI", "CI_low", "CI_high"))
      colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", 100 * i, ci_string)
      if (inherits(x, "bayestestR_si")) colnames(xsub)[ncol(xsub)] <- sprintf("BF = %.5g %s", i, ci_string)
      print_data_frame(xsub, digits = digits)
      cat("\n")
    }
  }
}



# as.list -----------------------------------------------------------------

#' @export
as.list.bayestestR_hdi <- function(x, ...) {
  if (nrow(x) == 1) {
    out <- list(CI = x$CI, CI_low = x$CI_low, CI_high = x$CI_high)
    out$Parameter <- x$Parameter
  } else {
    out <- list()
    for (param in x$Parameter) {
      out[[param]] <- list()
      out[[param]][["CI"]] <- x[x$Parameter == param, "CI"]
      out[[param]][["CI_low"]] <- x[x$Parameter == param, "CI_low"]
      out[[param]][["CI_high"]] <- x[x$Parameter == param, "CI_high"]
    }
  }
  out
}

#' @export
as.list.bayestestR_eti <- as.list.bayestestR_hdi

#' @export
as.list.bayestestR_si <- as.list.bayestestR_hdi

#' @export
as.list.bayestestR_ci <- as.list.bayestestR_hdi
