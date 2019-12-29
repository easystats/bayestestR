#' @export
print.bayestestR_hdi <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Highest Density Interval", ci_string = "HDI", ...)
  }
}


#' @export
print.bayestestR_eti <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Equal-Tailed Interval", ci_string = "ETI", ...)
  }
}

#' @export
print.bayestestR_si <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Support Interval", ci_string = "SI", ...)
  }
}

#' @export
print.bayestestR_ci <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Credible Interval", ci_string = "CI", ...)
  }
}


.print_hdi <- function(x, digits, title, ci_string, ...) {
  insight::print_color(sprintf(
    "# %s%s\n\n",
    title,
    ifelse(all(x$CI[1] == x$CI), "", "s")
  ), "blue")

  ci <- unique(x$CI)

  # find the longest HDI-value, so we can align the brackets in the ouput
  x$CI_low <- sprintf("%.*f", digits, x$CI_low)
  x$CI_high <- sprintf("%.*f", digits, x$CI_high)

  maxlen_low <- max(nchar(x$CI_low))
  maxlen_high <- max(nchar(x$CI_high))

  x$HDI <- sprintf("[%*s, %*s]", maxlen_low, x$CI_low, maxlen_high, x$CI_high)

  if (length(ci) == 1) {
    xsub <- .remove_column(x, c("CI", "CI_low", "CI_high"))
    colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", ci, ci_string)
    print_data_frame(xsub, digits = digits)
  } else {
    for (i in ci) {
      xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
      xsub <- .remove_column(xsub, c("CI", "CI_low", "CI_high"))
      colnames(xsub)[ncol(xsub)] <- sprintf("%.5g%% %s", i, ci_string)
      print_data_frame(xsub, digits = digits)
      cat("\n")
    }
  }
}
