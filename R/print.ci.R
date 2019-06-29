#' @export
print.hdi <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Highest Density Interval", ci_string = "HDI", ...)
  }
}


#' @export
print.eti <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    .print_hdi(x, digits, title = "Equal-Tailed Interval", ci_string = "ETI", ...)
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

















print_data_frame <- function(x, digits) {
  out <- list(x)
  names(out) <- "fixed"

  if (all(c("Group", "Component") %in% colnames(x))) {
    x$split <- sprintf("%s_%s", x$Group, x$Component)
  } else if ("Group" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Group")] <- "split"
  } else if ("Component" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Component")] <- "split"
  }

  if ("split" %in% colnames(x)) {
    out <- lapply(split(x, f = x$split), function(i) {
      .remove_column(i, c("split", "Component", "Group"))
    })
  }

  for (i in names(out)) {
    header <- switch(
      i,
      "conditional" = ,
      "fixed_conditional" = ,
      "fixed" = "# fixed effects, conditional component",
      "zi" = ,
      "fixed_zi" = "# fixed effects, zero-inflation component",
      "random" = ,
      "random_conditional" = "# random effects, conditional component",
      "random_zi" = "# random effects, zero-inflation component"
    )

    if ("Parameter" %in% colnames(out[[i]])) {
      # clean parameters names
      out[[i]]$Parameter <- gsub("^(b_zi_|bs_|b_|bsp_|bcs_)(.*)", "\\2", out[[i]]$Parameter)
      # remove ".1" etc. suffix
      out[[i]]$Parameter <- gsub("(.*)(\\.)(\\d)$", "\\1 \\3", out[[i]]$Parameter)
      # remove "__zi"
      out[[i]]$Parameter <- gsub("__zi", "", out[[i]]$Parameter)
    }

    if (length(out) > 1) {
      insight::print_color(header, "red")
      cat("\n\n")
    }

    print.data.frame(out[[i]], row.names = FALSE, digits = digits)
    cat("\n")
  }
}
