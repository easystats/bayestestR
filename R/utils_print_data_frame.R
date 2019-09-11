print_data_frame <- function(x, digits) {
  out <- list(x)
  names(out) <- "fixed"

  if (all(c("Effects", "Component") %in% colnames(x))) {
    x$split <- sprintf("%s_%s", x$Effects, x$Component)
  } else if ("Effects" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Effects")] <- "split"
  } else if ("Component" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Component")] <- "split"
  }

  if ("split" %in% colnames(x)) {
    out <- lapply(split(x, f = x$split), function(i) {
      .remove_column(i, c("split", "Component", "Effects"))
    })
  }

  for (i in names(out)) {
    header <- switch(
      i,
      "conditional" = ,
      "fixed_conditional" = ,
      "fixed" = "# fixed effects, conditional component",
      "zi" = ,
      "zero_inflated" = ,
      "fixed_zero_inflated" = ,
      "fixed_zi" = "# fixed effects, zero-inflation component",
      "random" = ,
      "random_conditional" = "# random effects, conditional component",
      "random_zero_inflated" = ,
      "random_zi" = "# random effects, zero-inflation component",
      "smooth_sd" = ,
      "fixed_smooth_sd" = "# smooth terms"
    )

    if ("Parameter" %in% colnames(out[[i]])) {
      # clean parameters names
      out[[i]]$Parameter <- gsub("(b_|bs_|bsp_|bcs_)(?!zi_)(.*)", "\\2", out[[i]]$Parameter, perl = TRUE)
      out[[i]]$Parameter <- gsub("(b_zi_|bs_zi_|bsp_zi_|bcs_zi_)(.*)", "\\2", out[[i]]$Parameter, perl = TRUE)
      # clean random effect parameters names
      out[[i]]$Parameter <- gsub("r_(.*)\\.(.*)\\.", "\\1", out[[i]]$Parameter)
      out[[i]]$Parameter <- gsub("b\\[\\(Intercept\\) (.*)\\]", "\\1", out[[i]]$Parameter)
      out[[i]]$Parameter <- gsub("b\\[(.*) (.*)\\]", "\\2", out[[i]]$Parameter)
      # clean smooth terms
      out[[i]]$Parameter <- gsub("^smooth_sd\\[(.*)\\]", "\\1", out[[i]]$Parameter)
      out[[i]]$Parameter <- gsub("^sds_", "\\1", out[[i]]$Parameter)
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
