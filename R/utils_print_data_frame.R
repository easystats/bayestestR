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
    if (anyNA(x$split)) {
      x$split[is.na(x$split)] <- "{other}"
    }
    out <- lapply(split(x, f = x$split), function(i) {
      datawizard::data_remove(i, c("split", "Component", "Effects"), verbose = FALSE)
    })
  }

  for (i in names(out)) {
    header <- switch(i,
      "conditional" = ,
      "fixed_conditional" = ,
      "fixed" = "# Fixed Effects (Conditional Model)",
      "fixed_sigma" = "# Sigma (fixed effects)",
      "sigma" = "# Sigma (fixed effects)",
      "zi" = ,
      "zero_inflated" = ,
      "fixed_zero_inflated" = ,
      "fixed_zi" = "# Fixed Effects (Zero-Inflated Model)",
      "random" = ,
      "random_conditional" = "# Random Effects (Conditional Model)",
      "random_zero_inflated" = ,
      "random_zi" = "# Random Effects (Zero-Inflated Model)",
      "smooth_sd" = ,
      "fixed_smooth_sd" = "# Smooth Terms",

      # blavaan
      "latent" = "# Latent Loading",
      "residual" = "# Residual Variance",
      "intercept" = "# Intercept",
      "regression" = "# Regression",

      # Default
      paste0("# ", i)
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
      # SD
      out[[i]]$Parameter <- gsub("(.*)(__Intercept|__zi_Intercept)(.*)", "\\1 (Intercept)\\3", gsub("^sd_(.*)", "SD \\1", out[[i]]$Parameter))
      # remove ".1" etc. suffix
      out[[i]]$Parameter <- gsub("(.*)(\\.)(\\d)$", "\\1 \\3", out[[i]]$Parameter)
      # remove "__zi"
      out[[i]]$Parameter <- gsub("__zi", "", out[[i]]$Parameter)
    }

    if (length(out) > 1) {
      insight::print_color(header, "blue")
      cat("\n\n")
    }

    cat(insight::export_table(out[[i]], digits = digits))
    cat("\n")
  }
}
