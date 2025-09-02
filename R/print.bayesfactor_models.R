#' @export
print.bayesfactor_matrix <- function(x, log = FALSE, exact = TRUE, ...) {
  orig_x <- x

  # Format values
  x <- unclass(x)
  if (log) {
    sgn <- sign(x) < 0
    x <- insight::format_value(abs(x), digits = 2, ...)

    if (any(sgn)) {
      x[sgn] <- paste0("-", x[sgn])
    }

    diag(x) <- "0"
  } else {
    x <- exp(x)
    x <- insight::format_bf(x, name = NULL, exact = exact, ...)

    diag(x) <- "1"
  }

  df <- as.data.frame(x)

  # Model names
  models <- colnames(df)
  models[models == "1"] <- "(Intercept only)"
  models <- paste0("[", seq_along(models), "] ", models)

  rownames(df) <- colnames(df) <- NULL
  df <- cbind(modl = models, df)
  colnames(df) <- c(
    "Denominator\\Numerator",
    paste0(" [", seq_along(models), "] ")
  )

  # caption and footer
  caption <- switch(
    attr(orig_x, "bf_fun"),
    "bayesfactor_restricted()" = "# Bayes Factors for Restricted Models",
    "# Bayes Factors for Model Comparison"
  )
  footer <- if (log) c("\nBayes Factors are on the log-scale.\n", "red")

  out <- insight::export_table(
    df,
    caption = c(caption, "blue"),
    footer = footer
  )
  # Fix spacing
  out <- sub("Denominator", " Denominator", out, fixed = TRUE)

  cat(out)

  invisible(orig_x)
}
