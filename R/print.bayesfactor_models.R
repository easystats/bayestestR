#' @export
print.bayesfactor_models_matrix <- function(x, digits = 2, log = FALSE, exact = TRUE, ...) {
  orig_x <- x

  # Format values
  x <- unclass(x)
  if (!log) x <- exp(x)
  sgn <- sign(x) < 0
  x <- insight::format_bf(abs(x), name = NULL, exact = exact, ...)
  diag(x) <- if (log) "0" else "1"
  if (any(sgn)) x[sgn] <- paste0("-", x[sgn])

  df <- as.data.frame(x)

  # Model names
  models <- colnames(df)
  models[models == "1"] <- "(Intercept only)"
  models <- paste0("[", seq_along(models), "] ", models)
  k <- max(vapply(c(models, "Denominator"), nchar, numeric(1))) + 2

  rownames(df) <- colnames(df) <- NULL
  df <- cbind(Model = models, df)
  colnames(df) <- c("placeholder", paste0(" [", seq_along(models), "] "))

  out <- insight::export_table(
    df,
    caption = c("# Bayes Factors for Model Comparison", "blue"),
    subtitle = c(sprintf("\n\n%sNumerator\nDenominator", strrep(" ", k)), "cyan"),
    footer = if (log) c("\nBayes Factors are on the log-scale.\n", "red")
  )
  out <- sub("placeholder", "\b\b", out, fixed = TRUE)
  cat(out)

  invisible(orig_x)
}
