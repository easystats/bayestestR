#' @importFrom insight print_color
#' @export
print.bayesfactor_models <- function(x, digits = 3, log = FALSE, ...) {
  BFE <- x
  denominator <- attr(BFE, "denominator")
  grid.type <- attr(BFE, "BF_method")

  BFE <- as.data.frame(BFE)
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA", zap_small = log)

  # indicate null-model
  BFE$Model[BFE$Model == "1"] <- "(Intercept only)"

  BFE$Model <- paste0(" [", seq_len(nrow(BFE)), "] ", BFE$Model)
  denM <- .trim(BFE$Model[denominator])
  BFE <- BFE[-denominator, ]
  BFE$Model <- format(BFE$Model)
  colnames(BFE) <- c(format(" Model", width = max(nchar(BFE$Model))), "BF")

  insight::print_color("# Bayes Factors for Model Comparison\n\n", "blue")

  print.data.frame(BFE, digits = digits, quote = FALSE, row.names = FALSE)
  cat("\n* Against Denominator: ")
  insight::print_color(denM, "cyan")
  cat("\n*   Bayes Factor Type: ")
  insight::print_color(grid.type, "cyan")
  cat("\n")
  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")
  invisible(x)
}

#' @importFrom insight print_color format_table
#' @export
print.bayesfactor_models_matrix <- function(x, digits = 2, log = FALSE, ...) {
  orig_x <- x

  if (log) {
    x <- log(x)
  }

  x[] <- insight::format_value(x[], digits = digits, zap_small = log)
  diag(x) <- if (log) "0" else "1"

  models <- colnames(x)
  models[models == "1"] <- "(Intercept only)"
  models <- paste0(" [", seq_along(models), "] ", models)

  df <- as.data.frame(x)
  rownames(df) <- colnames(df) <- NULL
  df <- cbind(Model = models, df)
  colnames(df) <- c("Numerator / Denominator", paste0(" [", seq_along(models), "] "))
  df <- insight::format_table(df)
  df <- sub("Numerator / Denominator", "", df)

  insight::print_color("# Bayes Factors for Model Comparison\n\n", "blue")
  insight::print_color("Denominator \\ Numerator", "cyan")
  cat(df)
  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")

  invisible(orig_x)
}
