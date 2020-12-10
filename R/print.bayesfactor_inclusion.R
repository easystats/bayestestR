#' @export
print.bayesfactor_inclusion <- function(x, digits = 3, log = FALSE, ...) {
  priorOdds <- attr(x, "priorOdds")
  matched <- attr(x, "matched")

  # format table
  BFE <- as.data.frame(x)
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")
  BFE <- cbind(rownames(BFE), BFE)
  colnames(BFE) <- c("","Pr(prior)", "Pr(posterior)", "Inclusion BF")


  # footer
  footer <- list(
    c("\n* Compared among: "),
    c(if (matched) "matched models only" else "all models", "cyan"),
    c("\n*    Priors odds: "),
    c(if (!is.null(priorOdds)) "custom" else "uniform-equal", "cyan"),
    if (log) c("\n\nBayes Factors are on the log-scale.", "red")
  )

  cat(insight::export_table(
    BFE, digits = digits, sep = " ", header = NULL,
    caption = c("# Inclusion Bayes Factors (Model Averaged)", "blue"),
    footer = footer
  ))

  invisible(x)
}
