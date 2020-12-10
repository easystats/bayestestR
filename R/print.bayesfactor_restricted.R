#' @export
print.bayesfactor_restricted <- function(x, digits = 3, log = FALSE, ...) {
  BFE <- as.data.frame(x)

  # Format
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")
  colnames(BFE) <- c("Hypothesis", "P(Prior)", "P(Posterior)", "BF")

  # footer
  footer <- list(
    c("\n* Bayes factors for the restricted model vs. the un-restricted model.\n"),
    if (log) c("\nBayes Factors are on the log-scale.\n", "red")
  )


  cat(insight::export_table(
    BFE, digits = digits, sep = " ", header = NULL,
    caption = c("# Bayes Factor (Order-Restriction)", "blue"),
    footer = footer
  ))


  invisible(x)
}
