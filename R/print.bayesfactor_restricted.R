#' @export
print.bayesfactor_restricted <- function(x, digits = 3, log = FALSE, ...) {
  BFE <- x

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")

  colnames(BFE) <- c("Hypothesis", "P(Prior)", "P(Posterior)", "BF")

  insight::print_color("# Bayes Factor (Order-Restriction)\n\n", "blue")

  print.data.frame(BFE, digits = digits, row.names = FALSE)

  cat("\n* Bayes factors for the restricted model vs. the un-restricted model.\n")

  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")
  invisible(x)
}
