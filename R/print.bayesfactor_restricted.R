#' @export
print.bayesfactor_restricted <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  BFE$BF <- format_big_small(BFE$BF, digits = digits)

  colnames(BFE) <- c("Hypothesis","P(Prior)","P(Posterior)","Bayes Factor")

  insight::print_color("# Bayes Factor (Order-Restriction)\n\n", "blue")

  print.data.frame(BFE, digits = digits, row.names = FALSE)

  cat("\n* Bayes factors for the restricted movel vs. the un-restricted model.\n")

  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")
  invisible(x)
}
