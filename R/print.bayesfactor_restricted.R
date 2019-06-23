#' @export
print.bayesfactor_restricted <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  colnames(BFE) <- c("Hypothesis","P(Prior)","P(Posterior)","BF")

  BFE$BF <- format_big_small(BFE$BF, digits = digits)

  if (log) {
    colnames(BFE)[colnames(BFE) == "BF"] <- "log(Bayes Factor)"
  } else {
    colnames(BFE)[colnames(BFE) == "BF"] <- "Bayes Factor"
  }

  insight::print_color("# Bayes Factor (Order-Restriction)\n\n", "blue")

  print.data.frame(BFE, digits = digits, row.names = FALSE)

  cat("---\n")
  cat("Bayes factors for the restricted movel vs. the un-restricted model.\n")
  invisible(x)
}
