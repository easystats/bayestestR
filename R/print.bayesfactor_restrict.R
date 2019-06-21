#' @export
print.bayesfactor_restrict <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  colnames(BFE) <- c("Hypothesis","P(Prior)","P(Posterior)","BF")

  xBF <- BFE$BF
  BFE$BF <- as.character(round(xBF, digits = digits))
  big_ind <- abs(xBF) >= 1000 | abs(xBF) < 1 / (10^digits)
  big_ind <- sapply(big_ind, isTRUE)
  if (isTRUE(any(big_ind))) {
    BFE$BF[big_ind] <- formatC(xBF, format = "e", digits = digits)[big_ind]
  }

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
