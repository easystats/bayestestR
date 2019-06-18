#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  hypothesis <- attr(BFE, "hypothesis")
  direction <- attr(BFE, "direction")

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

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

  insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")

  print.data.frame(BFE, digits = digits, row.names = FALSE)
  cat("---\n")
  cat("Evidence Against Test Value: ", round(hypothesis, digits), "\n")
  if (direction < 0) {
    cat("Left-Sided test\n")
  } else if (direction > 0) {
    cat("Right-Sided test\n")
  }
  invisible(x)
}
