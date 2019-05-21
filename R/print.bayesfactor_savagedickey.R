#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  hypothesis <- attr(BFE, "hypothesis")
  direction <- attr(BFE, "direction")

  if (log) {
    BFE$BF <- log(BFE$BF)
    colnames(BFE)[colnames(BFE) == "BF"] <- "log(Bayes Factor)"
  } else {
    colnames(BFE)[colnames(BFE) == "BF"] <- "Bayes Factor"
  }

  insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")

  print.data.frame(BFE, digits = digits, row.names = FALSE)
  cat("---\n")
  cat(paste0("Evidence Against Test Value: ", round(hypothesis, digits), "\n"))
  if (direction < 0) {
    cat(paste0("Left-Sided test\n"))
  } else if (direction > 0) {
    cat(paste0("Right-Sided test\n"))
  }
  invisible(x)
}
