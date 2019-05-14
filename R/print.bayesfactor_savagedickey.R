#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  hypothesis <- attr(BFE, "hypothesis")

  if (log) {
    BFE$BF <- log(BFE$BF)
    names(BFE)[names(BFE) == "BF"] <- "log(Bayes Factor)"
  } else {
    names(BFE)[names(BFE) == "BF"] <- "Bayes Factor"
  }

  insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")

  print.data.frame(BFE, digits = digits)
  cat("---\n")
  cat(paste0("Evidence Against Test Value: ", round(hypothesis, digits), "\n"))
  invisible(x)
}
