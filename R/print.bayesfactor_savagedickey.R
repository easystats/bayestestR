#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  hypothesis <- attr(BFE, "hypothesis")

  if (log) {
    names(BFE)[names(BFE) == "log.BF"] <- "log(Bayes Factor)"
  } else {
    BFE$log.BF <- exp(BFE$log.BF)
    names(BFE)[names(BFE) == "log.BF"] <- "Bayes Factor"
  }

  insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")

  print.data.frame(BFE, digits = digits)
  cat("---\n")
  cat(paste0("Evidence Against Test Value: ", round(hypothesis, digits), "\n"))
  invisible(x)
}
