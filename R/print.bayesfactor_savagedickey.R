#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  colnames(BFE) <- "Bayes Factor"

  if (log) {
    BFE[[1]] <- log(BFE[[1]])
    colnames(BFE) <- "Bayes Factor (log)"
  }

  print.data.frame(BFE, digits = digits)
  cat("---\n")
  cat("Method: Savage-Dickey density ratio\n")
  cat(paste0("Test Value: ", round(attr(BFE, "hypothesis"),digits), "\n"))
  invisible(x)
}
