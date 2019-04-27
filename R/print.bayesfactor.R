#' @export
print.bayesfactor <- function(x, digits = 2, log = FALSE, ...) {

  df <- data.frame(BF = x, row.names = names(x))
  colnames(df) <- "Bayes Factor"

  if (log) {
    df[[1]] <- log(df[[1]])
    colnames(df) <- "Bayes Factor (log)"
  }

  print.data.frame(df, digits = digits)
  cat("---\n")
  cat(paste0("H0: ", attr(x, "H0"),'\n'))
  cat(paste0("Method: ", attr(x, "method"),'\n'))
}