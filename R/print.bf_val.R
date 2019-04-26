#' @export
print.bf_val <- function(x, digits = 2, logBF = FALSE, ...) {
  method <- attr(x, "method")

  df <- data.frame(BF = x, row.names = names(x))
  colnames(df) <- "Bayes Factor"

  if (logBF) {
    df[[1]] <- log(df[[1]])
    colnames(df) <- "Bayes Factor (log)"
  }

  print.data.frame(df,digits = digits)
  cat("---\n")
  cat(paste0("Method: ", method,'\n'))
}