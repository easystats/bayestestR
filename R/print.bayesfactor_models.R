#' @export
print.bayesfactor_models <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  denominator <- attr(BFE, "denominator")
  grid.type <- attr(BFE, "BF_method")

  BFE <- as.data.frame(BFE)
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  xBF <- BFE$BF
  BFE$BF <- as.character(round(xBF, digits = digits))
  big_ind <- abs(xBF) >= 1000 | abs(xBF) < 1 / (10 ^ digits)
  big_ind <- sapply(big_ind, isTRUE)
  BFE$BF[big_ind] <- formatC(xBF, format = "e", digits = digits)[big_ind]

  # indicate null-model
  BFE$Model[BFE$Model == "1"] <- "(Intercept only)"

  rownames(BFE) <- paste0("[", seq_len(nrow(BFE)), "] ", BFE$Model, "   ")
  denM <- rownames(BFE)[denominator]
  BFE <- BFE[-denominator, "BF", drop = FALSE]
  colnames(BFE) <- ""

  cat("Bayes factor analysis
---------------------")
  print.data.frame(BFE, digits = digits)
  cat("\nAgainst denominator:\n\t\t", denM)
  cat("\n---\nBayes factor type: ", grid.type, "\n")
  if (log) cat("Presenting log(BF)\n")
  invisible(x)
}
