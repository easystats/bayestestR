#' @export
print.bayesfactor_models <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  denominator <- attr(BFE, "denominator")
  grid.type <- attr(BFE, "BF_method")

  BFE <- as.data.frame(BFE)
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  BFE$BF <- format_big_small(BFE$BF, digits = digits)

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
