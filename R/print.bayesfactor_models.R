#' @export
print.bayesfactor_models <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  denominator <- attr(BFE, "denominator")
  grid.type <- attr(BFE, "BF_method")

  BFE_ <- as.data.frame(BFE)
  if (log) {
    BFE_$BF <- log(BFE_$BF)
  }


  # indicate null-model
  BFE_$Model[BFE_$Model == "1"] <- "(Intercept only)"

  rownames(BFE_) <- paste0("[", seq_len(nrow(BFE_)), "] ", BFE_$Model, "   ")
  denM <- rownames(BFE_)[denominator]
  BFE_ <- BFE_[-denominator, "BF", drop = FALSE]
  colnames(BFE_) <- ""

  cat("Bayes factor analysis
--------------")
  print.data.frame(BFE_, digits = digits)
  cat("
Against denominator:
	", denM)
  cat("
---
Bayes factor type: ", grid.type, "
")
  if (log) cat("Presenting log(BF)
")
  invisible(x)
}
