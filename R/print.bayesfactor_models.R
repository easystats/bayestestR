#' @export
print.bayesfactor_models <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  denominator <- attr(BFE,'denominator')
  grid.type <- attr(BFE,'BF_method')

  BFE_ <- as.data.frame(BFE)
  if (!log) BFE_$log.BF <- exp(BFE_$log.BF)

  rownames(BFE_) <- paste0('[',seq_len(nrow(BFE_)),'] ',BFE_$Model, '   ')
  denM <- rownames(BFE_)[denominator]
  BFE_ <- BFE_[-denominator,'log.BF',drop = FALSE]
  colnames(BFE_) <- ""

  cat('Bayes factor analysis\n--------------')
  print.data.frame(BFE_,digits = digits)
  cat('\nAgainst denominator:\n\t', denM)
  cat('\n---\nBayes factor type: ', grid.type)
  if (log) cat('\nPresenting log(BF)')
  invisible(x)
}
