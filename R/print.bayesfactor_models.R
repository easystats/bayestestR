#' @importFrom insight print_color
#' @export
print.bayesfactor_models <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  denominator <- attr(BFE, "denominator")
  grid.type <- attr(BFE, "BF_method")

  BFE <- as.data.frame(BFE)
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  BFE$BF <- .format_big_small(BFE$BF, digits = digits)

  # indicate null-model
  BFE$Model[BFE$Model == "1"] <- "(Intercept only)"

  BFE$Model <- paste0(" [", seq_len(nrow(BFE)), "] ", BFE$Model)
  denM <- .trim(BFE$Model[denominator])
  BFE <- BFE[-denominator, ]
  BFE$Model <- format(BFE$Model)
  colnames(BFE) <- c(format(" Model", width = max(nchar(BFE$Model))), "BF")

  insight::print_color("# Bayes Factors for Model Comparison\n\n", "blue")

  print.data.frame(BFE, digits = digits, quote = FALSE, row.names = FALSE)
  cat("\n* Against Denominator: ")
  insight::print_color(denM, "cyan")
  cat("\n*   Bayes Factor Type: ")
  insight::print_color(grid.type, "cyan")
  cat("\n")
  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")
  invisible(x)
}
