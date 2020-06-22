#' @export
print.bayesfactor_inclusion <- function(x, digits = 3, log = FALSE, ...) {
  BFE <- x
  priorOdds <- attr(BFE, "priorOdds")

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")

  colnames(BFE) <- c("Pr(prior)", "Pr(posterior)", "Inclusion BF")

  insight::print_color("# Inclusion Bayes Factors (Model Averaged)\n\n", "blue")
  print.data.frame(BFE, digits = digits)

  cat("\n")
  cat("* Compared among: ")
  if (attr(BFE, "matched")) {
    insight::print_color("matched models only\n", "cyan")
  } else {
    insight::print_color("all models\n", "cyan")
  }

  cat("*    Priors odds: ")
  if (!is.null(priorOdds)) {
    insight::print_color("custom\n", "cyan")
  } else {
    insight::print_color("uniform-equal\n", "cyan")
  }

  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")
  invisible(x)
}
