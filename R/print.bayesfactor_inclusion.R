#' @export
print.bayesfactor_inclusion <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  priorOdds <- attr(BFE, "priorOdds")

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  BFE$BF <- format_big_small(BFE$BF, digits = digits)

  if (log) {
    colnames(BFE) <- c("Pr(prior)", "Pr(posterior)", "log(Inclusion BF)")
  } else {
    colnames(BFE) <- c("Pr(prior)", "Pr(posterior)", "Inclusion BF")
  }

  print.data.frame(BFE, digits = digits)

  cat("---\n")
  if (attr(BFE, "matched")) {
    cat("Inclusion BFs compared among matched models only.\n")
  } else {
    cat("Inclusion BFs compared among all models.\n")
  }

  if (!is.null(priorOdds)) {
    cat("Priors based on custom prior-odds.\n")
  }
  invisible(x)
}
