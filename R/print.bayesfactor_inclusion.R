#' @export
print.bayesfactor_inclusion <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x

  if (log) {
    colnames(BFE) <- c("Pr(prior)", "Pr(posterior)", "log(Inclusion BF)")
    BFE$BF <- log(BFE$BF)
  } else {
    colnames(BFE) <- c("Pr(prior)", "Pr(posterior)", "Inclusion BF")

  }


  print.data.frame(BFE, digits = digits)

  if (attr(BFE, "matched")) {
    cat("---
Inclusion BFs compared among matched models only.")
  } else {
    cat("---
Inclusion BFs compared among all models.")
  }

  if (!is.null(attr(BFE, "priorOdds"))) {
    cat("
Priors based on custom prior-odds.")
  }
  invisible(x)
}
