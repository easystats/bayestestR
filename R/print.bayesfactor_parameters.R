#' @export
print.bayesfactor_parameters <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  null <- attr(BFE, "hypothesis")
  direction <- attr(BFE, "direction")

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  BFE$BF <- .format_big_small(BFE$BF, digits = digits)

  colnames(BFE)[colnames(BFE) == "BF"] <- "Bayes Factor"

  if (length(null) == 1) {
    insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")
  } else {
    insight::print_color("# Bayes Factor (Null-Interval)\n\n", "blue")
  }

  print.data.frame(BFE, digits = digits, row.names = FALSE)
  cat("\n")
  cat("* Evidence Against The Null: ")
  insight::print_color(
    paste0("[", paste0(round(null, digits), collapse = ", "), "]\n", sep = ""),
    "cyan"
  )

  if (direction < 0) {
    cat("*                 Direction: ")
    insight::print_color("Left-Sided test\n", "cyan")
  } else if (direction > 0) {
    cat("*                 Direction: ")
    insight::print_color("Right-Sided test\n", "cyan")
  }

  if (log) insight::print_color("\nBayes Factors are on the log-scale.\n", "red")
  invisible(x)
}
