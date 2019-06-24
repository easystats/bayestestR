#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  null <- attr(BFE, "hypothesis")
  direction <- attr(BFE, "direction")

  if (log) {
    BFE$BF <- log(BFE$BF)
  }

  BFE$BF <- format_big_small(BFE$BF, digits = digits)

  if (log) {
    colnames(BFE)[colnames(BFE) == "BF"] <- "log(Bayes Factor)"
  } else {
    colnames(BFE)[colnames(BFE) == "BF"] <- "Bayes Factor"
  }

  insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")

  print.data.frame(BFE, digits = digits, row.names = FALSE)
  cat("---\n")
  cat("Evidence Against The Null: [", paste0(round(null, digits),collapse = ", "), "]\n", sep = "")
  if (direction < 0) {
    cat("Left-Sided test\n")
  } else if (direction > 0) {
    cat("Right-Sided test\n")
  }
  invisible(x)
}
