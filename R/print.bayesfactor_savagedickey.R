#' @export
print.bayesfactor_savagedickey <- function(x, digits = 2, log = FALSE, ...) {
  BFE <- x
  hypothesis <- attr(BFE, "hypothesis")
  direction <- attr(BFE, "direction")
  direction.opts <- data.frame(
    String = c("left", "right", "two-sided", "<", ">", "=", "-1", "0", "1", "+1"),
    Value = c(-1, 1, 0, -1, 1, 0, -1, 0, 1, 1)
  )
  direction <- direction.opts$Value[match(direction, direction.opts$String, 2)[1]]

  if (log) {
    BFE$BF <- log(BFE$BF)
    names(BFE)[names(BFE) == "BF"] <- "log(Bayes Factor)"
  } else {
    names(BFE)[names(BFE) == "BF"] <- "Bayes Factor"
  }

  insight::print_color("# Bayes Factor (Savage-Dickey density ratio)\n\n", "blue")

  print.data.frame(BFE, digits = digits)
  cat("---\n")
  cat(paste0("Evidence Against Test Value: ", round(hypothesis, digits), "\n"))
  if (direction < 0) {
    cat(paste0("Left-Sided test\n"))
  } else if (direction > 0) {
    cat(paste0("Right-Sided test\n"))
  }
  invisible(x)
}
