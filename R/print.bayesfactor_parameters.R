#' @export
print.bayesfactor_parameters <- function(x, digits = 3, log = FALSE, ...) {
  null <- attr(x, "hypothesis")
  direction <- attr(x, "direction")

  # format table
  BFE <- as.data.frame(x)
  if (log) {
    BFE$BF <- log(BFE$BF)
  }
  BFE$BF <- insight::format_value(BFE$BF, digits = digits, missing = "NA")

  caption <- c(sprintf(
    "# Bayes Factor (%s)\n\n",
    if (length(null) == 1) "Savage-Dickey density ratio" else "Null-Interval"
  ), "blue")

  footer <- list(
    c("* Evidence Against The Null: "),
    c(sprintf("[%s]", paste0(round(null, digits), collapse = ", ")), "cyan"),
    if (direction) c("\n*                 Direction: "),
    if (direction < 0) c("Left-Sided test", "cyan"),
    if (direction > 0) c("Right-Sided test", "cyan"),
    if (log) c("\n\nBayes Factors are on the log-scale.\n", "red")
  )

  {
    insight::print_color(caption[1], caption[2])
    print_data_frame(BFE, digits = digits)
    lapply(footer, function(txt) {
      if (length(txt) == 2)
        insight::print_color(txt[1], txt[2])
      else
        cat(txt)
      NULL
    })
  }


  invisible(x)
}
