#' @importFrom insight print_color
#' @export
print.equivalence_test <- function(x, digits = 2, ...) {
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, x$ROPE_low[1], digits, x$ROPE_high[1]))

  # find the longest HDI-value, so we can align the brackets in the ouput
  x$HDI_low <- sprintf("%.*f", digits, x$HDI_low)
  x$HDI_high <- sprintf("%.*f", digits, x$HDI_high)

  maxlen_low <- max(nchar(x$HDI_low))
  maxlen_high <- max(nchar(x$HDI_high))

  x$ROPE_Percentage <- sprintf("%.*f %%", digits, x$ROPE_Percentage)
  x$HDI <- sprintf("[%*s %*s]", maxlen_low, x$HDI_low, maxlen_high, x$HDI_high)

  ci <- unique(x$CI)
  keep.columns <- c("CI", "Parameter", "ROPE_Equivalence", "ROPE_Percentage", "HDI")

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  # clean parameter names
  if ("Parameter" %in% colnames(x)) {
    x$Parameter <- gsub("^(b_zi_|b_|bs_|bsp_|bcs_)(.*)", "\\2", x$Parameter)
  }

  for (i in ci) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
    colnames(xsub)[ncol(xsub)] <- sprintf("%i%% HDI", i)
    print.data.frame(xsub, digits = digits, row.names = FALSE)
    cat("\n")
  }
}

