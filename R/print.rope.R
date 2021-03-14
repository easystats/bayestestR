#' @importFrom insight print_color
#' @export
print.rope <- function(x, digits = 2, ...) {
  orig_x <- x

  # If the model is multivariate, we have have different ROPES depending on
  # the outcome variable.
  is_multivariate <- length(unique(x$Response)) > 1

  if (isTRUE(is_multivariate)) {
    insight::print_color(sprintf(
      "# Proportion%s of samples inside the ROPE.\nROPE with depends on outcome variable.\n\n",
      ifelse(all(x$CI[1] == x$CI), "", "s")
    ), "blue")
  } else {
    insight::print_color(sprintf(
      "# Proportion%s of samples inside the ROPE [%.*f, %.*f]:\n\n",
      ifelse(all(x$CI[1] == x$CI), "", "s"),
      digits,
      x$ROPE_low[1],
      digits,
      x$ROPE_high[1]
    ), "blue")
  }


  # I think this is something nobody will understand and we'll probably forget
  # why we did this, so I'll comment a bit...

  # These are the base columns we want to print
  cols <- c(
    "Parameter", "ROPE_Percentage", "Effects", "Component",
    if (is_multivariate) c("ROPE_low", "ROPE_high")
  )

  # In case we have ropes for different CIs, we also want this information
  # So we first check if values in the CI column differ, and if so, we also
  # keep this column for printing
  if (!all(x$CI[1] == x$CI)) {
    cols <- c("CI", cols)
  }

  # Either way, we need to know the different CI-values, so we can
  # split the data frame for printing later...
  ci <- unique(x$CI)

  # now we check which of the requested columns are actually in our data frame "x"
  # "x" may differ, depending on if "rope()" was called with a model-object,
  # or with a simple vector. So we can't hard-code this
  x <- subset(x, select = intersect(cols, colnames(x)))

  # This is just cosmetics, to have nicer column names and values
  x$ROPE_Percentage <- sprintf("%.*f %%", digits, x$ROPE_Percentage * 100)
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  # Add ROPE width for multivariate models
  if (isTRUE(is_multivariate)) {
    # This is just cosmetics, to have nicer column names and values
    x$ROPE_low <- sprintf("[%.*f, %.*f]", digits, x$ROPE_low, digits, x$ROPE_high)
    colnames(x)[which(colnames(x) == "ROPE_low")] <- "ROPE width"
    x$ROPE_high <- NULL
  }

  # In case we have multiple CI values, we create a subset for each CI value.
  # Else, parameter-rows would be mixed up with both CIs, which is a bit
  # more difficult to read...
  if (length(ci) == 1) {
    # print complete data frame, because we have no different CI values here
    print_data_frame(x, digits = digits)
  } else {
    for (i in ci) {
      xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
      insight::print_color(sprintf("ROPE for the %s%% HDI:\n\n", 100 * i), "cyan")
      print_data_frame(xsub, digits = digits)
      cat("\n")
    }
  }
  invisible(orig_x)
}
