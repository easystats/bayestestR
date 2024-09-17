#' @export
print.equivalence_test <- function(x, digits = 2, ...) {
  orig_x <- x
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")
  # print ROPE limits, if we just have one set of ROPE values
  if (insight::n_unique(x$ROPE_low) == 1) {
    cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, x$ROPE_low[1], digits, x$ROPE_high[1]))
  }

  # fix "sd" pattern
  model <- .retrieve_model(x)
  if (!is.null(model) && !is.data.frame(model)) {
    cp <- insight::clean_parameters(model)
    if (!is.null(cp$Group) && any(startsWith(cp$Group, "SD/Cor"))) {
      cp <- cp[startsWith(cp$Group, "SD/Cor"), ]
      matches <- match(cp$Parameter, x$Parameter)
      if (length(matches)) {
        new_pattern <- paste0(
          "SD/Cor: ",
          cp$Cleaned_Parameter[unique(stats::na.omit(match(x$Parameter, cp$Parameter)))]
        )
        if (length(new_pattern) == length(matches)) {
          x$Parameter[matches] <- new_pattern
        }
      }
    }
  }

  x$ROPE_Percentage <- sprintf("%.*f %%", digits, x$ROPE_Percentage * 100)
  x$HDI <- insight::format_ci(x$HDI_low, x$HDI_high, ci = NULL, digits = digits)

  ci <- unique(x$CI)
  keep.columns <- c(
    attr(x, "idvars"), "Parameter", "Effects", "Component",
    "ROPE_Equivalence", "ROPE_Percentage", "CI", "HDI"
  )

  # keep ROPE columns for multiple ROPE values
  if (insight::n_unique(x$ROPE_low) > 1) {
    keep.columns <- c(keep.columns, "ROPE")
    x$ROPE <- insight::format_ci(x$ROPE_low, x$ROPE_high, ci = NULL, digits = digits)
  }

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  .print_equivalence_component(x, ci, digits)

  invisible(orig_x)
}


.print_equivalence_component <- function(x, ci, digits) {
  for (i in ci) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
    colnames(xsub)[colnames(xsub) == "HDI"] <- sprintf("%i%% HDI", 100 * i)
    .print_data_frame(xsub, digits = digits)
    cat("\n")
  }
}
