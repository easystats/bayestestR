#' @importFrom insight print_color
#' @export
print.equivalence_test <- function(x, digits = 2, ...) {
  orig_x <- x
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, x$ROPE_low[1], digits, x$ROPE_high[1]))

  # find the longest HDI-value, so we can align the brackets in the ouput
  x$HDI_low <- sprintf("%.*f", digits, x$HDI_low)
  x$HDI_high <- sprintf("%.*f", digits, x$HDI_high)

  maxlen_low <- max(nchar(x$HDI_low))
  maxlen_high <- max(nchar(x$HDI_high))

  x$ROPE_Percentage <- sprintf("%.*f %%", digits, x$ROPE_Percentage * 100)
  x$HDI <- sprintf("[%*s %*s]", maxlen_low, x$HDI_low, maxlen_high, x$HDI_high)

  # clean parameter names
  # if ("Parameter" %in% colnames(x) && "Cleaned_Parameter" %in% colnames(x)) {
  #   x$Parameter <- x$Cleaned_Parameter
  # }

  ci <- unique(x$CI)
  keep.columns <- c("CI", "Parameter", "ROPE_Equivalence", "ROPE_Percentage", "HDI", "Effects", "Component")

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  .print_equivalence_component(x, ci, digits)

  # split_column <- ""
  # split_column <- c(split_column, ifelse("Component" %in% names(x) && length(unique(x$Component)) > 1, "Component", ""))
  # split_column <- c(split_column, ifelse("Effects" %in% names(x) && length(unique(x$Effects)) > 1, "Effects", ""))
  # split_column <- split_column[nchar(split_column) > 0]
  #
  # if (length(split_column)) {
  #
  #   # set up split-factor
  #   if (length(split_column) > 1) {
  #     split_by <- lapply(split_column, function(i) x[[i]])
  #   } else {
  #     split_by <- list(x[[split_column]])
  #   }
  #   names(split_by) <- split_column
  #
  #
  #   # make sure we have correct sorting here...
  #   tables <- split(x, f = split_by)
  #
  #   for (type in names(tables)) {
  #
  #     # Don't print Component column
  #     tables[[type]][["Effects"]] <- NULL
  #     tables[[type]][["Component"]] <- NULL
  #
  #     component_name <- switch(
  #       type,
  #       "fixed" = ,
  #       "conditional" = "Fixed Effects",
  #       "random" = "Random Effects",
  #       "conditional.fixed" = "Fixed Effects (Count Model)",
  #       "conditional.random" = "Random Effects (Count Model)",
  #       "zero_inflated" = "Zero-Inflated",
  #       "zero_inflated.fixed" = "Fixed Effects (Zero-Inflated Model)",
  #       "zero_inflated.random" = "Random Effects (Zero-Inflated Model)",
  #       "smooth_sd" = "Smooth Terms (SD)",
  #       "smooth_terms" = "Smooth Terms",
  #       type
  #     )
  #
  #     insight::print_color(sprintf(" %s\n\n", component_name), "red")
  #     .print_equivalence_component(tables[[type]], ci, digits)
  #   }
  # } else {
  #   type <- paste0(unique(x$Component), ".", unique(x$Effects))
  #   component_name <- switch(
  #     type,
  #     "conditional.fixed" = "Fixed Effects",
  #     "conditional.random" = "Random Effects",
  #     "zero_inflated.fixed" = "Fixed Effects (Zero-Inflated Model)",
  #     "zero_inflated.random" = "Random Effects (Zero-Inflated Model)",
  #     type
  #   )
  #
  #   x$Effects <- NULL
  #   x$Component <- NULL
  #
  #   insight::print_color(sprintf(" %s\n\n", component_name), "red")
  #   .print_equivalence_component(x, ci, digits)
  # }
  invisible(orig_x)
}


.print_equivalence_component <- function(x, ci, digits) {
  for (i in ci) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
    colnames(xsub)[colnames(xsub) == "HDI"] <- sprintf("%i%% HDI", i)
    print_data_frame(xsub, digits = digits)
    cat("\n")
  }
}
