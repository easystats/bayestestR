#' @importFrom stats cor cor.test
#' @importFrom insight find_parameters
#' @keywords internal
.check_multicollinearity <- function(model, method = "equivalence_test") {
  valid_parameters <- insight::find_parameters(model, parameters = "^(?!(r_|sd_|prior_|cor_|b\\[))", flatten = TRUE)
  dat <- as.data.frame(model)[, valid_parameters]
  dat <- dat[, -1, drop = FALSE]

  if (ncol(dat) > 1) {
    parameter_correlation <- stats::cor(dat)
    parameter <- expand.grid(colnames(dat), colnames(dat), stringsAsFactors = FALSE)

    results <- cbind(
      parameter,
      corr = abs(as.vector(expand.grid(parameter_correlation)[[1]])),
      pvalue = apply(parameter, 1, function(r) stats::cor.test(dat[[r[1]]], dat[[r[2]]])$p.value)
    )

    results <- results[results$pvalue < 0.05 & results$Var1 != results$Var2, ]

    if (nrow(results) > 0 && any(results$corr >= 0.5)) {
      warning("Possible multicollinearity, leading to inappropriate results. See 'Details' in '?", method, "'.", call. = FALSE)
    }
  }
}
