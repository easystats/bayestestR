#' @keywords internal
.check_multicollinearity <- function(model,
                                     method = "equivalence_test",
                                     threshold = 0.7, ...) {
  if (inherits(model, "CmdStanFit")) {
    return()
  }

  valid_parameters <- insight::find_parameters(
    model,
    parameters = "^(?!(r_|sd_|prior_|cor_|lp__|b\\[))",
    flatten = TRUE
  )

  if (inherits(model, "stanfit")) {
    dat <- insight::get_parameters(model)[, valid_parameters, drop = FALSE]
  } else {
    dat <- as.data.frame(model, optional = FALSE)[, valid_parameters, drop = FALSE]
  }

  # need at least three columns, one is removed anyway...

  if (ncol(dat) > 2) {
    dat <- dat[, -1, drop = FALSE]

    if (ncol(dat) > 1) {
      parameter_correlation <- stats::cor(dat)
      parameter <- expand.grid(colnames(dat), colnames(dat), stringsAsFactors = FALSE)

      results <- cbind(
        parameter,
        corr = abs(as.vector(expand.grid(parameter_correlation)[[1]])),
        pvalue = apply(parameter, 1, function(r) stats::cor.test(dat[[r[1]]], dat[[r[2]]])$p.value)
      )

      # Filter
      results <- results[results$pvalue < 0.05 & results$Var1 != results$Var2, ]

      if (nrow(results) > 0) {
        # Remove duplicates
        results$where <- paste0(results$Var1, " and ", results$Var2)
        results$where2 <- paste0(results$Var2, " and ", results$Var1)
        to_remove <- NULL
        for (i in seq_len(nrow(results))) {
          if (results$where2[i] %in% results$where[1:i]) {
            to_remove <- c(to_remove, i)
          }
        }
        results <- results[-to_remove, ]

        # Filter by first threshold
        threshold <- pmin(threshold, 0.9)
        results <- results[results$corr > threshold & results$corr <= 0.9, ]
        if (nrow(results) > 0) {
          where <- paste0("between ", toString(paste0(results$where, " (r = ", round(results$corr, 2), ")")), "")
          insight::format_alert(paste0(
            "Possible multicollinearity ",
            where,
            ". This might lead to inappropriate results. See 'Details' in '?",
            method,
            "'."
          ))
        }

        # Filter by second threshold
        results <- results[results$corr > 0.9, ]
        if (nrow(results) > 0) {
          where <- paste0("between ", toString(paste0(results$where, " (r = ", round(results$corr, 2), ")")), "")
          insight::format_alert(paste0(
            "Probable multicollinearity ",
            where,
            ". This might lead to inappropriate results. See 'Details' in '?",
            method,
            "'."
          ))
        }
      }
    }
  }
}
