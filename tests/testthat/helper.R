skip_if_not_or_load_if_installed <- function(package, minimum_version = NULL) {
  testthat::skip_if_not_installed(package, minimum_version = minimum_version)
  suppressMessages(suppressWarnings(suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
  )))
}

skip_if_fit_failed <- function(expr) {
  suppressWarnings(
    tryCatch(
      expr,
      error = function(e) {
        testthat::skip(sprintf(
          "Fitting model failed: %s",
          conditionMessage(e)
        ))
      }
    )
  )
}
