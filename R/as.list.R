
# as.list -----------------------------------------------------------------

#' @export
as.list.bayestestR_hdi <- function(x, ...) {
  if (nrow(x) == 1) {
    out <- list(CI = x$CI, CI_low = x$CI_low, CI_high = x$CI_high)
    out$Parameter <- x$Parameter
  } else {
    out <- list()
    for (param in x$Parameter) {
      out[[param]] <- list()
      out[[param]][["CI"]] <- x[x$Parameter == param, "CI"]
      out[[param]][["CI_low"]] <- x[x$Parameter == param, "CI_low"]
      out[[param]][["CI_high"]] <- x[x$Parameter == param, "CI_high"]
    }
  }
  out
}

#' @export
as.list.bayestestR_eti <- as.list.bayestestR_hdi

#' @export
as.list.bayestestR_si <- as.list.bayestestR_hdi

#' @export
as.list.bayestestR_ci <- as.list.bayestestR_hdi
