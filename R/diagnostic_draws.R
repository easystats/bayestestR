#' Diagnostic values for each iteration
#'
#' Returns the accumulated log-posterior, the average Metropolis acceptance rate, divergent transitions, treedepth rather than terminated its evolution normally.
#' @inheritParams diagnostic_posterior
#'
#' @examples
#' \donttest{
#' set.seed(333)
#'
#' if (require("brms", quietly = TRUE)) {
#'   model <- suppressWarnings(brm(mpg ~ wt * cyl * vs,
#'     data = mtcars,
#'     iter = 100, control = list(adapt_delta = 0.80),
#'     refresh = 0
#'   ))
#'   diagnostic_draws(model)
#' }
#' }
#'
#' @export
diagnostic_draws <- function(posterior, ...) {
  UseMethod("diagnostic_draws")
}


#' @export
diagnostic_draws.brmsfit <- function(posterior, ...) {
  insight::check_if_installed("brms")

  nuts_parameters <- brms::nuts_params(posterior)
  nuts_parameters$idvar <- paste0(
    nuts_parameters$Chain,
    "_",
    nuts_parameters$Iteration
  )
  out <- stats::reshape(
    nuts_parameters,
    v.names = "Value",
    idvar = "idvar",
    timevar = "Parameter",
    direction = "wide"
  )
  out$idvar <- NULL
  out <- merge(
    out,
    brms::log_posterior(posterior),
    by = c("Chain", "Iteration"),
    sort = FALSE
  )

  # Rename
  names(out)[names(out) == "Value.accept_stat__"] <- "Acceptance_Rate"
  names(out)[names(out) == "Value.treedepth__"] <- "Tree_Depth"
  names(out)[names(out) == "Value.stepsize__"] <- "Step_Size"
  names(out)[names(out) == "Value.divergent__"] <- "Divergent"
  names(out)[names(out) == "Value.n_leapfrog__"] <- "n_Leapfrog"
  names(out)[names(out) == "Value.energy__"] <- "Energy"
  names(out)[names(out) == "Value"] <- "LogPosterior"

  out
}
