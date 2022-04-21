#' Diagnostic values for each iteration
#'
#' Returns the accumulated log-posterior, the average Metropolis acceptance rate, divergent transitions, treedepth rather than terminated its evolution normally.
#' @inheritParams diagnostic_posterior
#'
#' @examples
#' \dontrun{
#' set.seed(333)
#'
#' if (require("brms", quietly = TRUE)) {
#'   model <- brm(mpg ~ wt * cyl * vs,
#'     data = mtcars,
#'     iter = 100, control = list(adapt_delta = 0.80),
#'     refresh = 0
#'   )
#'   diagnostic_draws(model)
#' }
#' }
#'
#' @export
diagnostic_draws <- function(posteriors, ...) {
  UseMethod("diagnostic_draws")
}


#' @export
diagnostic_draws.brmsfit <- function(posteriors, ...) {
  insight::check_if_installed("brms")

  data <- brms::nuts_params(posteriors)
  data$idvar <- paste0(data$Chain, "_", data$Iteration)
  out <- stats::reshape(
    data,
    v.names = "Value",
    idvar = "idvar",
    timevar = "Parameter",
    direction = "wide"
  )
  out$idvar <- NULL
  out <- merge(out, brms::log_posterior(posteriors), by = c("Chain", "Iteration"), sort = FALSE)

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
