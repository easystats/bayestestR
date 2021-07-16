#' Un-update Bayesian models to their prior-to-data state
#'
#' As posteriors are priors that have been updated after observing some data,
#' the goal of this function is to un-update the posteriors to obtain models
#' representing the priors. These models can then be used to examine the prior
#' predictive distribution, or to compare priors with posteriors.
#' \cr\cr
#' This function in used internally to compute Bayes factors.
#'
#' @param model A fitted Bayesian model.
#' @param verbose Toggle warnings.
#' @param newdata List of \code{data.frames} to update the model with new data. Required even if the original data should be used.
#' @param ... Not used
#'
#' @return A model un-fitted to the data, representing the prior model.
#'
#' @keywords internal
#' @export
unupdate <- function(model, verbose = TRUE, ...) {
  UseMethod("unupdate")
}



#' @export
#' @rdname unupdate
unupdate.stanreg <- function(model, verbose = TRUE, ...) {
  insight::check_if_installed("rstanarm")

  prior_PD <- stats::getCall(model)$prior_PD
  if (!is.null(prior_PD) && isTRUE(eval(parse(text = prior_PD)))) {
    return(model)
  }

  if (verbose) {
    message("Sampling priors, please wait...")
  }

  prior_dists <- sapply(rstanarm::prior_summary(model), `[[`, "dist")
  if (anyNA(prior_dists)) {
    stop(
      "Cannot sample from flat priors (such as when priors are ",
      "set to 'NULL' in a 'stanreg' model).",
      call. = FALSE
    )
  }

  model_prior <- suppressWarnings(
    stats::update(model, prior_PD = TRUE, refresh = 0)
  )

  model_prior
}



#' @export
#' @rdname unupdate
unupdate.brmsfit <- function(model, verbose = TRUE, ...) {
  insight::check_if_installed("brms")

  if (isTRUE(attr(model$prior, "sample_prior") == "only")) {
    return(model)
  }

  if (verbose) {
    message("Sampling priors, please wait...")
  }

  utils::capture.output(
    model_prior <- try(suppressMessages(suppressWarnings(
      stats::update(model, sample_prior = "only", refresh = 0)
    )), silent = TRUE)
  )

  if (methods::is(model_prior, "try-error")) {
    if (grepl("proper priors", model_prior)) {
      stop(
        "Cannot sample from flat priors (such as the default ",
        "priors for fixed-effects in a 'brmsfit' model).",
        call. = FALSE
      )
    } else {
      stop(model_prior)
    }
  }

  model_prior
}


#' @export
#' @rdname unupdate
unupdate.brmsfit_multiple <- function(model,
                                      verbose = TRUE,
                                      newdata = NULL,
                                      ...) {
  insight::check_if_installed("brms")

  if (isTRUE(attr(model$prior, "sample_prior") == "only")) {
    return(model)
  }

  if (verbose) {
    message("Sampling priors, please wait...")
  }

  utils::capture.output(model_prior <-
    try(suppressMessages(suppressWarnings(
      stats::update(
        model,
        sample_prior = "only",
        newdata = newdata,
        refresh = 0
      )
    )), silent = TRUE))

  if (methods::is(model_prior, "try-error")) {
    if (grepl("proper priors", model_prior)) {
      stop(
        "Cannot sample from flat priors (such as the default ",
        "priors for fixed-effects in a 'brmsfit' model).",
        call. = FALSE
      )
    } else {
      stop(model_prior)
    }
  }

  model_prior
}


#' @export
#' @rdname unupdate
unupdate.blavaan <- function(model, verbose = TRUE, ...) {
  insight::check_if_installed("blavaan")

  cl <- model@call
  if (isTRUE(eval(cl$prisamp))) {
    return(model)
  }

  if (verbose) {
    message("Sampling priors, please wait...")
  }

  cl$prisamp <- TRUE
  suppressMessages(suppressWarnings(
    utils::capture.output(model_prior <- eval(cl))
  ))

  model_prior
}
