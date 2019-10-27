#' @keywords internal
.update_to_priors <- function(model, verbose = TRUE) {
  UseMethod(".update_to_priors")
}



#' @keywords internal
#' @importFrom stats update getCall
#' @importFrom rstanarm prior_summary
.update_to_priors.stanreg <- function(model, verbose = TRUE) {
  if (!requireNamespace("rstanarm")) {
    stop("Package \"rstanarm\" needed for this function to work. Please install it.")
  }

  prior_PD <- stats::getCall(model)$prior_PD
  if (!is.null(prior_PD) && isTRUE(eval(parse(text = prior_PD)))) {
    return(model)
  }

  if (verbose) {
    message("Computation of Bayes factors: sampling priors, please wait...")
  }

  prior_dists <- sapply(rstanarm::prior_summary(model), `[[`, "dist")
  if (anyNA(prior_dists)) {
    stop(
      "Cannot compute Bayes factors with flat priors (such as when priors are ",
      "set to 'NULL' in a 'stanreg' model), as Bayes factors inform about the raltive ",
      "likelihood of two 'hypotheses', and flat priors provide no likelihood.\n",
      "See '?bayesfactor_parameters' for more information.\n",
      call. = FALSE
    )
  }

  model_prior <- suppressWarnings(
    stats::update(model, prior_PD = TRUE, refresh = 0)
  )

  model_prior
}



#' @keywords internal
#' @importFrom stats update
#' @importFrom utils capture.output
#' @importFrom methods is
.update_to_priors.brmsfit <- function(model, verbose = TRUE) {
  if (!requireNamespace("brms")) {
    stop("Package \"brms\" needed for this function to work. Please install it.")
  }

  if (isTRUE(attr(model$prior, "sample_prior") == "only")) {
    return(model)
  }

  if (verbose) {
    message("Computation of Bayes factors: sampling priors, please wait...")
  }

  utils::capture.output(
    model_prior <- try(suppressMessages(suppressWarnings(
      stats::update(model, sample_prior = "only", refresh = 0)
    )), silent = TRUE)
  )

  if (is(model_prior, "try-error")) {
    if (grepl("proper priors", model_prior)) {
      stop(
        "Cannot compute Bayes factors with flat priors (such as the default ",
        "priors for fixed-effects in a 'brmsfit' model), as Bayes factors inform about ",
        "the raltive likelihood of two 'hypotheses', and flat priors provide no ",
        "likelihood.\n",
        "See '?bayesfactor_parameters' for more information.\n",
        call. = FALSE
      )
    } else {
      stop(model_prior)
    }
  }

  model_prior
}

#' @keywords internal
.format_big_small <- function(BF, digits = 2) {
  BFx <- as.character(round(BF, digits = digits))
  big_ind <- abs(BF) >= (10 * 10^digits) | abs(BF) < 1 / (10^digits)
  big_ind <- sapply(big_ind, isTRUE)
  if (isTRUE(any(big_ind))) {
    BFx[big_ind] <- formatC(BF, format = "e", digits = digits)[big_ind]
  }
  BFx
}



# As numeric vector -------------------------------------------------------

#' @export
as.numeric.bayesfactor_inclusion <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$BF)))
  } else {
    return(as.vector(x))
  }
}

#' @export
as.numeric.bayesfactor_models <- as.numeric.bayesfactor_inclusion

#' @export
as.numeric.bayesfactor_parameters <- as.numeric.bayesfactor_inclusion

#' @export
as.numeric.bayesfactor_restricted <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_inclusion <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_models <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_parameters <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_restricted <- as.numeric.bayesfactor_inclusion
