#' @keywords internal
.update_to_priors <- function(model, verbose = TRUE) {
  UseMethod(".update_to_priors")
}



#' @keywords internal
#' @importFrom stats update
#' @importFrom utils capture.output
.update_to_priors.stanreg <- function(model, verbose = TRUE) {
  if (!requireNamespace("rstanarm")) {
    stop("Package \"rstanarm\" needed for this function to work. Please install it.")
  }

  if (verbose) {
    message("Computation of Bayes factors: sampling priors, please wait...")
  }

  utils::capture.output(
    model_prior <- suppressWarnings(
      stats::update(model, prior_PD = TRUE)
    )
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

  if (verbose) {
    message("Computation of Bayes factors: sampling priors, please wait...")
  }

  utils::capture.output(
    model_prior <- try(suppressMessages(suppressWarnings(
      stats::update(model, sample_prior = "only")
    )), silent = TRUE)
  )

  if (is(model_prior, "try-error")) {
    if (grepl("proper priors", model_prior)) {
      stop("Cannot compute BF for 'brmsfit' models fit with default priors.\n",
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
  big_ind <- abs(BF) >= (10*10^digits) | abs(BF) < 1 / (10^digits)
  big_ind <- sapply(big_ind, isTRUE)
  if (isTRUE(any(big_ind))) {
    BFx[big_ind] <- formatC(BF, format = "e", digits = digits)[big_ind]
  }
  BFx
}