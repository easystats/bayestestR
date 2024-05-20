#' Check if Prior is Informative
#'
#' Performs a simple test to check whether the prior is informative to the
#' posterior. This idea, and the accompanying heuristics, were discussed in
#' _Gelman et al. 2017_.
#'
#' @param method Can be `"gelman"` or `"lakeland"`. For the
#'   `"gelman"` method, if the SD of the posterior is more than 0.1 times
#'   the SD of the prior, then the prior is considered as informative. For the
#'   `"lakeland"` method, the prior is considered as informative if the
#'   posterior falls within the `95%` HDI of the prior.
#' @param simulate_priors Should prior distributions be simulated using
#'   [simulate_prior()] (default; faster) or sampled via
#'   [unupdate()] (slower, more accurate).
#' @inheritParams effective_sample
#' @inheritParams hdi
#'
#' @return A data frame with two columns: The parameter names and the quality
#'   of the prior (which might be `"informative"`, `"uninformative"`)
#'   or `"not determinable"` if the prior distribution could not be
#'   determined).
#'
#' @examplesIf require("rstanarm") && require("see")
#' \donttest{
#' library(bayestestR)
#' model <- rstanarm::stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#' check_prior(model, method = "gelman")
#' check_prior(model, method = "lakeland")
#'
#' # An extreme example where both methods diverge:
#' model <- rstanarm::stan_glm(mpg ~ wt,
#'   data = mtcars[1:3, ],
#'   prior = normal(-3.3, 1, FALSE),
#'   prior_intercept = normal(0, 1000, FALSE),
#'   refresh = 0
#' )
#' check_prior(model, method = "gelman")
#' check_prior(model, method = "lakeland")
#' # can provide visual confirmation to the Lakeland method
#' plot(si(model, verbose = FALSE))
#' }
#' @references
#' Gelman, A., Simpson, D., and Betancourt, M. (2017). The Prior Can Often Only
#' Be Understood in the Context of the Likelihood. Entropy, 19(10), 555.
#' \doi{10.3390/e19100555}
#'
#' @export
check_prior <- function(model, method = "gelman", simulate_priors = TRUE, ...) {
  UseMethod("check_prior")
}





#' @export
check_prior.brmsfit <- function(model,
                                method = "gelman",
                                simulate_priors = TRUE,
                                effects = c("fixed", "random", "all"),
                                component = c("conditional", "zi", "zero_inflated", "all"),
                                parameters = NULL,
                                verbose = TRUE,
                                ...) {
  # check arguments
  effects <- match.arg(effects)
  component <- match.arg(component)

  posteriors <- insight::get_parameters(
    model,
    effects = effects,
    component = component,
    parameters = parameters
  )

  if (isTRUE(simulate_priors)) {
    priors <- simulate_prior(
      model,
      effects = effects,
      component = component,
      parameters = parameters,
      verbose = verbose
    )
  } else {
    priors <- unupdate(model, verbose = FALSE)
    priors <- insight::get_parameters(
      priors,
      effects = effects,
      component = component,
      parameters = parameters
    )
  }

  .check_prior(priors, posteriors, method,
    verbose = verbose,
    cleaned_parameters = insight::clean_parameters(model)
  )
}

#' @export
check_prior.stanreg <- check_prior.brmsfit

#' @export
check_prior.blavaan <- check_prior.brmsfit


#' @keywords internal
.check_prior <- function(priors,
                         posteriors,
                         method = "gelman",
                         verbose = TRUE,
                         cleaned_parameters = NULL) {
  # validation check for matching parameters. Some weird priors like
  # rstanarm's R2 prior might cause problems

  if (!is.null(cleaned_parameters) && ncol(priors) != ncol(posteriors)) {
    ## TODO for now only fixed effects
    if ("Effects" %in% colnames(cleaned_parameters)) {
      cleaned_parameters <- cleaned_parameters[cleaned_parameters$Effects == "fixed", ]
    }

    # rename cleaned parameters, so they match name of prior parameter column
    cp <- cleaned_parameters$Cleaned_Parameter
    cp <- gsub("(.*)(\\.|\\[)\\d+(\\.|\\])", "\\1", cp)
    cp[cp == "Intercept"] <- "(Intercept)"
    cleaned_parameters$Cleaned_Parameter <- cp
    colnames(priors)[colnames(priors) == "Intercept"] <- "(Intercept)"

    # at this point, the colnames of "posteriors" should match "cp$Parameter",
    # while colnames of "priors" should match "cp$Cleaned_Parameter". To ensure
    # that ncol of priors is the same as ncol of posteriors, we now duplicate
    # prior columns and match them with the posteriors

    if (ncol(posteriors) > ncol(priors)) {
      matched_columns <- stats::na.omit(match(cleaned_parameters$Cleaned_Parameter, colnames(priors)))
      matched_column_names <- stats::na.omit(match(colnames(priors), cleaned_parameters$Cleaned_Parameter))
      priors <- priors[matched_columns]
    } else {
      matched_columns <- stats::na.omit(match(colnames(priors), cleaned_parameters$Cleaned_Parameter))
      matched_column_names <- stats::na.omit(match(cleaned_parameters$Cleaned_Parameter, colnames(priors)))
      priors <- priors[matched_columns]
    }
    colnames(priors) <- cleaned_parameters$Parameter[matched_column_names]
  }

  # still different ncols?
  if (ncol(priors) != ncol(posteriors)) {
    common_columns <- intersect(colnames(priors), colnames(posteriors))
    priors <- priors[common_columns]
    posteriors <- posteriors[common_columns]
    if (verbose) {
      insight::format_warning(
        "Parameters and priors could not be fully matched. Only returning results for parameters with matching priors."
      )
    }
  }


  # for priors whose distribution cannot be simulated, prior values are
  # all NA. Catch those, and warn user
  all_missing <- vapply(priors, function(i) all(is.na(i)), TRUE)

  if (any(all_missing) && verbose) {
    insight::format_warning("Some priors could not be simulated.")
  }

  .gelman <- function(prior, posterior) {
    if (all(is.na(prior))) {
      "not determinable"
    } else if (stats::sd(posterior, na.rm = TRUE) > 0.1 * stats::sd(prior, na.rm = TRUE)) {
      "informative"
    } else {
      "uninformative"
    }
  }

  .lakeland <- function(prior, posterior) {
    if (all(is.na(prior))) {
      "not determinable"
    } else {
      hdi <- hdi(prior, ci = 0.95)
      r <- rope(posterior, ci = 1, range = c(hdi$CI_low, hdi$CI_high))
      if (as.numeric(r) > 0.99) {
        "informative"
      } else {
        "misinformative"
      }
    }
  }

  if (method == "gelman") {
    result <- mapply(.gelman, priors, posteriors)
  } else if (method == "lakeland") {
    result <- mapply(.lakeland, priors, posteriors)
  } else {
    insight::format_error("method should be 'gelman' or 'lakeland'.")
  }

  data.frame(
    Parameter = names(posteriors),
    Prior_Quality = unname(result),
    stringsAsFactors = FALSE
  )
}
