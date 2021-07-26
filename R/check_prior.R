#' Check if Prior is Informative
#'
#' Performs a simple test to check whether the prior is informative to the
#' posterior. This idea, and the accompanying heuristics, were discussed in
#' [this blogpost](https://statmodeling.stat.columbia.edu/2019/08/10/).
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
#' @examples
#' \dontrun{
#' library(bayestestR)
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 1, refresh = 0)
#'   check_prior(model, method = "gelman")
#'   check_prior(model, method = "lakeland")
#'
#'   # An extreme example where both methods diverge:
#'   model <- stan_glm(mpg ~ wt,
#'     data = mtcars[1:3, ],
#'     prior = normal(-3.3, 1, FALSE),
#'     prior_intercept = normal(0, 1000, FALSE),
#'     refresh = 0
#'   )
#'   check_prior(model, method = "gelman")
#'   check_prior(model, method = "lakeland")
#'   plot(si(model)) # can provide visual confirmation to the Lakeland method
#' }
#' }
#' @references https://statmodeling.stat.columbia.edu/2019/08/10/
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


  # sanity check for matching parameters. Some weird priors like
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
      warning("Parameters and priors could not be fully matched. Only returning results for parameters with matching priors.", call. = FALSE)
    }
  }


  # for priors whose distribution cannot be simulated, prior values are
  # all NA. Catch those, and warn user
  all_missing <- sapply(priors, function(i) {
    all(is.na(i))
  })

  if (any(all_missing) && verbose) {
    warning("Some priors could not be simulated.", call. = FALSE)
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
      hdi <- hdi(prior, ci = .95)
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
    stop("method should be 'gelman' or 'lakeland'.")
  }

  data.frame(
    Parameter = names(posteriors),
    Prior_Quality = unname(result),
    stringsAsFactors = FALSE
  )
}
