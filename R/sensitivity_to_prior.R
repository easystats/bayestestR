#' Sensitivity to Prior
#'
#' Computes the sensitivity to priors specification. This represents the
#' proportion of change in some indices when the model is fitted with an
#' antagonistic prior (a prior of same shape located on the opposite of the
#' effect).
#'
#' @param model A Bayesian model (`stanreg` or `brmsfit`).
#' @param index The indices from which to compute the sensitivity. Can be one or
#'   multiple names of the columns returned by `describe_posterior`. The case is
#'   important here (e.g., write 'Median' instead of 'median').
#' @param magnitude This represent the magnitude by which to shift the
#'   antagonistic prior (to test the sensitivity). For instance, a magnitude of
#'   10 (default) means that the mode will be updated with a prior located at 10
#'   standard deviations from zero, on the opposite side of the effect.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#' For `brmsfit` models, all slopes must have a normal, student_t, or cauchy
#' prior, because only these priors are supported for moving to an antagonistic
#' location.
#' The default flat priors of **brms** are not supported, so set the priors
#' with `brms::set_prior()` when fitting the model. Multivariate models are not
#' supported.
#'
#' @return A data frame with a `Parameter` column and one `Sensitivity_*`
#'   column for `Median` and for each other index.
#'
#' @examplesIf require("rstanarm")
#' \donttest{
#' library(bayestestR)
#'
#' # rstanarm models
#' # -----------------------------------------------
#' model <- rstanarm::stan_glm(mpg ~ wt, data = mtcars)
#' sensitivity_to_prior(model)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' sensitivity_to_prior(model, index = c("Median", "MAP"))
#' }
#' @examplesIf require("brms")
#' \donttest{
#' # brms models
#' # -----------------------------------------------
#' model <- brms::brm(
#'   mpg ~ wt + cyl,
#'   data = mtcars,
#'   prior = brms::set_prior("normal(0, 5)", class = "b"),
#'   chains = 2,
#'   iter = 1000,
#'   refresh = 0
#' )
#' sensitivity_to_prior(model)
#' }
#' @seealso DescTools
#' @export
sensitivity_to_prior <- function(model, ...) {
  UseMethod("sensitivity_to_prior")
}


#' @rdname sensitivity_to_prior
#' @export
sensitivity_to_prior.stanreg <- function(model, index = "Median", magnitude = 10, ...) {
  # Original
  params <- .extract_parameters(model, index = index, ...)

  # Priors
  priors <- .extract_priors_rstanarm(model)
  new_priors <- .prior_new_location(prior = priors$prior, sign = sign(params$Median), magnitude = magnitude)
  model_updated <- stats::update(model, data = insight::get_data(model), prior = new_priors, refresh = 0)

  # New model
  params_updated <- .extract_parameters(model_updated, index = index, ...)

  # Compute index
  sensitivity <- abs(as.matrix(params_updated[-1]) - as.matrix(params[-1])) / abs(as.matrix(params[-1]))

  # Clean up
  sensitivity <- as.data.frame(sensitivity)
  names(sensitivity) <- paste0("Sensitivity_", names(params_updated)[-1])
  sensitivity <- cbind(params_updated[1], sensitivity)
  row.names(sensitivity) <- NULL
  sensitivity
}


#' @rdname sensitivity_to_prior
#' @export
sensitivity_to_prior.brmsfit <- function(model, index = "Median", magnitude = 10, ...) {
  insight::check_if_installed("brms")

  if (insight::is_multivariate(model)) {
    insight::format_error("Multivariate models are not supported yet.")
  }

  # Priors of the slopes (intercepts and thresholds of ordinal models are not shifted)
  priors <- insight::get_priors(model, effects = "fixed", component = "conditional")
  priors <- priors[!grepl("^b_Intercept(\\[\\d+\\])?$", priors$Parameter), ]
  priors <- priors[!duplicated(priors$Parameter), ]
  if (!nrow(priors)) {
    insight::format_error("`sensitivity_to_prior()` requires a model with at least one slope.")
  }

  # Only regular coefficients of class "b" get a new prior, because brms
  # silently ignores priors for other parameters when the model is updated
  slope_info <- .brms_slope_info(model)
  coefs <- sub("^b_", "", priors$Parameter)
  not_slope <- !startsWith(priors$Parameter, "b_") | !coefs %in% slope_info$coef[nzchar(slope_info$coef)]
  if (any(not_slope)) {
    insight::format_error(
      paste0(
        "`sensitivity_to_prior()` supports only regular slopes for brms models. ",
        "These parameters are not supported: ",
        toString(priors$Parameter[not_slope]),
        "."
      )
    )
  }

  # A shifted prior could fall outside the bounds of a bounded prior
  bounded <- vapply(coefs, function(coef) {
    rows <- slope_info[slope_info$coef %in% c(coef, ""), ]
    any(nzchar(c(rows$lb, rows$ub)), na.rm = TRUE)
  }, logical(1))
  if (any(bounded)) {
    insight::format_error(
      paste0(
        "`sensitivity_to_prior()` does not support bounded priors (`lb` or `ub`) on slopes. ",
        "These slopes have bounded priors: ",
        toString(priors$Parameter[bounded]),
        "."
      )
    )
  }

  priors <- .brms_cauchy_priors(slope_info, priors)

  # Only location-scale priors can be moved to an antagonistic location
  unsupported <- !priors$Distribution %in% c("normal", "student_t", "cauchy") |
    is.na(priors$Location) | is.na(priors$Scale)
  if (any(unsupported)) {
    insight::format_error(
      paste0(
        "`sensitivity_to_prior()` requires normal, student_t, or cauchy priors on all slopes. ",
        "These slopes have flat or other priors: ",
        toString(priors$Parameter[unsupported]),
        "."
      ),
      "Use `brms::set_prior()` to specify one of these priors and refit the model."
    )
  }

  # Original
  params <- .extract_parameters(model, index = index, ...)
  missing_slopes <- setdiff(priors$Parameter, params$Parameter)
  if (length(missing_slopes)) {
    insight::format_error(paste0("Could not summarize the posterior of these slopes: ", toString(missing_slopes), "."))
  }
  params <- params[params$Parameter %in% priors$Parameter, ]
  priors <- priors[match(params$Parameter, priors$Parameter), ]

  # Priors
  new_priors <- .prior_new_location(
    prior = list(location = priors$Location, scale = priors$Scale),
    sign = sign(params$Median),
    magnitude = magnitude
  )
  new_priors <- .brms_priors(
    distribution = priors$Distribution,
    location = new_priors$location,
    scale = new_priors$scale,
    df = priors$df,
    coef = sub("^b_", "", priors$Parameter)
  )
  model_updated <- .refit_brms(model, new_priors)
  .brms_check_refit_priors(model_updated, new_priors)

  # New model
  params_updated <- .extract_parameters(model_updated, index = index, ...)
  params_updated <- params_updated[match(params$Parameter, params_updated$Parameter), ]

  # Compute index
  sensitivity <- abs(as.matrix(params_updated[-1]) - as.matrix(params[-1])) / abs(as.matrix(params[-1]))

  # Clean up
  sensitivity <- as.data.frame(sensitivity)
  names(sensitivity) <- paste0("Sensitivity_", names(params_updated)[-1])
  sensitivity <- cbind(params_updated[1], sensitivity)
  row.names(sensitivity) <- NULL
  sensitivity
}


#' @export
sensitivity_to_prior.default <- function(model, ...) {
  insight::format_error(sprintf("Models of class '%s' are not yet supported.", class(model)[1]))
}


#' @keywords internal
.extract_parameters <- function(model, index = "Median", ...) {
  # Handle BF
  test <- c("pd", "rope", "p_map")
  if (any(c("bf", "bayesfactor", "bayes_factor") %in% index)) {
    test <- c(test, "bf")
  }

  params <- suppressMessages(describe_posterior(
    model,
    centrality = "all",
    dispersion = TRUE,
    test = test,
    ...
  ))

  params <- params[params$Parameter != "(Intercept)", ]
  params[unique(c("Parameter", "Median", index))]
}


#' Set a new location for a prior
#' @keywords internal
.prior_new_location <- function(prior, sign, magnitude = 10) {
  prior$location <- -1 * sign * magnitude * prior$scale
  prior
}


#' Priors of class "b" that belong to the main formula of a brms model
#' @keywords internal
.brms_slope_info <- function(model) {
  prior_info <- as.data.frame(brms::prior_summary(model))
  prior_info[prior_info$class == "b" & !nzchar(prior_info$dpar) & !nzchar(prior_info$nlpar), ]
}


#' Make sure that brms used the shifted priors when refitting
#' @keywords internal
.brms_check_refit_priors <- function(model, new_priors) {
  new_priors <- as.data.frame(new_priors)
  slope_info <- .brms_slope_info(model)
  used <- slope_info$prior[match(new_priors$coef, slope_info$coef)]
  not_used <- is.na(used) | used != new_priors$prior
  if (any(not_used)) {
    insight::format_error(
      paste0(
        "brms did not use the shifted priors for these slopes: ",
        toString(new_priors$coef[not_used]),
        "."
      )
    )
  }
  invisible(NULL)
}


#' Add location and scale of cauchy slope priors, which insight does not parse
#' @keywords internal
.brms_cauchy_priors <- function(prior_info, priors) {
  is_cauchy <- priors$Distribution == "cauchy"
  if (!any(is_cauchy)) {
    return(priors)
  }
  for (i in which(is_cauchy)) {
    prior_string <- prior_info$prior[prior_info$coef == sub("^b_", "", priors$Parameter[i])]
    # a slope without its own prior uses the class-level prior
    if (!length(prior_string) || !nzchar(prior_string[1])) {
      prior_string <- prior_info$prior[!nzchar(prior_info$coef)]
    }
    values <- regmatches(prior_string[1], regexec("^cauchy\\((.*),(.*)\\)$", prior_string[1]))[[1]]
    if (length(values) == 3) {
      priors$Location[i] <- suppressWarnings(as.numeric(values[2]))
      priors$Scale[i] <- suppressWarnings(as.numeric(values[3]))
    }
  }
  priors
}


#' Build one brms prior per slope
#' @keywords internal
.brms_priors <- function(distribution, location, scale, df, coef) {
  values <- function(x) sprintf("%.15g", x)
  prior_strings <- ifelse(
    distribution == "student_t",
    sprintf("student_t(%s, %s, %s)", values(df), values(location), values(scale)),
    sprintf("%s(%s, %s)", distribution, values(location), values(scale))
  )
  do.call(c, lapply(seq_along(coef), function(i) {
    brms::set_prior(prior_strings[i], class = "b", coef = coef[i])
  }))
}


#' Refit a brms model with new priors
#' @keywords internal
.refit_brms <- function(model, prior) {
  withCallingHandlers(
    suppressMessages(stats::update(model, prior = prior, refresh = 0)),
    warning = function(w) {
      # the original class-level prior is kept but no longer used
      if (grepl("will not be used in the model", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Extract and Returns the priors formatted for rstanarm
#' @keywords internal
.extract_priors_rstanarm <- function(model, ...) {
  priors <- rstanarm::prior_summary(model)

  # Deal with adjusted scale
  if (!is.null(priors$prior$adjusted_scale)) {
    priors$prior$scale <- priors$prior$adjusted_scale
    priors$prior$adjusted_scale <- NULL
  }
  priors$prior$autoscale <- FALSE


  priors
}
