#' @title Summary of Bayesian multivariate-response mediation-models
#' @name mediation
#'
#' @description `mediation()` is a short summary for multivariate-response
#'   mediation-models, i.e. this function computes average direct and average
#'   causal mediation effects of multivariate response models.
#'
#' @param model A `brmsfit` or `stanmvreg` object.
#' @param treatment Character, name of the treatment variable (or direct effect)
#'   in a (multivariate response) mediator-model. If missing, `mediation()`
#'   tries to find the treatment variable automatically, however, this may fail.
#' @param mediator Character, name of the mediator variable in a (multivariate
#'   response) mediator-model. If missing, `mediation()` tries to find the
#'   treatment variable automatically, however, this may fail.
#' @param response A named character vector, indicating the names of the response
#'   variables to be used for the mediation analysis. Usually can be `NULL`,
#'   in which case these variables are retrieved automatically. If not `NULL`,
#'   names should match the names of the model formulas,
#'   `names(insight::find_response(model, combine = TRUE))`. This can be
#'   useful if, for instance, the mediator variable used as predictor has a different
#'   name from the mediator variable used as response. This might occur when the
#'   mediator is transformed in one model, but used "as is" as response variable
#'   in the other model. Example: The mediator `m` is used as response variable,
#'   but the centered version `m_center` is used as mediator variable. The
#'   second response variable (for the treatment model, with the mediator as
#'   additional predictor), `y`, is not transformed. Then we could use
#'   `response` like this: `mediation(model, response = c(m = "m_center", y = "y"))`.
#' @param ... Not used.
#' @inheritParams ci
#' @inheritParams describe_posterior
#'
#' @return A data frame with direct, indirect, mediator and
#'   total effect of a multivariate-response mediation-model, as well as the
#'   proportion mediated. The effect sizes are median values of the posterior
#'   samples (use `centrality` for other centrality indices).
#'
#' @details `mediation()` returns a data frame with information on the
#'       *direct effect* (mean value of posterior samples from `treatment`
#'       of the outcome model), *mediator effect* (mean value of posterior
#'       samples from `mediator` of the outcome model), *indirect effect*
#'       (mean value of the multiplication of the posterior samples from
#'       `mediator` of the outcome model and the posterior samples from
#'       `treatment` of the mediation model) and the total effect (mean
#'       value of sums of posterior samples used for the direct and indirect
#'       effect). The *proportion mediated* is the indirect effect divided
#'       by the total effect.
#'       \cr \cr
#'       For all values, the `89%` credible intervals are calculated by default.
#'       Use `ci` to calculate a different interval.
#'       \cr \cr
#'       The arguments `treatment` and `mediator` do not necessarily
#'       need to be specified. If missing, `mediation()` tries to find the
#'       treatment and mediator variable automatically. If this does not work,
#'       specify these variables.
#'       \cr \cr
#'       The direct effect is also called *average direct effect* (ADE),
#'       the indirect effect is also called *average causal mediation effects*
#'       (ACME). See also \cite{Tingley et al. 2014} and \cite{Imai et al. 2010}.
#'
#' @note There is an `as.data.frame()` method that returns the posterior
#'   samples of the effects, which can be used for further processing in the
#'   different \pkg{bayestestR} package.
#'
#' @references
#' \itemize{
#' \item Imai, K., Keele, L. and Tingley, D. (2010) A General Approach to Causal
#' Mediation Analysis, Psychological Methods, Vol. 15, No. 4 (December), pp.
#' 309-334.
#'
#' \item Tingley, D., Yamamoto, T., Hirose, K., Imai, K. and Keele, L. (2014).
#' mediation: R package for Causal Mediation Analysis, Journal of Statistical
#' Software, Vol. 59, No. 5, pp. 1-38.
#' }
#'
#' @seealso The \pkg{mediation} package for a causal mediation analysis in
#'   the frequentist framework.
#'
#' @examples
#' \dontrun{
#' library(mediation)
#' library(brms)
#' library(rstanarm)
#'
#' # load sample data
#' data(jobs)
#' set.seed(123)
#'
#' # linear models, for mediation analysis
#' b1 <- lm(job_seek ~ treat + econ_hard + sex + age, data = jobs)
#' b2 <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data = jobs)
#' # mediation analysis, for comparison with Stan models
#' m1 <- mediate(b1, b2, sims = 1000, treat = "treat", mediator = "job_seek")
#'
#' # Fit Bayesian mediation model in brms
#' f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
#' f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)
#' m2 <- brm(f1 + f2 + set_rescor(FALSE), data = jobs, cores = 4, refresh = 0)
#'
#' # Fit Bayesian mediation model in rstanarm
#' m3 <- stan_mvmer(
#'   list(
#'     job_seek ~ treat + econ_hard + sex + age + (1 | occp),
#'     depress2 ~ treat + job_seek + econ_hard + sex + age + (1 | occp)
#'   ),
#'   data = jobs,
#'   cores = 4,
#'   refresh = 0
#' )
#'
#' summary(m1)
#' mediation(m2, centrality = "mean", ci = 0.95)
#' mediation(m3, centrality = "mean", ci = 0.95)
#' }
#' @export
mediation <- function(model, ...) {
  UseMethod("mediation")
}


#' @rdname mediation
#' @export
mediation.brmsfit <- function(model,
                              treatment,
                              mediator,
                              response = NULL,
                              centrality = "median",
                              ci = 0.95,
                              method = "ETI",
                              ...) {
  .mediation(
    model = model,
    treatment = treatment,
    mediator = mediator,
    response = response,
    centrality = centrality,
    ci = ci,
    method = method,
    pattern = "b_%s_%s",
    ...
  )
}


#' @rdname mediation
#' @export
mediation.stanmvreg <- function(model, treatment, mediator, response = NULL, centrality = "median", ci = 0.95, method = "ETI", ...) {
  .mediation(
    model = model,
    treatment = treatment,
    mediator = mediator,
    response = response,
    centrality = centrality,
    ci = ci,
    method = method,
    pattern = "%s|%s",
    ...
  )
}





# workhorse ---------------------------------


.mediation <- function(model,
                       treatment,
                       mediator,
                       response = NULL,
                       centrality = "median",
                       ci = 0.95,
                       method = "ETI",
                       pattern = "b_%s_%s",
                       ...) {
  # only one HDI interval
  if (length(ci) > 1) ci <- ci[1]

  # check for binary response. In this case, user should rescale variables
  modelinfo <- insight::model_info(model)
  if (any(sapply(modelinfo, function(i) i$is_binomial, simplify = TRUE))) {
    insight::format_alert("One of moderator or outcome is binary, so direct and indirect effects may be on different scales. Consider rescaling model predictors, e.g. with `effectsize::standardize()`.")
  }

  # model responses
  if (is.null(response)) {
    response <- insight::find_response(model, combine = TRUE)
  }
  fix_mediator <- FALSE

  # find mediator, if not specified
  if (missing(mediator)) {
    predictors <- insight::find_predictors(model, flatten = TRUE)
    mediator <- predictors[predictors %in% response]
    fix_mediator <- TRUE
  }

  # find treatment, if not specified
  if (missing(treatment)) {
    predictors <- lapply(
      insight::find_predictors(model),
      function(.f) .f$conditional
    )

    treatment <- predictors[[1]][predictors[[1]] %in% predictors[[2]]][1]
    treatment <- .fix_factor_name(model, treatment)
  }


  mediator.model <- which(response == mediator)
  treatment.model <- which(response != mediator)

  if (fix_mediator) mediator <- .fix_factor_name(model, mediator)

  if (inherits(model, "brmsfit")) {
    response_name <- names(response)
  } else {
    response_name <- unname(response)
  }

  # brms removes underscores from variable names when naming estimates
  # so we need to fix variable names here

  response <- names(response)


  # Direct effect: coef(treatment) from model_y_treatment
  coef_treatment <- sprintf(pattern, response[treatment.model], treatment)
  effect_direct <- insight::get_parameters(model)[[coef_treatment]]

  # Mediator effect: coef(mediator) from model_y_treatment
  coef_mediator <- sprintf(pattern, response[treatment.model], mediator)
  effect_mediator <- insight::get_parameters(model)[[coef_mediator]]

  # Indirect effect: coef(treament) from model_m_mediator * coef(mediator) from model_y_treatment
  coef_indirect <- sprintf(pattern, response[mediator.model], treatment)
  tmp.indirect <- insight::get_parameters(model)[c(coef_indirect, coef_mediator)]
  effect_indirect <- tmp.indirect[[coef_indirect]] * tmp.indirect[[coef_mediator]]

  # Total effect
  effect_total <- effect_indirect + effect_direct

  # proportion mediated: indirect effect / total effect
  proportion_mediated <- as.numeric(point_estimate(effect_indirect, centrality = centrality)) / as.numeric(point_estimate(effect_total, centrality = centrality))
  hdi_eff <- ci(effect_indirect / effect_total, ci = ci, method = method)
  prop_mediated_se <- (hdi_eff$CI_high - hdi_eff$CI_low) / 2
  prop_mediated_ci <- proportion_mediated + c(-1, 1) * prop_mediated_se

  res <- cbind(
    data.frame(
      Effect = c("Direct Effect (ADE)", "Indirect Effect (ACME)", "Mediator Effect", "Total Effect", "Proportion Mediated"),
      Estimate = c(
        as.numeric(point_estimate(effect_direct, centrality = centrality)),
        as.numeric(point_estimate(effect_indirect, centrality = centrality)),
        as.numeric(point_estimate(effect_mediator, centrality = centrality)),
        as.numeric(point_estimate(effect_total, centrality = centrality)),
        proportion_mediated
      ),
      stringsAsFactors = FALSE
    ),
    as.data.frame(rbind(
      ci(effect_direct, ci = ci, method = method)[, -1],
      ci(effect_indirect, ci = ci, method = method)[, -1],
      ci(effect_mediator, ci = ci, method = method)[, -1],
      ci(effect_total, ci = ci, method = method)[, -1],
      prop_mediated_ci
    ))
  )

  colnames(res) <- c("Effect", "Estimate", "CI_low", "CI_high")
  samples <- data.frame(
    effect_direct,
    effect_indirect,
    effect_mediator,
    effect_total,
    proportion_mediated = effect_indirect / effect_total
  )

  attr(res, "ci") <- ci
  attr(res, "ci_method") <- method
  attr(res, "treatment") <- treatment
  attr(res, "mediator") <- mediator
  attr(res, "response") <- response_name[treatment.model]
  attr(res, "data") <- samples

  class(res) <- c("bayestestR_mediation", "see_bayestestR_mediation", class(res))
  res
}


# methods ---------------------

#' @export
as.data.frame.bayestestR_mediation <- function(x, ...) {
  attributes(x)$data
}


# helper ---------------------------------

.fix_factor_name <- function(model, variable) {
  # check for categorical. if user has not specified a treatment variable
  # and this variable is categorical, the posterior samples contain the
  # samples from each category of the treatment variable - so we need to
  # fix the variable name

  mf <- insight::get_data(model)
  if (variable %in% colnames(mf)) {
    check_fac <- mf[[variable]]
    if (is.factor(check_fac)) {
      variable <- sprintf("%s%s", variable, levels(check_fac)[nlevels(check_fac)])
    } else if (is.logical(check_fac)) {
      variable <- sprintf("%sTRUE", variable)
    }
  }

  variable
}



# S3 ---------------------------------

#' @export
print.bayestestR_mediation <- function(x, digits = 3, ...) {
  attr(x, "data") <- NULL
  insight::print_color("# Causal Mediation Analysis for Stan Model\n\n", "blue")

  cat(sprintf(
    "  Treatment: %s\n  Mediator : %s\n  Response : %s\n\n",
    attr(x, "treatment", exact = TRUE),
    attr(x, "mediator", exact = TRUE),
    attr(x, "response", exact = TRUE)
  ))

  prop_mediated <- prop_mediated_ori <- x[nrow(x), ]
  x <- x[-nrow(x), ]

  x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = "auto", missing = "NA")
  x <- datawizard::data_remove(x, c("CI_low", "CI_high"), verbose = FALSE)
  colnames(x)[ncol(x)] <- sprintf("%.5g%% %s", 100 * attributes(x)$ci, attributes(x)$ci_method)

  # remove class, to avoid conflicts with "as.data.frame.bayestestR_mediation()"
  class(x) <- "data.frame"
  cat(insight::export_table(x, digits = digits))
  cat("\n")

  prop_mediated[] <- lapply(prop_mediated, insight::format_value, as_percent = TRUE)
  insight::print_color(
    sprintf(
      "Proportion mediated: %s [%s, %s]\n",
      prop_mediated$Estimate, prop_mediated$CI_low, prop_mediated$CI_high
    ),
    "red"
  )

  if (any(prop_mediated_ori$Estimate < 0)) {
    insight::format_alert("\nDirect and indirect effects have opposite directions. The proportion mediated is not meaningful.")
  }
}



#' @export
plot.bayestestR_mediation <- function(x, ...) {
  insight::check_if_installed("see", "to plot results from mediation analysis")
  NextMethod()
}
