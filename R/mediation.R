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
#'   response) mediator-model. For *brms* models, this can be several names
#'   (see 'Details'). If missing, `mediation()` tries to find the mediator
#'   variable automatically, however, this may fail.
#' @param response A named character vector, indicating the names of the response
#'   variables to be used for the mediation analysis. Usually can be `NULL`,
#'   in which case these variables are retrieved automatically. If not `NULL`,
#'   names must match the names of the model formulas. For *brms* models,
#'   these are `names(insight::find_formula(model))`, one name per formula,
#'   and the values are the left side of each formula without addition terms
#'   such as `cens()`. For other models, they are
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
#'  *direct effect* (mean value of posterior samples from `treatment`
#'  of the outcome model), *mediator effect* (mean value of posterior
#'  samples from `mediator` of the outcome model), *indirect effect*
#'  (mean value of the multiplication of the posterior samples from
#'  `mediator` of the outcome model and the posterior samples from
#'  `treatment` of the mediation model) and the total effect (mean
#'  value of sums of posterior samples used for the direct and indirect
#'  effect). The *proportion mediated* is the indirect effect divided
#'  by the total effect.
#'
#'  For all values, the 95% credible intervals are calculated by default.
#'  Use `ci` to calculate a different interval.
#'
#'  The arguments `treatment` and `mediator` do not necessarily
#'  need to be specified. If missing, `mediation()` tries to find the
#'  treatment and mediator variable automatically. If this does not work,
#'  specify these variables.
#'
#'  The direct effect is also called *average direct effect* (ADE),
#'  the indirect effect is also called *average causal mediation effects*
#'  (ACME). See also _Tingley et al. 2014_ and _Imai et al. 2010_.
#'
#'  For *brms* models, there can be more than one mediator, in parallel or in
#'  series (where one mediator is a predictor of another). Each mediator must
#'  also be a predictor in the outcome model. The outcome is the
#'  one response that no other model formula uses as a predictor. If `mediator`
#'  is missing, the mediators are the other responses on a path from the
#'  treatment to the outcome. They are in the order of the model formulas. If
#'  `treatment` is missing, the treatment is the first suitable predictor of
#'  the outcome model. A suitable predictor is not a response, and it is a
#'  predictor in a mediator model. A path goes from the treatment through one or
#'  more mediators to the outcome. Its indirect effect is the product of the
#'  coefficients along the path, for each posterior sample. With several
#'  mediators, the rows are:
#'
#'  - the direct effect,
#'  - one indirect effect for each path, such as
#'    `Indirect Effect (ACME): m1 -> m2`,
#'  - the total indirect effect,
#'  - one mediator effect for each mediator, which is its coefficient in the
#'    outcome model,
#'  - the total effect,
#'  - the proportion mediated. Its estimate is the point estimate of the total
#'    indirect effect divided by the point estimate of the total effect.
#'
#'  If `mediator` names only some of the mediators, `mediation()` uses only the
#'  paths whose mediators are all among these. A path through any other
#'  mediator is left out. The total effect is then the direct effect plus the
#'  indirect effects of the paths that are used.
#'
#' @note There is an `as.data.frame()` method that returns the posterior
#'   samples of the effects, which can be used for further processing in the
#'   **bayestestR** package. With several mediators, each column has the name
#'   of its row in the summary.
#'
#' @references
#'
#' - Imai, K., Keele, L. and Tingley, D. (2010) A General Approach to Causal
#' Mediation Analysis, Psychological Methods, Vol. 15, No. 4 (December), pp.
#' 309-334.
#'
#' - Tingley, D., Yamamoto, T., Hirose, K., Imai, K. and Keele, L. (2014).
#' mediation: R package for Causal Mediation Analysis, Journal of Statistical
#' Software, Vol. 59, No. 5, pp. 1-38.
#'
#' @seealso The \pkg{mediation} package for a causal mediation analysis in
#'   the frequentist framework.
#'
#' @examplesIf require("mediation") && require("brms") && require("rstanarm")
#' \donttest{
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
#' m2 <- brm(f1 + f2 + set_rescor(FALSE), data = jobs, refresh = 0)
#'
#' # Fit Bayesian mediation model in rstanarm
#' m3 <- suppressWarnings(stan_mvmer(
#'   list(
#'     job_seek ~ treat + econ_hard + sex + age + (1 | occp),
#'     depress2 ~ treat + job_seek + econ_hard + sex + age + (1 | occp)
#'   ),
#'   data = jobs,
#'   refresh = 0
#' ))
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
mediation.brmsfit <- function(
  model,
  treatment,
  mediator,
  response = NULL,
  centrality = "median",
  ci = 0.95,
  method = "ETI",
  ...
) {
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


#' @export
mediation.stanmvreg <- function(
  model,
  treatment,
  mediator,
  response = NULL,
  centrality = "median",
  ci = 0.95,
  method = "ETI",
  ...
) {
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

.mediation <- function(
  model,
  treatment,
  mediator,
  response = NULL,
  centrality = "median",
  ci = 0.95,
  method = "ETI",
  pattern = "b_%s_%s",
  ...
) {
  # only one HDI interval
  if (length(ci) > 1) {
    ci <- ci[1]
  }

  # check for binary response. In this case, user should rescale variables
  modelinfo <- insight::model_info(model, verbose = FALSE)
  if (any(sapply(modelinfo, function(i) i$is_binomial, simplify = TRUE))) {
    insight::format_alert(
      "One of moderator or outcome is binary, so direct and indirect effects may be on different scales. Consider rescaling model predictors, e.g. with `effectsize::standardize()`."
    )
  }

  # model responses
  if (is.null(response)) {
    response <- .mediation_response(model)
  }
  params <- insight::get_parameters(model)

  if (missing(treatment)) {
    treatment <- NULL
  }
  if (missing(mediator)) {
    mediator <- NULL
  }
  if (!is.null(treatment) && length(treatment) != 1) {
    insight::format_error("`treatment` must be the name of one variable.")
  }
  if (!is.null(mediator) && !length(mediator)) {
    insight::format_error("`mediator` must name at least one variable.")
  }
  if (length(centrality) != 1 || identical(tolower(centrality), "all")) {
    insight::format_error(
      "`mediation()` needs one centrality index, such as `centrality = \"median\"`."
    )
  }

  # treatment, mediators, outcome, and the paths from the treatment through
  # the mediators to the outcome
  if (inherits(model, "brmsfit")) {
    info <- .mediation_paths_brms(model, treatment, mediator, response, params)
  } else {
    info <- .mediation_paths_default(model, treatment, mediator, response)
  }

  # the direct effect, each mediator effect, and each link of each path need
  # a coefficient
  coef_missing <- setdiff(
    unique(c(
      sprintf(pattern, info$outcome, info$treatment),
      sprintf(pattern, info$outcome, info$mediator),
      unlist(lapply(info$paths, function(p) {
        sprintf(pattern, c(info$equation[p], info$outcome), c(info$treatment, p))
      }))
    )),
    colnames(params)
  )
  if (length(coef_missing)) {
    insight::format_error(sprintf(
      "Could not find %s %s in the model. Please check the `treatment` and `mediator` arguments.",
      if (length(coef_missing) > 1) "the coefficients" else "the coefficient",
      toString(paste0("`", coef_missing, "`"))
    ))
  }

  .draws <- function(equation, variable) {
    params[[sprintf(pattern, equation, variable)]]
  }

  # Direct effect: coef(treatment) from the outcome model
  effect_direct <- .draws(info$outcome, info$treatment)

  # Mediator effects: coef(mediator) from the outcome model
  effect_mediator <- lapply(info$mediator, function(m) .draws(info$outcome, m))

  # Indirect effect of a path: the product of the coefficients along the path,
  # e.g. coef(treatment) from the mediator model * coef(mediator) from the
  # outcome model
  effect_paths <- lapply(info$paths, function(p) {
    Reduce(`*`, Map(.draws, c(info$equation[p], info$outcome), c(info$treatment, p)))
  })
  effect_indirect <- Reduce(`+`, effect_paths)

  # Total effect
  effect_total <- effect_indirect + effect_direct

  if (length(info$mediator) == 1) {
    effects <- list(
      effect_direct = effect_direct,
      effect_indirect = effect_indirect,
      effect_mediator = effect_mediator[[1]],
      effect_total = effect_total
    )
    labels <- c(
      "Direct Effect (ADE)",
      "Indirect Effect (ACME)",
      "Mediator Effect",
      "Total Effect"
    )
    label_proportion <- "proportion_mediated"
  } else {
    labels <- c(
      "Direct Effect (ADE)",
      paste0(
        "Indirect Effect (ACME): ",
        vapply(info$paths, paste, character(1), collapse = " -> ")
      ),
      "Total Indirect Effect",
      paste0("Mediator Effect: ", info$mediator),
      "Total Effect"
    )
    effects <- c(
      list(effect_direct),
      effect_paths,
      list(effect_indirect),
      effect_mediator,
      list(effect_total)
    )
    names(effects) <- labels
    label_proportion <- "Proportion Mediated"
  }

  # proportion mediated: indirect effect / total effect
  proportion_mediated <- as.numeric(point_estimate(
    effect_indirect,
    centrality = centrality
  )) /
    as.numeric(point_estimate(effect_total, centrality = centrality))
  hdi_eff <- ci(effect_indirect / effect_total, ci = ci, method = method)
  prop_mediated_se <- (hdi_eff$CI_high - hdi_eff$CI_low) / 2
  prop_mediated_ci <- proportion_mediated + c(-1, 1) * prop_mediated_se

  res <- cbind(
    data.frame(
      Effect = c(labels, "Proportion Mediated"),
      Estimate = c(
        vapply(
          effects,
          function(e) as.numeric(point_estimate(e, centrality = centrality)),
          numeric(1),
          USE.NAMES = FALSE
        ),
        proportion_mediated
      ),
      stringsAsFactors = FALSE
    ),
    as.data.frame(do.call(
      rbind,
      c(
        lapply(effects, function(e) ci(e, ci = ci, method = method)[, -1]),
        list(prop_mediated_ci)
      )
    ))
  )

  colnames(res) <- c("Effect", "Estimate", "CI_low", "CI_high")
  rownames(res) <- NULL
  samples <- data.frame(effects, check.names = FALSE)
  samples[[label_proportion]] <- effect_indirect / effect_total

  attr(res, "ci") <- ci
  attr(res, "ci_method") <- method
  attr(res, "treatment") <- info$treatment
  attr(res, "mediator") <- info$mediator
  attr(res, "response") <- info$response
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

# Returns a list with the treatment and the mediators, as they are named in
# the coefficients, the model equations of the outcome and of each mediator,
# the response attribute, and the paths, each a vector of mediator names from
# the treatment to the outcome.
.mediation_paths_default <- function(model, treatment, mediator, response) {
  if (length(response) != 2) {
    insight::format_error(
      "For this model class, `mediation()` supports two model formulas only, one for the mediator and one for the outcome."
    )
  }
  fix_mediator <- FALSE

  # find mediator, if not specified
  if (is.null(mediator)) {
    predictors <- insight::find_predictors(model, flatten = TRUE)
    mediator <- predictors[predictors %in% response]
    fix_mediator <- TRUE
  }

  # find treatment, if not specified
  if (is.null(treatment)) {
    predictors <- lapply(
      insight::find_predictors(model),
      function(.f) .f$conditional
    )

    treatment <- predictors[[1]][predictors[[1]] %in% predictors[[2]]][1]
    treatment <- .fix_factor_name(model, treatment)
  }

  if (length(mediator) != 1) {
    insight::format_error(
      "For this model class, `mediation()` needs exactly one mediator. Give one name in `mediator`, or, if `mediator` is missing, one response must be a predictor in the other model formula."
    )
  }

  mediator.model <- which(response == mediator)
  treatment.model <- which(response != mediator)
  if (length(mediator.model) != 1) {
    insight::format_error(sprintf(
      "`%s` is not a mediator. The mediator is the response of one model formula: %s.",
      mediator,
      toString(paste0("`", response, "`"))
    ))
  }

  if (fix_mediator) {
    mediator <- .fix_factor_name(model, mediator)
  }

  equation <- names(response)
  list(
    treatment = treatment,
    mediator = mediator,
    equation = stats::setNames(equation[mediator.model], mediator),
    outcome = equation[treatment.model],
    response = unname(response)[treatment.model],
    paths = list(mediator)
  )
}


.mediation_paths_brms <- function(
  model,
  treatment,
  mediator,
  response,
  params,
  predictors = NULL
) {
  equation <- names(response)
  # predictors of each model formula, in the order of `response`
  if (is.null(predictors)) {
    all_predictors <- insight::find_predictors(model)
    predictors <- lapply(equation, function(i) all_predictors[[i]]$conditional)
  }

  # a variable is in a model formula if it is a predictor there, or if the
  # formula has a coefficient named after it (e.g. `treatment = "xb"` for
  # level "b" of a factor "x")
  .in_formula <- function(variable, i) {
    variable %in% predictors[[i]] ||
      sprintf("b_%s_%s", equation[i], variable) %in% colnames(params)
  }
  .in_any <- function(variable, models) {
    any(vapply(models, .in_formula, logical(1), variable = variable))
  }

  # the outcome is the one response that no other model formula uses as a
  # predictor
  is_outcome <- vapply(
    seq_along(response),
    function(i) !.in_any(response[[i]], seq_along(response)[-i]),
    logical(1)
  )
  if (sum(is_outcome) != 1) {
    insight::format_error(sprintf(
      "Could not find the outcome. The outcome is the one response that no other model formula uses as a predictor, and this model has %s. If a mediator is transformed in its own model formula, such as `log(m)`, use `response` to give its name as a predictor.",
      if (any(is_outcome)) {
        sprintf(
          "%i such responses: %s",
          sum(is_outcome),
          toString(paste0("`", response[is_outcome], "`"))
        )
      } else {
        "none"
      }
    ))
  }
  outcome <- which(is_outcome)
  others <- seq_along(response)[-outcome]

  if (!is.null(mediator)) {
    mediator <- unique(mediator)
    not_mediator <- setdiff(mediator, response[others])
    if (length(not_mediator)) {
      insight::format_error(paste(
        paste0("`", not_mediator, "` is not a mediator.", collapse = " "),
        sprintf(
          "Mediators are the responses of the model formulas other than the outcome `%s`: %s.",
          response[[outcome]],
          toString(paste0("`", response[others], "`"))
        )
      ))
    }
  }

  # find treatment, if not specified: the first predictor of the outcome that
  # is not a response and is a predictor of a mediator
  fix_treatment <- is.null(treatment)
  if (fix_treatment) {
    mediator_models <- others
    if (!is.null(mediator)) {
      mediator_models <- others[response[others] %in% mediator]
    }
    treatment <- setdiff(predictors[[outcome]], response)
    treatment <- treatment[vapply(
      treatment,
      .in_any,
      logical(1),
      models = mediator_models
    )]
    if (!length(treatment)) {
      insight::format_error(sprintf(
        "Could not find the treatment. The treatment is a predictor of the outcome `%s` that is also a predictor of a mediator. Please specify `treatment`.",
        response[[outcome]]
      ))
    }
    treatment <- treatment[1]
  }

  # find mediators, if not specified: the responses on a path from the
  # treatment to the outcome, in the order of the model formulas
  fix_mediator <- is.null(mediator)
  if (fix_mediator) {
    .grow <- function(models, linked) {
      repeat {
        grown <- union(models, others[vapply(others, linked, logical(1), models = models)])
        if (length(grown) == length(models)) {
          return(models)
        }
        models <- grown
      }
    }
    from_treatment <- .grow(
      others[vapply(others, .in_formula, logical(1), variable = treatment)],
      function(i, models) any(vapply(response[models], .in_formula, logical(1), i = i))
    )
    to_outcome <- .grow(
      others[vapply(response[others], .in_formula, logical(1), i = outcome)],
      function(i, models) .in_any(response[[i]], models)
    )
    mediator <- unname(response[others[others %in% intersect(from_treatment, to_outcome)]])
    if (!length(mediator)) {
      insight::format_error(sprintf(
        "Could not find a mediator. A mediator is the response of a model formula on a path from the treatment `%s` to the outcome `%s`. Please specify `mediator`.",
        treatment,
        response[[outcome]]
      ))
    }
  }
  mediator_models <- match(mediator, response)

  # brms names monotonic effects "bsp_<formula>_mo<variable>", where
  # <formula> is the formula name (e.g. "mb" for the response "m_b"). The
  # product of coefficients is not defined for them, so stop before the
  # mediator name is changed for factors
  # only the models of the outcome and of the mediators are used
  used_models <- c(outcome, mediator_models)
  for (m in mediator) {
    coef_mo <- sprintf("bsp_%s_mo%s", equation[used_models], m) %in% colnames(params)
    if (any(coef_mo)) {
      insight::format_error(sprintf(
        "The mediator `%s` enters the %s model as a monotonic effect with `mo()`. `mediation()` multiplies coefficients and does not support mediators modeled with `mo()`.",
        m,
        if (coef_mo[1]) {
          "outcome"
        } else {
          sprintf("`%s`", response[[used_models[which(coef_mo)[1]]]])
        }
      ))
    }
  }

  # paths, as indices of `mediator`. A single mediator has one path. For
  # several mediators, a path starts at a mediator whose model has the
  # treatment, goes on to mediators whose models have the previous mediator,
  # and ends at a mediator that is a predictor of the outcome
  if (length(mediator) == 1) {
    paths <- list(1L)
  } else {
    paths <- list()
    .extend <- function(path) {
      last <- path[length(path)]
      if (.in_formula(mediator[last], outcome)) {
        paths[[length(paths) + 1]] <<- path
      }
      for (k in setdiff(seq_along(mediator), path)) {
        if (.in_formula(mediator[last], mediator_models[k])) {
          .extend(c(path, k))
        }
      }
    }
    for (k in seq_along(mediator)) {
      if (.in_formula(treatment, mediator_models[k])) {
        .extend(k)
      }
    }
    if (!length(paths)) {
      insight::format_error(sprintf(
        "Could not find a path from the treatment `%s` through the mediators %s to the outcome `%s`.",
        treatment,
        toString(paste0("`", mediator, "`")),
        response[[outcome]]
      ))
    }
    off_path <- mediator[setdiff(seq_along(mediator), unlist(paths))]
    if (length(off_path)) {
      insight::format_error(sprintf(
        "Could not find a path from the treatment `%s` through %s to the outcome `%s`.",
        treatment,
        if (length(off_path) > 1) {
          paste("any of", toString(paste0("`", off_path, "`")))
        } else {
          paste0("`", off_path, "`")
        },
        response[[outcome]]
      ))
    }
    # shorter paths first, then in the order of `mediator`
    path_order <- vapply(
      paths,
      function(p) paste(sprintf("%05i", c(length(p), p)), collapse = " "),
      character(1)
    )
    paths <- paths[order(path_order, method = "radix")]
  }

  if (fix_treatment || fix_mediator) {
    mf <- insight::get_data(model)
  }
  if (fix_treatment) {
    treatment <- .fix_factor_name(model, treatment, mf)
  }
  if (fix_mediator) {
    mediator <- vapply(
      mediator,
      function(m) .fix_factor_name(model, m, mf),
      character(1),
      USE.NAMES = FALSE
    )
  }

  list(
    treatment = treatment,
    mediator = mediator,
    equation = stats::setNames(equation[mediator_models], mediator),
    outcome = equation[outcome],
    response = equation[outcome],
    paths = lapply(paths, function(p) mediator[p])
  )
}


.mediation_response <- function(model) {
  if (!inherits(model, "brmsfit")) {
    return(insight::find_response(model, combine = TRUE))
  }
  # one response per model formula. find_response() also returns the
  # variables of addition terms, like "cens" in "y | cens(cens)". We keep
  # transformations, like "log(m)", so that a transformed mediator is not
  # matched with the untransformed predictor
  vapply(
    insight::find_formula(model, verbose = FALSE),
    function(f) {
      lhs <- f$conditional[[2]]
      if (is.call(lhs) && identical(lhs[[1]], as.name("|"))) {
        lhs <- lhs[[2]]
      }
      insight::safe_deparse(lhs)
    },
    character(1)
  )
}


.fix_factor_name <- function(model, variable, mf = insight::get_data(model)) {
  # check for categorical. if user has not specified a treatment variable
  # and this variable is categorical, the posterior samples contain the
  # samples from each category of the treatment variable - so we need to
  # fix the variable name

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
    toString(attr(x, "mediator", exact = TRUE)),
    attr(x, "response", exact = TRUE)
  ))

  prop_mediated <- prop_mediated_ori <- x[nrow(x), ]
  x <- x[-nrow(x), ]

  x$CI <- insight::format_ci(
    x$CI_low,
    x$CI_high,
    ci = NULL,
    digits = digits,
    width = "auto",
    missing = "NA"
  )
  x <- datawizard::data_remove(x, c("CI_low", "CI_high"), verbose = FALSE)
  colnames(x)[ncol(x)] <- sprintf(
    "%.5g%% %s",
    100 * attributes(x)$ci,
    attributes(x)$ci_method
  )

  # remove class, to avoid conflicts with "as.data.frame.bayestestR_mediation()"
  class(x) <- "data.frame"
  cat(insight::export_table(x, digits = digits))
  cat("\n")

  prop_mediated[] <- lapply(prop_mediated, insight::format_value, as_percent = TRUE)
  insight::print_color(
    sprintf(
      "Proportion mediated: %s [%s, %s]\n",
      prop_mediated$Estimate,
      prop_mediated$CI_low,
      prop_mediated$CI_high
    ),
    "red"
  )

  if (any(prop_mediated_ori$Estimate < 0)) {
    insight::format_alert(
      "\nDirect and indirect effects have opposite directions. The proportion mediated is not meaningful."
    )
  }
}


#' @export
plot.bayestestR_mediation <- function(x, ...) {
  insight::check_if_installed("see", "to plot results from mediation analysis")
  NextMethod()
}
