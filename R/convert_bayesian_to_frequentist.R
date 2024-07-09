#' Convert (refit) a Bayesian model to frequentist
#'
#' Refit Bayesian model as frequentist. Can be useful for comparisons.
#'
#' @param model A Bayesian model.
#' @param data Data used by the model. If `NULL`, will try to extract it
#'   from the model.
#' @param REML For mixed effects, should models be estimated using
#'   restricted maximum likelihood (REML) (`TRUE`, default) or maximum
#'   likelihood (`FALSE`)?
#' @examplesIf require("rstanarm")
#' \donttest{
#' # Rstanarm ----------------------
#' # Simple regressions
#' model <- rstanarm::stan_glm(Sepal.Length ~ Species,
#'   data = iris, chains = 2, refresh = 0
#' )
#' bayesian_as_frequentist(model)
#'
#' model <- rstanarm::stan_glm(vs ~ mpg,
#'   family = "binomial",
#'   data = mtcars, chains = 2, refresh = 0
#' )
#' bayesian_as_frequentist(model)
#'
#' # Mixed models
#' model <- rstanarm::stan_glmer(
#'   Sepal.Length ~ Petal.Length + (1 | Species),
#'   data = iris, chains = 2, refresh = 0
#' )
#' bayesian_as_frequentist(model)
#'
#' model <- rstanarm::stan_glmer(vs ~ mpg + (1 | cyl),
#'   family = "binomial",
#'   data = mtcars, chains = 2, refresh = 0
#' )
#' bayesian_as_frequentist(model)
#' }
#'
#' @export
convert_bayesian_as_frequentist <- function(model, data = NULL, REML = TRUE) {
  if (is.null(data)) {
    data <- insight::get_data(model)
  }

  info <- insight::model_info(model, verbose = FALSE)
  model_formula <- insight::find_formula(model)
  model_family <- insight::get_family(model)

  if (inherits(model_family, "brmsfamily")) {
    insight::check_if_installed("glmmTMB")
    # exception: ordbetareg()
    if ("custom" %in% model_family$family && all(model_family$name == "ord_beta_reg")) {
      model_family <- glmmTMB::ordbeta()
    } else {
      # not all families return proper objects from "get", so we capture
      # some families via switch here...
      model_family <- .safe(switch(
        model_family$family,
        beta = glmmTMB::beta_family(link = model_family$link),
        beta_binomial = glmmTMB::betabinomial(link = model_family$link),
        negbinomial = glmmTMB::nbinom1(link = model_family$link),
        lognormal = glmmTMB::lognormal(link = model_family$link),
        student = glmmTMB::t_family(link = model_family$link),
        get(model_family$family)(link = model_family$link)
      ))
    }
  }

  # if family could not be identified, stop here
  if (is.null(model_family)) {
    insight::format_error("Model could not be automatically converted to frequentist model.")
  }

  # first attempt
  freq <- tryCatch(.convert_bayesian_as_frequentist(
    info = info, formula = model_formula, data = data, family = model_family, REML = REML
  ), error = function(e) e)

  if (inherits(freq, "error")) {
    # try again to extract family, using generic approach
    model_family <- get(model_family$family)(link = model_family$link)
    freq <- .convert_bayesian_as_frequentist(
      info = info, formula = model_formula, data = data, family = model_family, REML = REML
    )
  }

  if (inherits(freq, "error")) {
    insight::format_error("Model could not be automatically converted to frequentist model.")
  }

  freq
}

# internal

.convert_bayesian_as_frequentist <- function(info, formula, data, family, REML = TRUE) {
  # TODO: Check for
  # nonlinear formulas,
  # correlation structures,
  # weights,
  # offset,
  # subset,
  # knots,
  # meta-analysis
  if (info$is_dispersion || info$is_orderedbeta || info$is_beta || info$is_betabinomial || info$is_zero_inflated || info$is_zeroinf || info$is_hurdle || info$is_negbin) { # nolint
    insight::check_if_installed("glmmTMB")

    cond_formula <- .rebuild_cond_formula(formula)
    dispformula <- formula$dispersion
    if (is.null(dispformula)) dispformula <- formula$sigma
    if (is.null(dispformula)) dispformula <- ~1

    ziformula <- formula$zero_inflated
    if (is.null(ziformula)) ziformula <- formula$zi
    if (is.null(ziformula)) ziformula <- ~0

    freq <- tryCatch(
      glmmTMB::glmmTMB(
        formula = cond_formula,
        ziformula = ziformula,
        dispformula = dispformula,
        family = family,
        data = data,
        REML = REML
      ),
      error = function(e) e
    )
  } else if (info$is_gam) {
    insight::check_if_installed("gamm4")

    freq <- tryCatch(
      gamm4::gamm4(
        formula = formula$conditional,
        random = formula$random,
        family = family,
        data = data
      ),
      error = function(e) e
    )
  } else if (info$is_mixed) {
    insight::check_if_installed("lme4")
    insight::check_if_installed("glmmTMB")

    cond_formula <- .rebuild_cond_formula(formula)
    if (info$is_linear) {
      freq <- tryCatch(
        lme4::lmer(
          formula = cond_formula,
          data = data
        ),
        error = function(e) e
      )
    } else {
      ## TODO: check if beta/Gamma are correctly captured
      freq <- tryCatch(
        lme4::glmer(
          formula = cond_formula,
          family = family,
          data = data
        ),
        error = function(e) e
      )
      if (inherits(freq, "error")) {
        freq <- tryCatch(
          glmmTMB::glmmTMB(
            formula = cond_formula,
            family = family,
            data = data
          ),
          error = function(e) e
        )
      }
    }
  } else if (info$is_linear) {
    freq <- stats::lm(formula$conditional, data = data)
  } else {
    freq <- stats::glm(formula$conditional, data = data, family = family)
  }

  freq
}

.rebuild_cond_formula <- function(formula) {
  if (is.null(formula$random)) {
    return(formula$conditional)
  }

  if (is.list(formula$random)) {
    random_formula <- paste(
      lapply(
        formula$random, function(x) {
          paste0("(", as.character(x)[-1], ")")
        }
      ),
      collapse = " + "
    )
  } else {
    random_formula <- paste0("(", as.character(formula$random)[-1], ")")
  }

  fixed_formula <- paste(as.character(formula$conditional)[c(2, 1, 3)], collapse = " ")
  cond_formula <- stats::as.formula(paste(
    fixed_formula, random_formula,
    sep = " + "
  ))

  cond_formula
}

#' @rdname convert_bayesian_as_frequentist
#' @export
bayesian_as_frequentist <- convert_bayesian_as_frequentist
