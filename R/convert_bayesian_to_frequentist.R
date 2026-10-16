#' Convert (refit) a Bayesian model to frequentist
#'
#' Refit Bayesian model as frequentist. Can be useful for comparisons.
#'
#' @details
#' Linear models from *BayesFactor* with random factors (see `whichRandom` in
#' `BayesFactor::lmBF()`) are refit with `lme4::lmer()`. A random term with only
#' factors, such as `cyl:gear`, becomes `(1 | cyl:gear)`. A random term with a
#' continuous variable, such as `Days:Subject`, becomes `(0 + Days | Subject)`.
#'
#' @param model A Bayesian model.
#' @param data Data used by the model. If `NULL`, will try to extract it
#'   from the model.
#' @param REML For mixed effects, should models be estimated using
#'   restricted maximum likelihood (REML) (`TRUE`, default) or maximum
#'   likelihood (`FALSE`)?
#' @param resp For multivariate *brms* models, the name of the one response to
#'   convert, as returned by `names(insight::find_formula(model))`. Other models
#'   ignore `resp`.
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
convert_bayesian_as_frequentist <- function(
  model,
  data = NULL,
  REML = TRUE,
  resp = NULL
) {
  if (is.null(data)) {
    data <- insight::get_data(model)
  }

  # BayesFactor mixed models: random terms are not in lme4 syntax, so we build
  # the formula from the model's data types
  if (inherits(model, "BFBayesFactor")) {
    bf_formula <- .bf_mixed_formula(model)
    if (!is.null(bf_formula)) {
      insight::check_if_installed("lme4")
      freq <- tryCatch(
        lme4::lmer(formula = bf_formula, data = data, REML = REML),
        error = function(e) e
      )
      if (inherits(freq, "error")) {
        insight::format_error(
          "Model could not be automatically converted to frequentist model.",
          paste0("`lme4::lmer()` failed with: ", conditionMessage(freq))
        )
      }
      return(freq)
    }
  }

  if (inherits(model, "brmsfit") && insight::is_multivariate(model)) {
    # multivariate brms models: convert only the response named in `resp`
    model_formulas <- insight::find_formula(model)
    responses <- names(model_formulas)
    if (!is.character(resp) || length(resp) != 1 || !resp %in% responses) {
      insight::format_error(
        "For multivariate brms models, `resp` must name one response of the model.",
        paste0("Possible responses are: ", toString(responses), ".")
      )
    }
    info <- insight::model_info(
      model,
      response = which(responses == resp),
      verbose = FALSE
    )
    model_formula <- model_formulas[[resp]]
    model_family <- insight::get_family(model)[[resp]]
  } else {
    info <- insight::model_info(model, verbose = FALSE)
    model_formula <- insight::find_formula(model)
    model_family <- insight::get_family(model)
  }

  # fix exception: The 0 + Intercept syntax in brms can be used to facilitate
  # prior specification for the intercept, but but it leads to issues where it
  # wrongly can be believed that Intercept is a variable and not a special term.
  f_string <- insight::safe_deparse(model_formula$conditional)
  if (grepl("0 + Intercept", f_string, fixed = TRUE)) {
    model_formula$conditional <- stats::as.formula(gsub("0 + Intercept", "1", f_string, fixed = TRUE))
  }

  if (inherits(model_family, "brmsfamily")) {
    insight::check_if_installed("glmmTMB")
    # exception: ordbetareg()
    if ("custom" %in% model_family$family && all(model_family$name == "ord_beta_reg")) {
      model_family <- glmmTMB::ordbeta()
    } else {
      # not all families return proper objects from "get", so we capture
      # some families via switch here...
      model_family <- .safe(switch(model_family$family,
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
  stats::as.formula(paste(fixed_formula, random_formula, sep = " + "))
}

.bf_mixed_formula <- function(model) {
  # same model that insight::find_formula() uses (the last numerator)
  bf_model <- utils::tail(model@numerator, 1)[[1]]
  if (!inherits(bf_model, "BFlinearModel")) {
    return(NULL)
  }
  data_types <- bf_model@dataTypes
  if (!"random" %in% data_types) {
    return(NULL)
  }

  f <- stats::as.formula(bf_model@identifier$formula)
  fixed_terms <- NULL
  random_terms <- NULL
  for (term in attr(stats::terms(f), "term.labels")) {
    # term labels keep backticks for non-syntactic names, data types do not
    term_vars <- all.vars(str2lang(term))
    term_types <- data_types[term_vars]
    if (!any(term_types == "random")) {
      fixed_terms <- c(fixed_terms, term)
    } else if (any(term_types == "continuous")) {
      # random slopes without an intercept; a random intercept is its own
      # term, so the two are uncorrelated
      random_terms <- c(random_terms, sprintf(
        "(0 + %s | %s)",
        paste0("`", term_vars[term_types == "continuous"], "`", collapse = ":"),
        paste0("`", term_vars[term_types != "continuous"], "`", collapse = ":")
      ))
    } else {
      random_terms <- c(random_terms, sprintf("(1 | %s)", term))
    }
  }
  if (is.null(fixed_terms)) {
    fixed_terms <- "1"
  }

  stats::reformulate(c(fixed_terms, random_terms), response = f[[2]])
}

#' @rdname convert_bayesian_as_frequentist
#' @export
bayesian_as_frequentist <- convert_bayesian_as_frequentist
