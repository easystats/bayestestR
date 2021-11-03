#' Convert (refit) a Bayesian model to frequentist
#'
#' Refit Bayesian model as frequentist. Can be useful for comparisons.
#'
#' @param model A Bayesian model.
#' @param data Data used by the model. If `NULL`, will try to extract it
#'   from the model.
#' @examples
#' \donttest{
#' # Rstanarm ----------------------
#' if (require("rstanarm")) {
#'   # Simple regressions
#'   model <- stan_glm(Sepal.Length ~ Species,
#'     data = iris, chains = 2, refresh = 0
#'   )
#'   bayesian_as_frequentist(model)
#' }
#' }
#'
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- stan_glm(vs ~ mpg,
#'     family = "binomial",
#'     data = mtcars, chains = 2, refresh = 0
#'   )
#'   bayesian_as_frequentist(model)
#'
#'   # Mixed models
#'   model <- stan_glmer(Sepal.Length ~ Petal.Length + (1 | Species),
#'     data = iris, chains = 2, refresh = 0
#'   )
#'   bayesian_as_frequentist(model)
#'
#'   model <- stan_glmer(vs ~ mpg + (1 | cyl),
#'     family = "binomial",
#'     data = mtcars, chains = 2, refresh = 0
#'   )
#'   bayesian_as_frequentist(model)
#' }
#' }
#'
#' @export
convert_bayesian_as_frequentist <- function(model, data = NULL, REML = TRUE) {
  if (is.null(data)) {
    data <- insight::get_data(model)
  }

  info <- insight::model_info(model)
  formula <- insight::find_formula(model)
  family <- insight::get_family(model)

  freq <- .convert_bayesian_as_frequentist(
    info = info, formula = formula, data = data, family = family, REML = REML
  )

  freq
}

# internaal

.convert_bayesian_as_frequentist <- function(info, formula, data, family, REML = TRUE) {
  # TODO: Check for
  # nonlinear formulas,
  # correlation structures,
  # weights,
  # offset,
  # subset,
  # knots,
  # meta-analysis
  if (info$is_dispersion ||
      info$is_zero_inflated ||
      info$is_zeroinf ||
      info$is_hurdle) {
    insight::check_if_installed("glmmTMB")

    cond_formula <- .rebuild_cond_formula(formula)
    dispformula <- formula$dispersion
    if (is.null(dispformula)) dispformula <- formula$sigma
    if (is.null(dispformula)) dispformula <- ~ 1

    ziformula <- formula$zero_inflated
    if (is.null(ziformula)) ziformula <- formula$zi
    if (is.null(ziformula)) ziformula <- ~ 0

    freq <- tryCatch(
      glmmTMB::glmmTMB(
        formula = cond_formula,
        ziformula = ziformula,
        dispformula = dispformula,
        family = family,
        data = data,
        REML = REML
      ), error = function(e) e
    )
  } else if (info$is_gam) {
    insight::check_if_installed("gamm4")

    freq <- tryCatch(
      gamm4::gamm4(
        formula = formula$conditional,
        random = formula$random,
        family = family,
        data = data
      ), error = function(e) e
    )

  } else if (info$is_mixed) {
    insight::check_if_installed("lme4")

    cond_formula <- .rebuild_cond_formula(formula)
    if (info$is_linear) {
      freq <- tryCatch(
        lme4::lmer(
          formula = cond_formula,
          data = data
        ), error = function(e) e
      )
    } else {
      freq <- tryCatch(
        lme4::glmer(
          formula = cond_formula,
          family = family,
          data = data
        ), error = function(e) e
      )
    }
  } else {
    if (info$is_linear) {
      freq <- stats::lm(formula$conditional, data = data)
    } else {
      freq <- stats::glm(formula$conditional, data = data, family = family)
    }
  }

  if (inherits(freq, "error")) {
    stop(insight::format_message(
      "Model could not be automatically converted to frequentist model."
    ), call. = FALSE)
  } else {
    return(freq)
  }
}

.rebuild_cond_formula <- function(formula) {
  if (is.null(formula$random)) {
    return(formula$conditional)
  } else {
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
    cond_formula <- as.formula(paste(
      fixed_formula, random_formula, sep = " + "
    ))
    return(cond_formula)
  }
}

#' @rdname convert_bayesian_as_frequentist
#' @export
bayesian_as_frequentist <- convert_bayesian_as_frequentist
