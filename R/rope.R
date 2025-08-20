#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion of the HDI (default to the `89%` HDI) of a posterior
#' distribution that lies within a region of practical equivalence.
#'
#' @param x Vector representing a posterior distribution. Can also be a
#'   `stanreg` or `brmsfit` model.
#' @param range ROPE's lower and higher bounds. Should be `"default"` or
#' depending on the number of outcome variables a vector or a list. For models
#' with one response, `range` can be:
#'
#' - a vector of length two (e.g., `c(-0.1, 0.1)`),
#' - a list of numeric vector of the same length as numbers of parameters (see
#'   'Examples').
#' - a list of *named* numeric vectors, where names correspond to parameter
#'   names. In this case, all parameters that have no matching name in `range`
#'   will be set to `"default"`.
#'
#' In multivariate models, `range` should be a list with another list (one for
#' each response variable) of numeric vectors . Vector names should correspond to
#' the name of the response variables. If `"default"` and input is a vector, the
#' range is set to `c(-0.1, 0.1)`. If `"default"` and input is a Bayesian model,
#' [`rope_range()`] is used. See 'Examples'.
#' @param ci The Credible Interval (CI) probability, corresponding to the
#'   proportion of HDI, to use for the percentage in ROPE.
#' @param ci_method The type of interval to use to quantify the percentage in
#'   ROPE. Can be 'HDI' (default) or 'ETI'. See [`ci()`].
#' @param complement Should the probabilities above/below the ROPE (the
#'   _complementary_ probabilities) be returned as well? See
#'   [equivalence_test()] as well.
#'
#' @inheritParams hdi
#'
#' @inheritSection hdi Model components
#'
#' @section ROPE:
#' Statistically, the probability of a posterior distribution of being
#' different from 0 does not make much sense (the probability of a single value
#' null hypothesis in a continuous distribution is 0). Therefore, the idea
#' underlining ROPE is to let the user define an area around the null value
#' enclosing values that are *equivalent to the null* value for practical
#' purposes (_Kruschke 2010, 2011, 2014_).
#'
#' Kruschke (2018) suggests that such null value could be set, by default,
#' to the -0.1 to 0.1 range of a standardized parameter (negligible effect
#' size according to Cohen, 1988). This could be generalized: For instance,
#' for linear models, the ROPE could be set as `0 +/- .1 * sd(y)`.
#' This ROPE range can be automatically computed for models using the
#' [`rope_range()`] function.
#'
#' Kruschke (2010, 2011, 2014) suggests using the proportion of  the `95%`
#' (or `89%`, considered more stable) [HDI][hdi] that falls within the
#' ROPE as an index for "null-hypothesis" testing (as understood under the
#' Bayesian framework, see [`equivalence_test()`]).
#'
#' @section Sensitivity to parameter's scale:
#' It is important to consider the unit (i.e., the scale) of the predictors
#' when using an index based on the ROPE, as the correct interpretation of the
#' ROPE as representing a region of practical equivalence to zero is dependent
#' on the scale of the predictors. Indeed, the percentage in ROPE depend on
#' the unit of its parameter. In other words, as the ROPE represents a fixed
#' portion of the response's scale, its proximity with a coefficient depends
#' on the scale of the coefficient itself.
#'
#' @section Multicollinearity - Non-independent covariates:
#' When parameters show strong correlations, i.e. when covariates are not
#' independent, the joint parameter distributions may shift towards or
#' away from the ROPE. Collinearity invalidates ROPE and hypothesis
#' testing based on univariate marginals, as the probabilities are conditional
#' on independence. Most problematic are parameters that only have partial
#' overlap with the ROPE region. In case of collinearity, the (joint) distributions
#' of these parameters may either get an increased or decreased ROPE, which
#' means that inferences based on `rope()` are inappropriate
#' (_Kruschke 2014, 340f_).
#'
#' `rope()` performs a simple check for pairwise correlations between
#' parameters, but as there can be collinearity between more than two variables,
#' a first step to check the assumptions of this hypothesis testing is to look
#' at different pair plots. An even more sophisticated check is the projection
#' predictive variable selection (_Piironen and Vehtari 2017_).
#'
#' @section Strengths and Limitations:
#' **Strengths:** Provides information related to the practical relevance of
#' the effects.
#'
#' **Limitations:** A ROPE range needs to be arbitrarily defined. Sensitive to
#' the scale (the unit) of the predictors. Not sensitive to highly significant
#' effects.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioural sciences.
#' - Kruschke, J. K. (2010). What to believe: Bayesian methods for data analysis.
#'   Trends in cognitive sciences, 14(7), 293-300. \doi{10.1016/j.tics.2010.05.001}.
#' - Kruschke, J. K. (2011). Bayesian assessment of null values via parameter
#'   estimation and model comparison. Perspectives on Psychological Science,
#'   6(3), 299-312. \doi{10.1177/1745691611406925}.
#' - Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with R,
#'   JAGS, and Stan. Academic Press. \doi{10.1177/2515245918771304}.
#' - Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian
#'   estimation. Advances in Methods and Practices in Psychological Science,
#'   1(2), 270-280. \doi{10.1177/2515245918771304}.
#' - Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of Effect
#'   Existence and Significance in the Bayesian Framework. Frontiers in
#'   Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
#' - Piironen, J., & Vehtari, A. (2017). Comparison of Bayesian predictive
#'   methods for model selection. Statistics and Computing, 27(3), 711–735.
#'   \doi{10.1007/s11222-016-9649-y}
#'
#' @examplesIf all(insight::check_if_installed(c("rstanarm", "emmeans", "brms", "BayesFactor"), quietly = TRUE))
#' library(bayestestR)
#'
#' rope(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' rope(x = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#' rope(x = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
#' rope(x = rnorm(1000, 1, 1), ci = c(0.90, 0.95))
#' \donttest{
#' model <- suppressWarnings(
#'   rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' rope(model)
#' rope(model, ci = c(0.90, 0.95))
#'
#' # multiple ROPE ranges
#' rope(model, range = list(c(-10, 5), c(-0.2, 0.2), "default"))
#'
#' # named ROPE ranges
#' rope(model, range = list(gear = c(-3, 2), wt = c(-0.2, 0.2)))
#'
#' rope(emmeans::emtrends(model, ~1, "wt"), ci = c(0.90, 0.95))
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars, refresh = 0)
#' rope(model)
#' rope(model, ci = c(0.90, 0.95))
#'
#' model <- brms::brm(
#'   brms::bf(brms::mvbind(mpg, disp) ~ wt + cyl) + brms::set_rescor(rescor = TRUE),
#'   data = mtcars,
#'   refresh = 0
#' )
#' rope(model)
#' rope(model, ci = c(0.90, 0.95))
#'
#' # different ROPE ranges for model parameters. For each response, a named
#' # list (with the name of the response variable) is required as list-element
#' # for the `range` argument.
#' rope(
#'   model,
#'   range = list(
#'     mpg = list(b_mpg_wt = c(-1, 1), b_mpg_cyl = c(-2, 2)),
#'     disp = list(b_disp_wt = c(-5, 5), b_disp_cyl = c(-4, 4))
#'   )
#' )
#'
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' rope(bf)
#' rope(bf, ci = c(0.90, 0.95))
#' }
#' @export
rope <- function(x, ...) {
  UseMethod("rope")
}


#' @method as.double rope
#' @export
as.double.rope <- function(x, ...) {
  x$ROPE_Percentage
}


#' @export
rope.default <- function(x, ...) {
  NULL
}


#' @rdname rope
#' @export
rope.numeric <- function(x,
                         range = "default",
                         ci = 0.95,
                         ci_method = "ETI",
                         complement = FALSE,
                         verbose = TRUE,
                         ...) {
  if (all(range == "default")) {
    range <- c(-0.1, 0.1)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    insight::format_error("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  rope_values <- lapply(ci, function(i) {
    .rope(
      x,
      range = range,
      ci = i,
      ci_method = ci_method,
      complement = complement,
      verbose = verbose
    )
  })

  # "do.call(rbind)" does not bind attribute values together
  # so we need to capture the information about HDI separately

  out <- do.call(rbind, rope_values)
  if (nrow(out) > 1) {
    iv <- intersect(
      colnames(out),
      c("ROPE_Percentage", "Superiority_Percentage", "Inferiority_Percentage")
    )
    out[iv] <- lapply(out[iv], as.numeric)
  }

  # Attributes
  hdi_area <- cbind(
    CI = ci,
    data.frame(do.call(rbind, lapply(rope_values, attr, "HDI_area")))
  )
  names(hdi_area) <- c("CI", "CI_low", "CI_high")

  attr(out, "HDI_area") <- hdi_area
  attr(out, "data") <- x

  class(out) <- unique(c("rope", "see_rope", class(out)))

  out
}


#' @export
rope.get_predicted <- function(x,
                               range = "default",
                               ci = 0.95,
                               ci_method = "ETI",
                               complement = FALSE,
                               use_iterations = FALSE,
                               verbose = TRUE,
                               ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- rope(
        as.data.frame(t(attributes(x)$iterations)),
        range = range,
        ci = ci,
        ci_method = ci_method,
        complement = complement,
        verbose = verbose,
        ...
      )
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- rope(
      as.numeric(x),
      range = range,
      ci = ci,
      ci_method = ci_method,
      complement = complement,
      verbose = verbose,
      ...
    )
  }
  out
}


#' @export
#' @rdname rope
#' @inheritParams p_direction
rope.data.frame <- function(x,
                            range = "default",
                            ci = 0.95,
                            ci_method = "ETI",
                            complement = FALSE,
                            rvar_col = NULL,
                            verbose = TRUE,
                            ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::rope
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x))
  }

  out <- .prepare_rope_df(x, range, ci, ci_method, complement, verbose)
  HDI_area_attributes <- insight::compact_list(out$HDI_area)
  dat <- data.frame(
    Parameter = rep(names(HDI_area_attributes), each = length(ci)),
    out$tmp,
    stringsAsFactors = FALSE
  )
  row.names(dat) <- NULL

  attr(dat, "HDI_area") <- HDI_area_attributes
  attr(dat, "object_name") <- obj_name

  class(dat) <- c("rope", "see_rope", "data.frame")
  dat
}


#' @export
rope.draws <- function(x,
                       range = "default",
                       ci = 0.95,
                       ci_method = "ETI",
                       complement = FALSE,
                       verbose = TRUE,
                       ...) {
  rope(
    .posterior_draws_to_df(x),
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
}

#' @export
rope.rvar <- rope.draws


#' @export
rope.emmGrid <- function(x,
                         range = "default",
                         ci = 0.95,
                         ci_method = "ETI",
                         complement = FALSE,
                         verbose = TRUE,
                         ...) {
  xdf <- insight::get_parameters(x)
  dat <- rope(
    xdf,
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
  dat <- .append_datagrid(dat, x)
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
rope.emm_list <- rope.emmGrid


#' @export
rope.slopes <- function(x,
                        range = "default",
                        ci = 0.95,
                        ci_method = "ETI",
                        complement = FALSE,
                        verbose = TRUE,
                        ...) {
  xrvar <- .get_marginaleffects_draws(x)
  dat <- rope(
    xrvar,
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
  dat <- .append_datagrid(dat, x)
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
rope.comparisons <- rope.slopes

#' @export
rope.predictions <- rope.slopes


#' @export
rope.BFBayesFactor <- function(x,
                               range = "default",
                               ci = 0.95,
                               ci_method = "ETI",
                               complement = FALSE,
                               verbose = TRUE,
                               ...) {
  if (all(range == "default")) {
    range <- rope_range(x, verbose = verbose)
  }
  out <- rope(
    insight::get_parameters(x),
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
rope.bamlss <- rope.BFBayesFactor


#' @export
rope.MCMCglmm <- function(x,
                          range = "default",
                          ci = 0.95,
                          ci_method = "ETI",
                          complement = FALSE,
                          verbose = TRUE,
                          ...) {
  nF <- x$Fixed$nfl
  out <- rope(
    as.data.frame(x$Sol[, 1:nF, drop = FALSE]),
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
rope.mcmc <- function(x,
                      range = "default",
                      ci = 0.95,
                      ci_method = "ETI",
                      complement = FALSE,
                      verbose = TRUE,
                      ...) {
  out <- rope(
    as.data.frame(x),
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- NULL
  attr(out, "data") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
rope.bcplm <- function(x,
                       range = "default",
                       ci = 0.95,
                       ci_method = "ETI",
                       complement = FALSE,
                       verbose = TRUE,
                       ...) {
  out <- rope(
    insight::get_parameters(x),
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- NULL
  attr(out, "data") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
rope.bayesQR <- rope.bcplm

#' @export
rope.blrm <- rope.bcplm

#' @export
rope.BGGM <- rope.bcplm

#' @export
rope.mcmc.list <- rope.bcplm


#' @rdname rope
#' @export
rope.stanreg <- function(x,
                         range = "default",
                         ci = 0.95,
                         ci_method = "ETI",
                         complement = FALSE,
                         effects = "fixed",
                         component = "location",
                         parameters = NULL,
                         verbose = TRUE,
                         ...) {
  effects <- insight::validate_argument(effects, c("fixed", "random", "all"))
  component <- insight::validate_argument(
    component,
    c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary")
  )

  if (all(range == "default")) {
    range <- rope_range(x, verbose = verbose)
  } else if (!is.list(range) && (!all(is.numeric(range)) || length(range) != 2)) {
    insight::format_error("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  # check for possible collinearity that might bias ROPE
  if (verbose && !inherits(x, "blavaan")) .check_multicollinearity(x, "rope")

  rope_data <- rope(
    insight::get_parameters(
      x,
      effects = effects,
      component = component,
      parameters = parameters
    ),
    range = range,
    ci = ci,
    ci_method = ci_method,
    complement = complement,
    verbose = verbose,
    ...
  )

  out <- .prepare_output(
    rope_data,
    .get_cleaned_parameters(x, ...),
    inherits(x, "stanmvreg")
  )

  attr(out, "HDI_area") <- attr(rope_data, "HDI_area")
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- class(rope_data)

  out
}

#' @export
rope.stanfit <- rope.stanreg

#' @export
rope.blavaan <- rope.stanreg


#' @rdname rope
#' @export
rope.brmsfit <- function(x,
                         range = "default",
                         ci = 0.95,
                         ci_method = "ETI",
                         complement = FALSE,
                         effects = "fixed",
                         component = "conditional",
                         parameters = NULL,
                         verbose = TRUE,
                         ...) {
  # check range argument
  if (all(range == "default")) {
    range <- rope_range(x, verbose = verbose)
    # we expect a list with named vectors (length two) in the multivariate case.
    # Names state the response variable.
  } else if (insight::is_multivariate(x)) {
    if (
      !is.list(range) ||
        length(range) < length(insight::find_response(x)) ||
        !all(names(range) %in% insight::find_response(x))
    ) {
      insight::format_error(
        "With a multivariate model, `range` should be 'default' or a list with multiple lists (one for each response) of named numeric vectors with length 2."
      )
    }
  } else if (!is.list(range) && (!all(is.numeric(range)) || length(range) != 2)) {
    insight::format_error(
      "`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1))."
    )
  }

  # check for possible collinearity that might bias ROPE and print a warning
  if (verbose) .check_multicollinearity(x, "rope")

  # calc rope
  if (insight::is_multivariate(x)) {
    dv <- insight::find_response(x)

    # ROPE range / width differs between response varialbe. Thus ROPE is
    # calculated for every variable on its own.
    rope_data <- lapply(
      dv,
      function(dv_item) {
        ret <- rope(
          insight::get_parameters(
            x,
            effects = effects,
            component = component,
            parameters = parameters,
            ...
          ),
          range = range[[dv_item]],
          ci = ci,
          ci_method = ci_method,
          complement = complement,
          verbose = verbose,
          ...
        )

        # It's a waste of performance to calculate ROPE for all parameters
        # with the ROPE width of a specific response variable and to throw
        # away the unwanted results. However, performance impact should not be
        # too high and this way it is much easier to handle the `parameters`
        # argument.
        ret[grepl(paste0("(.*)", dv_item), ret$Parameter), ]
      }
    )
    rope_data <- do.call(rbind, rope_data)

    out <- .prepare_output(
      rope_data,
      .get_cleaned_parameters(x, ...),
      is_brms_mv = TRUE
    )
  } else {
    rope_data <- rope(
      insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
      range = range,
      ci = ci,
      ci_method = ci_method,
      complement = complement,
      verbose = verbose,
      ...
    )

    out <- .prepare_output(rope_data, .get_cleaned_parameters(x, ...))
  }

  attr(out, "HDI_area") <- attr(rope_data, "HDI_area")
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- class(rope_data)

  out
}


#' @export
rope.sim.merMod <- function(x,
                            range = "default",
                            ci = 0.95,
                            ci_method = "ETI",
                            complement = FALSE,
                            effects = c("fixed", "random", "all"),
                            parameters = NULL,
                            verbose = TRUE,
                            ...) {
  if (all(range == "default")) {
    range <- rope_range(x, verbose = verbose)
  } else if (!is.list(range) && (!all(is.numeric(range)) || length(range) != 2)) {
    insight::format_error("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  rope_list <- lapply(c("fixed", "random"), function(.x) {
    parms <- insight::get_parameters(x, effects = .x, parameters = parameters)

    getropedata <- .prepare_rope_df(parms, range, ci, ci_method, complement, verbose)
    tmp <- getropedata$tmp
    HDI_area <- getropedata$HDI_area

    if (insight::is_empty_object(tmp)) {
      tmp <- NULL
    } else {
      tmp <- .clean_up_tmp_stanreg(
        tmp,
        group = .x,
        cols = c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Group"),
        parms = names(parms)
      )

      if (!insight::is_empty_object(HDI_area)) {
        attr(tmp, "HDI_area") <- HDI_area
      }
    }

    tmp
  })

  dat <- do.call(
    rbind,
    args = c(insight::compact_list(rope_list), make.row.names = FALSE)
  )

  dat <- switch(effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- datawizard::data_remove(dat, "Group", verbose = FALSE)
  }

  HDI_area_attributes <- lapply(insight::compact_list(rope_list), attr, "HDI_area")

  if (effects != "all") {
    HDI_area_attributes <- HDI_area_attributes[[1]]
  } else {
    names(HDI_area_attributes) <- c("fixed", "random")
  }

  attr(dat, "HDI_area") <- HDI_area_attributes
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))

  dat
}


#' @export
rope.sim <- function(x, range = "default", ci = 0.95, ci_method = "ETI", complement = FALSE,
                     parameters = NULL, verbose = TRUE, ...) {
  if (all(range == "default")) {
    range <- rope_range(x, verbose = verbose)
  } else if (!is.list(range) && (!all(is.numeric(range)) || length(range) != 2)) {
    insight::format_error("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  parms <- insight::get_parameters(x, parameters = parameters)
  getropedata <- .prepare_rope_df(parms, range, ci, ci_method, complement, verbose)

  dat <- getropedata$tmp
  HDI_area <- getropedata$HDI_area

  if (insight::is_empty_object(dat)) {
    dat <- NULL
  } else {
    dat <- .clean_up_tmp_stanreg(
      dat,
      group = "fixed",
      cols = c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage"),
      parms = names(parms)
    )

    if (!insight::is_empty_object(HDI_area)) {
      attr(dat, "HDI_area") <- HDI_area
    }
  }

  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))

  dat
}


# helper -------------------------------------------------------------------


#' @keywords internal
.rope <- function(x,
                  range = c(-0.1, 0.1),
                  ci = 0.95,
                  ci_method = "ETI",
                  complement = FALSE,
                  verbose = TRUE) {
  ci_bounds <- ci(x, ci = ci, method = ci_method, verbose = verbose)

  if (anyNA(ci_bounds)) {
    inferiority_percentage <- superiority_percentage <- rope_percentage <- NA
  } else {
    HDI_area <- x[x >= ci_bounds$CI_low & x <= ci_bounds$CI_high]
    rope_percentage <- mean(HDI_area >= min(range) & HDI_area <= max(range))
    superiority_percentage <- mean(HDI_area > max(range))
    inferiority_percentage <- mean(HDI_area < min(range))
  }

  rope <- data.frame(
    CI = ci,
    ROPE_low = range[1],
    ROPE_high = range[2],
    ROPE_Percentage = rope_percentage
  )

  if (isTRUE(complement)) {
    rope[["Superiority_Percentage"]] <- superiority_percentage
    rope[["Inferiority_Percentage"]] <- inferiority_percentage
  }

  attr(rope, "HDI_area") <- c(ci_bounds$CI_low, ci_bounds$CI_high)
  attr(rope, "CI_bounds") <- c(ci_bounds$CI_low, ci_bounds$CI_high)
  class(rope) <- unique(c("rope", "see_rope", class(rope)))
  rope
}


#' @keywords internal
.prepare_rope_df <- function(parms, range, ci, ci_method, complement, verbose) {
  if (is.list(range)) {
    # check if list of values contains only valid values
    range <- .check_list_range(range, parms)
    # apply thresholds to each column
    tmp <- mapply(
      function(p, r) {
        rope(
          p,
          range = r,
          ci = ci,
          ci_method = ci_method,
          complement = complement,
          verbose = verbose
        )
      },
      parms,
      range,
      SIMPLIFY = FALSE
    )
  } else {
    tmp <- sapply(
      parms,
      rope,
      range = range,
      ci = ci,
      ci_method = ci_method,
      complement = complement,
      verbose = verbose,
      simplify = FALSE
    )
  }

  HDI_area <- lapply(tmp, attr, which = "HDI_area")

  # HDI_area <- lapply(HDI_area, function(.x) {
  #   dat <- cbind(CI = ci, data.frame(do.call(rbind, .x)))
  #   colnames(dat) <- c("CI", "HDI_low", "HDI_high")
  #   dat
  # })

  list(
    tmp = do.call(rbind, tmp),
    HDI_area = HDI_area
  )
}
