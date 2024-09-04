#' Probability of Direction (pd)
#'
#' Compute the **Probability of Direction** (***pd***, also known as the Maximum
#' Probability of Effect - *MPE*). This can be interpreted as the probability
#' that a parameter (described by its posterior distribution) is strictly
#' positive or negative (whichever is the most probable). Although differently
#' expressed, this index is fairly similar (*i.e.*, is strongly correlated) to
#' the frequentist **p-value** (see details).
#'
#' @param x A vector representing a posterior distribution, a data frame of
#'   posterior draws (samples be parameter). Can also be a Bayesian model.
#' @param method Can be `"direct"` or one of methods of [`estimate_density()`],
#'   such as `"kernel"`, `"logspline"` or `"KernSmooth"`. See details.
#' @param null The value considered as a "null" effect. Traditionally 0, but
#'   could also be 1 in the case of ratios of change (OR, IRR, ...).
#' @param as_p If `TRUE`, the p-direction (pd) values are converted to a
#' frequentist p-value using [`pd_to_p()`].
#' @param remove_na Should missing values be removed before computation? Note
#' that `Inf` (infinity) are *not* removed.
#' @param rvar_col Name of an `rvar`-type column. If `NULL`, each column in the
#' data frame is assumed to represent draws from a posterior distribution.
#' @inheritParams hdi
#'
#' @section What is the *pd*?:
#'
#' The Probability of Direction (pd) is an index of effect existence, representing
#' the certainty with which an effect goes in a particular direction (i.e., is
#' positive or negative / has a sign), typically ranging from 0.5 to 1 (but see
#' next section for cases where it can range between 0 and 1). Beyond
#' its simplicity of interpretation, understanding and computation, this index
#' also presents other interesting properties:
#' - Like other posterior-based indices, *pd* is solely based on the posterior
#'   distributions and does not require any additional information from the data
#'   or the model (e.g., such as priors, as in the case of Bayes factors).
#' - It is robust to the scale of both the response variable and the predictors.
#' - It is strongly correlated with the frequentist p-value, and can thus
#'   be used to draw parallels and give some reference to readers non-familiar
#'   with Bayesian statistics (Makowski et al., 2019).
#'
#' @section Relationship with the p-value:
#'
#' In most cases, it seems that the *pd* has a direct correspondence with the
#' frequentist one-sided *p*-value through the formula (for two-sided *p*):
#' \deqn{p = 2 \times (1 - p_d)}{p = 2 * (1 - pd)}
#' Thus, a two-sided p-value of respectively `.1`, `.05`, `.01` and `.001` would
#' correspond approximately to a *pd* of `95%`, `97.5%`, `99.5%` and `99.95%`.
#' See [pd_to_p()] for details.
#'
#' @section Possible Range of Values:
#'
#' The largest value *pd* can take is 1 - the posterior is strictly directional.
#' However, the smallest value *pd* can take depends on the parameter space
#' represented by the posterior.
#'
#' **For a continuous parameter space**, exact values of 0 (or any point null
#' value) are not possible, and so 100% of the posterior has _some_ sign, some
#' positive, some negative. Therefore, the smallest the *pd* can be is 0.5 -
#' with an equal posterior mass of positive and negative values. Values close to
#' 0.5 _cannot_ be used to support the null hypothesis (that the parameter does
#' _not_ have a direction) is a similar why to how large p-values cannot be used
#' to support the null hypothesis (see [`pd_to_p()`]; Makowski et al., 2019).
#'
#' **For a discrete parameter space or a parameter space that is a mixture
#' between discrete and continuous spaces**, exact values of 0 (or any point
#' null value) _are_ possible! Therefore, the smallest the *pd* can be is 0 -
#' with 100% of the posterior mass on 0. Thus values close to 0 can be used to
#' support the null hypothesis (see van den Bergh et al., 2021).
#'
#' Examples of posteriors representing discrete parameter space:
#' - When a parameter can only take discrete values.
#' - When a mixture prior/posterior is used (such as the spike-and-slab prior;
#'   see van den Bergh et al., 2021).
#' - When conducting Bayesian model averaging (e.g., [weighted_posteriors()] or
#'   `brms::posterior_average`).
#'
#' @section Methods of computation:
#'
#' The *pd* is defined as:
#' \deqn{p_d = max({Pr(\hat{\theta} < \theta_{null}), Pr(\hat{\theta} > \theta_{null})})}{pd = max(mean(x < null), mean(x > null))}
#'
#' The most simple and direct way to compute the *pd* is to compute the
#' proportion of positive (or larger than `null`) posterior samples, the
#' proportion of negative (or smaller than `null`) posterior samples, and take
#' the larger of the two. This "simple" method is the most straightforward, but
#' its precision is directly tied to the number of posterior draws.
#'
#' The second approach relies on [density estimation][estimate_density]: It starts by
#' estimating the continuous-smooth density function (for which many methods are
#' available), and then computing the [area under the curve][area_under_curve]
#' (AUC) of the density curve on either side of `null` and taking the maximum
#' between them. Note the this approach assumes a continuous density function,
#' and so **when the posterior represents a (partially) discrete parameter
#' space, only the direct method _must_ be used** (see above).
#'
#' @return
#' Values between 0.5 and 1 *or* between 0 and 1 (see above) corresponding to
#' the probability of direction (pd).
#'
#' @seealso [pd_to_p()] to convert between Probability of Direction (pd) and p-value.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @references
#' - Makowski, D., Ben-Shachar, M. S., Chen, S. A., & Lüdecke, D. (2019).
#'   Indices of effect existence and significance in the Bayesian framework.
#'   Frontiers in psychology, 10, 2767. \doi{10.3389/fpsyg.2019.02767}
#' - van den Bergh, D., Haaf, J. M., Ly, A., Rouder, J. N., & Wagenmakers, E. J.
#'   (2021). A cautionary note on estimating effect size. Advances in Methods
#'   and Practices in Psychological Science, 4(1). \doi{10.1177/2515245921992035}
#'
#' @examplesIf requireNamespace("rstanarm", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE) && requireNamespace("brms", quietly = TRUE) && requireNamespace("BayesFactor", quietly = TRUE)
#' library(bayestestR)
#'
#' # Simulate a posterior distribution of mean 1 and SD 1
#' # ----------------------------------------------------
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' p_direction(posterior)
#' p_direction(posterior, method = "kernel")
#'
#' # Simulate a dataframe of posterior distributions
#' # -----------------------------------------------
#' df <- data.frame(replicate(4, rnorm(100)))
#' p_direction(df)
#' p_direction(df, method = "kernel")
#' \donttest{
#' # rstanarm models
#' # -----------------------------------------------
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl,
#'   data = mtcars,
#'   chains = 2, refresh = 0
#' )
#' p_direction(model)
#' p_direction(model, method = "kernel")
#'
#' # emmeans
#' # -----------------------------------------------
#' p_direction(emmeans::emtrends(model, ~1, "wt", data = mtcars))
#'
#' # brms models
#' # -----------------------------------------------
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_direction(model)
#' p_direction(model, method = "kernel")
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' p_direction(bf)
#' p_direction(bf, method = "kernel")
#' }
#' @export
p_direction <- function(x, ...) {
  UseMethod("p_direction")
}

#' @rdname p_direction
#' @export
pd <- p_direction


#' @export
p_direction.default <- function(x, ...) {
  insight::format_error(paste0("'p_direction()' is not yet implemented for objects of class '", class(x)[1], "'."))
}


#' @rdname p_direction
#' @export
p_direction.numeric <- function(x,
                                method = "direct",
                                null = 0,
                                as_p = FALSE,
                                remove_na = TRUE,
                                ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))
  out <- p_direction(
    data.frame(Posterior = x),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
  attr(out, "object_name") <- obj_name
  out
}


#' @rdname p_direction
#' @export
p_direction.data.frame <- function(x,
                                   method = "direct",
                                   null = 0,
                                   as_p = FALSE,
                                   remove_na = TRUE,
                                   rvar_col = NULL,
                                   ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  if (is.null(rvar_col)) {
    return(.p_direction_df(
      x,
      method = method,
      null = null,
      as_p = as_p,
      remove_na = remove_na,
      obj_name = obj_name,
      ...
    ))
  }

  if (length(rvar_col) != 1L && !rvar_col %in% colnames(x)) {
    insight::format_error("The `rvar_col` argument must be a single, valid column name.")
  }

  out <- p_direction(
    x[[rvar_col]],
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )

  x[["pd"]] <- out[["pd"]]
  attr(x, "object_name") <- obj_name
  attr(x, "as_p") <- as_p

  x
}


#' @keywords internal
.p_direction_df <- function(x,
                            method = "direct",
                            null = 0,
                            as_p = FALSE,
                            remove_na = TRUE,
                            obj_name = NULL,
                            ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    pd <- .p_direction(
      x[[1]],
      method = method,
      null = null,
      as_p = as_p,
      remove_na = remove_na,
      ...
    )
  } else {
    pd <- sapply(
      x,
      .p_direction,
      method = method,
      null = null,
      as_p = as_p,
      remove_na = remove_na,
      simplify = TRUE,
      ...
    )
  }

  out <- data.frame(
    Parameter = names(x),
    pd = pd,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # rename column
  if (as_p) {
    colnames(out)[2] <- "p"
  }

  attr(out, "object_name") <- obj_name
  attr(out, "as_p") <- as_p
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))

  out
}


#' @export
p_direction.draws <- function(x,
                              method = "direct",
                              null = 0,
                              as_p = FALSE,
                              remove_na = TRUE,
                              ...) {
  p_direction(
    .posterior_draws_to_df(x),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
}

#' @export
p_direction.rvar <- p_direction.draws


#' @rdname p_direction
#' @export
p_direction.MCMCglmm <- function(x,
                                 method = "direct",
                                 null = 0,
                                 as_p = FALSE,
                                 remove_na = TRUE,
                                 ...) {
  nF <- x$Fixed$nfl
  out <- p_direction(as.data.frame(x$Sol[, 1:nF, drop = FALSE]),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_direction.mcmc <- function(x,
                             method = "direct",
                             null = 0,
                             as_p = FALSE,
                             remove_na = TRUE,
                             ...) {
  p_direction(
    as.data.frame(x),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
}


#' @export
p_direction.BGGM <- function(x, method = "direct", null = 0, as_p = FALSE, remove_na = TRUE, ...) {
  p_direction(as.data.frame(x), method = method, null = null, as_p = as_p, remove_na = remove_na, ...)
}


#' @export
p_direction.bcplm <- function(x, method = "direct", null = 0, as_p = FALSE, remove_na = TRUE, ...) {
  p_direction(insight::get_parameters(x), method = method, null = null, as_p = as_p, remove_na = remove_na, ...)
}

#' @export
p_direction.mcmc.list <- p_direction.bcplm

#' @export
p_direction.blrm <- p_direction.bcplm

#' @export
p_direction.bayesQR <- p_direction.bcplm


#' @export
p_direction.bamlss <- function(x,
                               method = "direct",
                               null = 0,
                               as_p = FALSE,
                               remove_na = TRUE,
                               component = c("all", "conditional", "location"),
                               ...) {
  component <- match.arg(component)
  out <- p_direction(
    insight::get_parameters(x, component = component),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
  out <- .add_clean_parameters_attribute(out, x)
  out
}


#' @rdname p_direction
#' @export
p_direction.emmGrid <- function(x, method = "direct", null = 0, as_p = FALSE, remove_na = TRUE, ...) {
  xdf <- insight::get_parameters(x)
  out <- p_direction(xdf, method = method, null = null, as_p = as_p, remove_na = remove_na, ...)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_direction.emm_list <- p_direction.emmGrid

#' @rdname p_direction
#' @export
p_direction.slopes <- function(x, method = "direct", null = 0, as_p = FALSE, remove_na = TRUE, ...) {
  xrvar <- .get_marginaleffects_draws(x)
  out <- p_direction(xrvar, method = method, null = null, as_p = as_p, remove_na = remove_na, ...)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_direction.comparisons <- p_direction.slopes

#' @export
p_direction.predictions <- p_direction.slopes


#' @keywords internal
.p_direction_models <- function(x,
                                effects,
                                component,
                                parameters,
                                method = "direct",
                                null = 0,
                                as_p = FALSE,
                                remove_na = TRUE,
                                ...) {
  p_direction(
    insight::get_parameters(
      x,
      effects = effects,
      component = component,
      parameters = parameters
    ),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
}


#' @export
p_direction.sim.merMod <- function(x,
                                   effects = c("fixed", "random", "all"),
                                   parameters = NULL,
                                   method = "direct",
                                   null = 0,
                                   as_p = FALSE,
                                   remove_na = TRUE,
                                   ...) {
  effects <- match.arg(effects)

  out <- .p_direction_models(
    x = x,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
  attr(out, "data") <- insight::get_parameters(x, effects = effects, parameters = parameters)
  out
}


#' @export
p_direction.sim <- function(x,
                            parameters = NULL,
                            method = "direct",
                            null = 0,
                            as_p = FALSE,
                            remove_na = TRUE,
                            ...) {
  out <- .p_direction_models(
    x = x,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}



#' @rdname p_direction
#' @export
p_direction.stanreg <- function(x,
                                effects = c("fixed", "random", "all"),
                                component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                                parameters = NULL,
                                method = "direct",
                                null = 0,
                                as_p = FALSE,
                                remove_na = TRUE,
                                ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    p_direction(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters
      ),
      method = method,
      null = null,
      as_p = as_p,
      remove_na = remove_na,
      ...
    ),
    cleaned_parameters,
    inherits(x, "stanmvreg")
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "as_p") <- as_p
  out
}

#' @export
p_direction.stanfit <- p_direction.stanreg

#' @export
p_direction.blavaan <- p_direction.stanreg


#' @rdname p_direction
#' @export
p_direction.brmsfit <- function(x,
                                effects = c("fixed", "random", "all"),
                                component = c("conditional", "zi", "zero_inflated", "all"),
                                parameters = NULL,
                                method = "direct",
                                null = 0,
                                as_p = FALSE,
                                remove_na = TRUE,
                                ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    p_direction(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters
      ),
      method = method,
      null = null,
      as_p = as_p,
      remove_na = remove_na,
      ...
    ),
    cleaned_parameters
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "as_p") <- as_p
  out
}


#' @rdname p_direction
#' @export
p_direction.BFBayesFactor <- function(x,
                                      method = "direct",
                                      null = 0,
                                      as_p = FALSE,
                                      remove_na = TRUE,
                                      ...) {
  out <- p_direction(
    insight::get_parameters(x),
    method = method,
    null = null,
    as_p = as_p,
    remove_na = remove_na,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @rdname p_direction
#' @export
p_direction.get_predicted <- function(x,
                                      method = "direct",
                                      null = 0,
                                      as_p = FALSE,
                                      remove_na = TRUE,
                                      use_iterations = FALSE,
                                      verbose = TRUE,
                                      ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- p_direction(
        as.data.frame(t(attributes(x)$iterations)),
        method = method,
        null = null,
        as_p = as_p,
        remove_na = remove_na,
        verbose = verbose,
        ...
      )
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- p_direction(as.numeric(x),
      method = method,
      null = null,
      as_p = as_p,
      remove_na = remove_na,
      verbose = verbose,
      ...
    )
  }
  out
}

#' @export
p_direction.parameters_model <- function(x, ...) {
  out <- data.frame(
    Parameter = x$Parameter,
    pd = p_to_pd(p = x[["p"]]),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (!is.null(x$Component)) {
    out$Component <- x$Component
  }

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))

  out
}



# Definition --------------------------------------------------------------


#' @keywords internal
.p_direction <- function(x,
                         method = "direct",
                         null = 0,
                         as_p = FALSE,
                         remove_na = TRUE,
                         ...) {
  # handle missing values
  if (remove_na) {
    x <- x[!is.na(x)]
  }
  # sanity check
  if (length(x) == 0) {
    insight::format_error("No valid values found. Maybe the data contains only missing values.")
  }
  # sanity check
  if (anyNA(x)) {
    return(NA_real_)
  }

  # any inf values? then warn...
  if (any(is.infinite(x))) {
    insight::format_warning("Infinite values detected. These are not removed. Please check your results carefully!")
  }

  if (method == "direct") {
    pdir <- max(
      length(x[x > null]), # pd positive
      length(x[x < null]) # pd negative
    ) / length(x)
  } else {
    dens <- estimate_density(x, method = method, precision = 2^10, extend = TRUE, ...)
    if (length(x[x > null]) > length(x[x < null])) {
      dens <- dens[dens$x > null, ]
    } else {
      dens <- dens[dens$x < null, ]
    }
    pdir <- area_under_curve(dens$x, dens$y, method = "spline")
    if (pdir >= 1) {
      # Enforce bounds
      pdir <- 1
    }
  }

  # convert to frequentist p?
  if (as_p) {
    pdir <- pd_to_p(pdir)
  }

  pdir
}

# Methods -----------------------------------------------------------------


#' Convert to Numeric
#'
#' @inheritParams base::as.numeric
#' @method as.numeric p_direction
#' @export
as.numeric.p_direction <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    # check if we have frequentist p-values
    if (isTRUE(attributes(x)$as_p) && "p" %in% colnames(x)) {
      as.numeric(as.vector(x$p))
    } else {
      as.numeric(as.vector(x$pd))
    }
  } else {
    as.vector(x)
  }
}


#' @method as.double p_direction
#' @export
as.double.p_direction <- as.numeric.p_direction

#' @method as.vector p_direction
#' @export
as.vector.p_direction <- as.numeric.p_direction
