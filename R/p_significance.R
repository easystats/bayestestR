#' Practical Significance (ps)
#'
#' Compute the probability of **Practical Significance** (***ps***), which can
#' be conceptualized as a unidirectional equivalence test. It returns the
#' probability that effect is above a given threshold corresponding to a
#' negligible effect in the median's direction. Mathematically, it is defined as
#' the proportion of the posterior distribution of the median sign above the
#' threshold.
#'
#' @param threshold The threshold value that separates significant from
#' negligible effect, which can have following possible values:
#' - `"default"`, in which case the range is set to `0.1` if input is a vector,
#'   and based on [`rope_range()`] if a (Bayesian) model is provided.
#' - a single numeric value (e.g., 0.1), which is used as range around zero
#'   (i.e. the threshold range is set to -0.1 and 0.1, i.e. reflects a symmetric
#'   interval)
#' - a numeric vector of length two (e.g., `c(-0.2, 0.1)`), useful for
#'   asymmetric intervals
#' - a list of numeric vectors, where each vector corresponds to a parameter
#' - a list of *named* numeric vectors, where names correspond to parameter
#'   names. In this case, all parameters that have no matching name in `threshold`
#'   will be set to `"default"`.
#' @inheritParams rope
#' @inheritParams hdi
#'
#' @return Values between 0 and 1 corresponding to the probability of practical significance (ps).
#'
#' @details `p_significance()` returns the proportion of a probability
#'   distribution (`x`) that is outside a certain range (the negligible
#'   effect, or ROPE, see argument `threshold`). If there are values of the
#'   distribution both below and above the ROPE, `p_significance()` returns
#'   the higher probability of a value being outside the ROPE. Typically, this
#'   value should be larger than 0.5 to indicate practical significance. However,
#'   if the range of the negligible effect is rather large compared to the
#'   range of the probability distribution `x`, `p_significance()`
#'   will be less than 0.5, which indicates no clear practical significance.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examplesIf require("rstanarm")
#' library(bayestestR)
#'
#' # Simulate a posterior distribution of mean 1 and SD 1
#' # ----------------------------------------------------
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' p_significance(posterior)
#'
#' # Simulate a dataframe of posterior distributions
#' # -----------------------------------------------
#' df <- data.frame(replicate(4, rnorm(100)))
#' p_significance(df)
#' \donttest{
#' # rstanarm models
#' # -----------------------------------------------
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl,
#'   data = mtcars,
#'   chains = 2, refresh = 0
#' )
#' p_significance(model)
#' # multiple thresholds - asymmetric, symmetric, default
#' p_significance(model, threshold = list(c(-10, 5), 0.2, "default"))
#' # named thresholds
#' p_significance(model, threshold = list(wt = 0.2, `(Intercept)` = c(-10, 5)))
#' }
#' @export
p_significance <- function(x, ...) {
  UseMethod("p_significance")
}


#' @export
p_significance.default <- function(x, ...) {
  insight::format_error(
    paste0("'p_significance()' is not yet implemented for objects of class '", class(x)[1], "'.")
  )
}


#' @rdname p_significance
#' @export
p_significance.numeric <- function(x, threshold = "default", ...) {
  threshold <- .select_threshold_ps(threshold = threshold)
  out <- p_significance(data.frame(Posterior = x), threshold = threshold)
  attr(out, "data") <- x
  out
}


#' @rdname p_significance
#' @export
p_significance.get_predicted <- function(x,
                                         threshold = "default",
                                         use_iterations = FALSE,
                                         verbose = TRUE,
                                         ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- p_significance(
        as.data.frame(t(attributes(x)$iterations)),
        threshold = threshold,
        verbose = verbose,
        ...
      )
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- p_significance(as.numeric(x),
      threshold = threshold,
      verbose = verbose,
      ...
    )
  }
  out
}


#' @export
#' @rdname p_significance
#' @inheritParams p_direction
p_significance.data.frame <- function(x, threshold = "default", rvar_col = NULL, ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::p_significance
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x))
  }


  threshold <- .select_threshold_ps(threshold = threshold, params = x)
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    ps <- .p_significance(x[, 1], threshold = threshold, ...)
  } else if (is.list(threshold)) {
    # check if list of values contains only valid values
    threshold <- .check_list_range(threshold, x, larger_two = TRUE)
    # apply thresholds to each column
    ps <- mapply(
      function(p, thres) {
        .p_significance(
          p,
          threshold = thres
        )
      },
      x,
      threshold,
      SIMPLIFY = FALSE
    )
  } else {
    ps <- sapply(x, .p_significance, threshold = threshold, simplify = TRUE, ...)
  }

  out <- data.frame(
    Parameter = names(x),
    ps = as.numeric(ps),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- obj_name
  class(out) <- unique(c("p_significance", "see_p_significance", class(out)))

  out
}


#' @export
p_significance.draws <- function(x, threshold = "default", ...) {
  p_significance(.posterior_draws_to_df(x), threshold = threshold, ...)
}

#' @export
p_significance.rvar <- p_significance.draws


#' @export
p_significance.parameters_simulate_model <- function(x, threshold = "default", ...) {
  obj_name <- attr(x, "object_name")
  if (!is.null(obj_name)) {
    # first try, parent frame
    model <- .safe(get(obj_name, envir = parent.frame()))

    if (is.null(model)) {
      # second try, global env
      model <- .safe(get(obj_name, envir = globalenv()))
    }
  }
  threshold <- .select_threshold_ps(model = model, threshold = threshold)
  out <- p_significance.data.frame(x, threshold = threshold)
  attr(out, "object_name") <- obj_name
  out
}


#' @export
p_significance.MCMCglmm <- function(x, threshold = "default", ...) {
  nF <- x$Fixed$nfl
  out <- p_significance(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), threshold = threshold, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_significance.BFBayesFactor <- function(x, threshold = "default", ...) {
  out <- p_significance(insight::get_parameters(x), threshold = threshold, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_significance.mcmc <- function(x, threshold = "default", ...) {
  p_significance(as.data.frame(x), threshold = threshold, ...)
}


#' @export
p_significance.bamlss <- function(x, threshold = "default", component = c("all", "conditional", "location"), ...) {
  out <- p_significance(insight::get_parameters(x, component = component), threshold = threshold, ...)
  out <- .add_clean_parameters_attribute(out, x)
  out
}


#' @export
p_significance.bcplm <- function(x, threshold = "default", ...) {
  p_significance(insight::get_parameters(x), threshold = threshold, ...)
}

#' @export
p_significance.mcmc.list <- p_significance.bcplm

#' @export
p_significance.bayesQR <- p_significance.bcplm

#' @export
p_significance.blrm <- p_significance.bcplm

#' @export
p_significance.BGGM <- p_significance.bcplm


#' @export
p_significance.emmGrid <- function(x, threshold = "default", ...) {
  xdf <- insight::get_parameters(x)
  out <- p_significance(xdf, threshold = threshold, ...)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_significance.emm_list <- p_significance.emmGrid

#' @export
p_significance.slopes <- function(x, threshold = "default", ...) {
  xrvar <- .get_marginaleffects_draws(x)
  out <- p_significance(xrvar, threshold = threshold, ...)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_significance.comparisons <- p_significance.slopes

#' @export
p_significance.predictions <- p_significance.slopes


#' @rdname p_significance
#' @export
p_significance.stanreg <- function(x,
                                   threshold = "default",
                                   effects = c("fixed", "random", "all"),
                                   component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), # nolint
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  params <- insight::get_parameters(x, effects = effects, component = component, parameters = parameters)

  threshold <- .select_threshold_ps(
    model = x,
    threshold = threshold,
    params = params,
    verbose = verbose
  )
  result <- p_significance(params, threshold = threshold)

  cleaned_parameters <- insight::clean_parameters(x)
  out <- .prepare_output(result, cleaned_parameters, inherits(x, "stanmvreg"))

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- class(result)

  out
}

#' @export
p_significance.stanfit <- p_significance.stanreg

#' @export
p_significance.blavaan <- p_significance.stanreg


#' @rdname p_significance
#' @export
p_significance.brmsfit <- function(x,
                                   threshold = "default",
                                   effects = c("fixed", "random", "all"),
                                   component = c("conditional", "zi", "zero_inflated", "all"),
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  params <- insight::get_parameters(x, effects = effects, component = component, parameters = parameters)

  threshold <- .select_threshold_ps(
    model = x,
    threshold = threshold,
    params = params,
    verbose = verbose
  )
  result <- p_significance(params, threshold = threshold)

  cleaned_parameters <- insight::clean_parameters(x)
  out <- .prepare_output(result, cleaned_parameters)

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- class(result)

  out
}

.p_significance <- function(x, threshold, ...) {
  if (length(threshold) == 1) {
    psig <- max(
      c(
        length(x[x > abs(threshold)]) / length(x), # ps positive
        length(x[x < -abs(threshold)]) / length(x) # ps negative
      )
    )
  } else {
    psig <- max(
      c(
        length(x[x > threshold[2]]) / length(x), # ps positive
        length(x[x < threshold[1]]) / length(x) # ps negative
      )
    )
  }

  psig
}

# methods ---------------------------

#' @rdname as.numeric.p_direction
#' @export
as.numeric.p_significance <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    as.numeric(as.vector(x$ps))
  } else {
    as.vector(x)
  }
}


#' @method as.double p_significance
#' @export
as.double.p_significance <- as.numeric.p_significance



# helpers --------------------------

#' @keywords internal
.select_threshold_ps <- function(model = NULL, threshold = "default", params = NULL, verbose = TRUE) {
  if (is.list(threshold)) {
    # if we have named elements, complete list
    if (!is.null(params)) {
      named_threshold <- names(threshold)
      if (!is.null(named_threshold)) {
        # find out which name belongs to which parameter
        pos <- match(named_threshold, colnames(params))
        # if not all element names were found, error
        if (anyNA(pos)) {
          insight::format_error(paste(
            "Not all elements of `threshold` were found in the parameters. Please check following names:",
            toString(named_threshold[is.na(pos)])
          ))
        }
        # now "fill" non-specified elements with "default"
        out <- as.list(rep("default", ncol(params)))
        out[pos] <- threshold
        # overwrite former threshold
        threshold <- out
      }
    }
    lapply(threshold, function(i) {
      out <- .select_threshold_list(model = model, threshold = i, verbose = verbose)
      if (length(out) == 1) {
        out <- c(-1 * abs(out), abs(out))
      }
      out
    })
  } else {
    .select_threshold_list(model = model, threshold = threshold, verbose = verbose)
  }
}

#' @keywords internal
.select_threshold_list <- function(model = NULL, threshold = "default", verbose = TRUE) {
  # If default
  if (all(threshold == "default")) {
    if (is.null(model)) {
      threshold <- 0.1
    } else {
      threshold <- rope_range(model, verbose = verbose)[2]
    }
  } else if (!is.list(threshold) && (!all(is.numeric(threshold)) || length(threshold) > 2)) {
    insight::format_error(
      "`threshold` should be one of the following values:",
      "- \"default\", in which case the threshold is based on `rope_range()`",
      "- a single numeric value (e.g., 0.1), which is used as range around zero (i.e. the threshold range is set to -0.1 and 0.1)", # nolint
      "- a numeric vector of length two (e.g., `c(-0.2, 0.1)`)"
    )
  }
  threshold
}

.check_list_range <- function(range, params, larger_two = FALSE) {
  # if we have named elements, complete list
  named_range <- names(range)
  if (!is.null(named_range)) {
    # find out which name belongs to which parameter
    pos <- match(named_range, colnames(params))
    # if not all element names were found, error
    if (anyNA(pos)) {
      insight::format_error(paste(
        "Not all elements of `range` were found in the parameters. Please check following names:",
        toString(named_range[is.na(pos)])
      ))
    }
    # now "fill" non-specified elements with "default"
    out <- as.list(rep("default", ncol(params)))
    out[pos] <- range
    # overwrite former range
    range <- out
  }
  if (length(range) != ncol(params)) {
    insight::format_error("Length of `range` (i.e. number of ROPE limits) should match the number of parameters.")
  }
  # check if list of values contains only valid values
  checks <- vapply(range, function(r) {
    if (larger_two) {
      !all(r == "default") || !all(is.numeric(r)) || length(r) > 2
    } else {
      !all(r == "default") || !all(is.numeric(r)) || length(r) != 2
    }
  }, logical(1))
  if (!all(checks)) {
    insight::format_error("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }
  range
}
