#' Practical Significance (ps)
#'
#' Compute the probability of **Practical Significance** (***ps***), which can be conceptualized as a unidirectional equivalence test. It returns the probability that effect is above a given threshold corresponding to a negligible effect in the median's direction. Mathematically, it is defined as the proportion of the posterior distribution of the median sign above the threshold.
#'
#' @inheritParams rope
#' @param threshold The threshold value that separates significant from negligible effect. If `"default"`, the range is set to `0.1` if input is a vector, and based on [`rope_range()`][rope_range] if a Bayesian model is provided.
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
#' @examples
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
#' if (require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt + cyl,
#'     data = mtcars,
#'     chains = 2, refresh = 0
#'   )
#'   p_significance(model)
#' }
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
  out <- p_significance(data.frame(x = x), threshold = threshold)
  out[[1]] <- NULL
  attr(out, "data") <- x
  out
}


#' @export
p_significance.data.frame <- function(x, threshold = "default", ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))
  threshold <- .select_threshold_ps(threshold = threshold)
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    ps <- .p_significance(x[, 1], threshold = threshold, ...)
  } else {
    ps <- sapply(x, .p_significance, threshold = threshold, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "ps" = as.numeric(ps),
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_significance.emm_list <- p_significance.emmGrid



#' @rdname p_significance
#' @export
p_significance.stanreg <- function(x,
                                   threshold = "default",
                                   effects = c("fixed", "random", "all"),
                                   component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                                   parameters = NULL,
                                   verbose = TRUE,
                                   ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  threshold <- .select_threshold_ps(model = x, threshold = threshold, verbose = verbose)

  data <- p_significance(
    insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    threshold = threshold
  )

  cleaned_parameters <- insight::clean_parameters(x)
  out <- .prepare_output(data, cleaned_parameters, inherits(x, "stanmvreg"))

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- class(data)

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
  threshold <- .select_threshold_ps(model = x, threshold = threshold, verbose = verbose)

  data <- p_significance(
    insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    threshold = threshold
  )

  cleaned_parameters <- insight::clean_parameters(x)
  out <- .prepare_output(data, cleaned_parameters)

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- class(data)

  out
}

.p_significance <- function(x, threshold, ...) {
  psig <- max(
    c(
      length(x[x > abs(threshold)]) / length(x), # ps positive
      length(x[x < -abs(threshold)]) / length(x) # ps negative
    )
  )

  psig
}

# methods ---------------------------

#' @rdname as.numeric.p_direction
#' @export
as.numeric.p_significance <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    return(as.numeric(as.vector(x$ps)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_significance
#' @export
as.double.p_significance <- as.numeric.p_significance



# helpers --------------------------

#' @keywords internal
.select_threshold_ps <- function(model = NULL, threshold = "default", verbose = TRUE) {
  # If a range is passed
  if (length(threshold) > 1) {
    if (length(unique(abs(threshold))) == 1) {
      # If symmetric range
      threshold <- abs(threshold[2])
    } else {
      insight::format_error("`threshold` should be 'default' or a numeric value (e.g., 0.1).")
    }
  }
  # If default
  if (all(threshold == "default")) {
    if (!is.null(model)) {
      threshold <- rope_range(model, verbose = verbose)[2]
    } else {
      threshold <- 0.1
    }
  } else if (!all(is.numeric(threshold))) {
    insight::format_error("`threshold` should be 'default' or a numeric value (e.g., 0.1).")
  }
  threshold
}
