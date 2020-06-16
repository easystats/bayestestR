#' Practical Significance (ps)
#'
#' Compute the probability of \strong{Practical Significance} (\strong{\emph{ps}}), which can be conceptualized as a unidirectional equivalence test. It returns the probability that effect is above a given threshold corresponding to a negligible effect in the median's direction. Mathematically, it is defined as the proportion of the posterior distribution of the median sign above the threshold.
#'
#' @inheritParams rope
#' @param threshold The threshold value that separates significant from negligible effect. If \code{"default"}, the range is set to \code{0.1} if input is a vector, and based on \code{\link[=rope_range]{rope_range()}} if a Bayesian model is provided.
#'
#' @return Values between 0 and 1 corresponding to the probability of practical significance (ps).
#'
#' @details \code{p_significance()} returns the proportion of a probability
#'   distribution (\code{x}) that is outside a certain range (the negligible
#'   effect, or ROPE, see argument \code{threshold}). If there are values of the
#'   distribution both below and above the ROPE, \code{p_significance()} returns
#'   the higher probability of a value being outside the ROPE. Typically, this
#'   value should be larger than 0.5 to indicate practical significance. However,
#'   if the range of the negligible effect is rather large compared to the
#'   range of the probability distribution \code{x}, \code{p_significance()}
#'   will be less than 0.5, which indicates no clear practical significance.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
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
#'
#' \dontrun{
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




#' @rdname p_significance
#' @export
p_significance.numeric <- function(x, threshold = "default", ...) {
  threshold <- .select_threshold_ps(x = x, threshold = threshold)

  psig <- max(
    c(
      length(x[x > abs(threshold)]) / length(x), # ps positive
      length(x[x < -abs(threshold)]) / length(x) # ps negative
    )
  )

  attr(psig, "threshold") <- threshold
  attr(psig, "data") <- x

  class(psig) <- unique(c("p_significance", "see_p_significance", class(psig)))

  psig
}


#' @export
p_significance.data.frame <- function(x, threshold = "default", ...) {
  obj_name <- .safe_deparse(substitute(x))
  threshold <- .select_threshold_ps(x = x, threshold = threshold)
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    ps <- p_significance(x[, 1], threshold = threshold, ...)
  } else {
    ps <- sapply(x, p_significance, threshold = threshold, simplify = TRUE, ...)
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
p_significance.parameters_simulate_model <- function(x, threshold = "default", ...) {
  obj_name <- attr(x, "object_name")
  if (!is.null(obj_name)) {
    # first try, parent frame
    model <- tryCatch({
      get(obj_name, envir = parent.frame())
    },
    error = function(e) { NULL }
    )

    if (is.null(model)) {
      # second try, global env
      model <- tryCatch({
        get(obj_name, envir = globalenv())
      },
      error = function(e) { NULL }
      )
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
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @export
p_significance.BFBayesFactor <- function(x, threshold = "default", ...) {
  out <- p_significance(insight::get_parameters(x), threshold = threshold, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @export
p_significance.mcmc <- function(x, threshold = "default", ...) {
  p_significance(as.data.frame(x), threshold = threshold, ...)
}


#' @export
p_significance.bcplm <- function(x, threshold = "default", ...) {
  p_significance(insight::get_parameters(x), threshold = threshold, ...)
}


#' @rdname p_significance
#' @export
p_significance.emmGrid <- function(x, threshold = "default", ...) {
  xdf <- .clean_emmeans_draws(x)
  out <- p_significance(xdf, threshold = threshold, ...)

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}







#' @rdname p_significance
#' @export
p_significance.stanreg <- function(x, threshold = "default", effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  threshold <- .select_threshold_ps(model = x, threshold = threshold)

  data <- p_significance(
    insight::get_parameters(x, effects = effects, parameters = parameters),
    threshold = threshold
  )

  out <- .prepare_output(data, insight::clean_parameters(x))

  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- class(data)

  out
}

#' @export
p_significance.stanfit <- p_significance.stanreg





#' @rdname p_significance
#' @export
p_significance.brmsfit <- function(x, threshold = "default", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  threshold <- .select_threshold_ps(model = x, threshold = threshold)

  data <- p_significance(
    insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    threshold = threshold
  )

  out <- .prepare_output(data, insight::clean_parameters(x))

  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- class(data)

  out
}





#' @rdname as.numeric.p_direction
#' @export
as.numeric.p_significance <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$ps)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_significance
#' @export
as.double.p_significance <- as.numeric.p_significance



#' @keywords internal
.select_threshold_ps <- function(x = NULL, model = NULL, threshold = "default") {
  # If a range is passed
  if (length(threshold) > 1) {
    if (length(unique(abs(threshold))) == 1) { # If symmetric range
      threshold <- abs(threshold[2])
    } else{
      stop("`threshold` should be 'default' or a numeric value (e.g., 0.1).")
    }
  }
  # If default
  if (all(threshold == "default")) {
    if (!is.null(model)) {
      threshold <- rope_range(model)[2]
    } else {
      threshold <- 0.1
    }
  } else if (!all(is.numeric(threshold))) {
    stop("`threshold` should be 'default' or a numeric value (e.g., 0.1).")
  }
  threshold
}
