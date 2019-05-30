#' ROPE-based p-value
#'
#' Compute the ROPE-based p-value, an exploratory index representing the maximum percentage of \link[=hdi]{HDI} that does not contain (positive values) or is entirely contained (negative values) in the negligible values space defined by the \link[=rope]{ROPE}. It differs from the ROPE percentage, \emph{i.e.}, from the proportion of a given CI in the ROPE, as it represents the maximum CI to reach a ROPE proportion of 0\% (positive values) or 100\% (negative values). A ROPE-based \emph{p} of 97\% means that there is a probability of .97 that a parameter (described by its posterior distribution) is outside the ROPE. On the contrary, a ROPE-based p of -97\% means that there is a probability of .97 that the parameter is inside the ROPE.
#'
#'
#' @param range ROPE's lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the range is set to \code{c(0.1, 0.1)} if input is a vector and \code{x +- 0.1*SD(response)} if a Bayesian model is provided.
#' @param precision The precision by which to explore the ROPE space (in percentage). Lower values increase the precision of the returned p value but can be quite computationaly costly.
#'
#' @inheritParams hdi
#'
#' @examples
#' library(bayestestR)
#'
#' # precision = 1 is used to speed up examples...
#'
#' p_rope(
#'   x = rnorm(1000, mean = 1, sd = 1),
#'   range = c(-0.1, 0.1),
#'   precision = 1
#' )
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' p_rope(df, precision = 1)
#'
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#' p_rope(model, precision = 1)
#' \dontrun{
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_rope(model)
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' p_rope(bf)
#' }
#'
#' @importFrom stats na.omit
#' @export
p_rope <- function(x, ...) {
  UseMethod("p_rope")
}






#' @rdname p_rope
#' @export
p_rope.numeric <- function(x, range = "default", precision = .1, ...) {

  # This implementation is very clunky

  if (all(range == "default")) {
    range <- c(-0.1, 0.1)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }


  rope_df <- rope(x, range, ci = seq(0, 1, by = precision / 100), verbose = FALSE)
  rope_df <- stats::na.omit(rope_df)

  rope_values <- rope_df$ROPE_Percentage

  if (all(rope_values == min(rope_values))) {
    if (rope_values[1] == 0) {
      p <- 100
    } else {
      p <- -100
    }
  } else {
    min_rope <- min(rope_values)
    if (rope_values[1] == min_rope) {
      name_min2 <- rope_df$CI[rope_values != min_rope][1]
      CI_position <- match(name_min2, rope_df$CI) - 1
      if (CI_position > 1) CI_position <- CI_position - 1
      h0 <- 1
    } else {
      name_max <- rope_df$CI[rope_values != max(rope_values)][1]
      CI_position <- match(name_max, rope_df$CI)
      if (CI_position > 1) CI_position <- CI_position - 1
      h0 <- -1
    }
    p <- rope_df$CI[CI_position]
    p <- as.numeric(unlist(p))
    p <- h0 * p
    # p <- 1/p  # Convert to probability
  }

  class(p) <- c("p_rope", class(p))
  p
}





#' @rdname p_rope
#' @export
p_rope.data.frame <- function(x, range = "default", precision = .1, ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    p_ROPE <- p_rope(x[, 1], range = range, precision = precision, ...)
  } else {
    p_ROPE <- sapply(x, p_rope, range = range, precision = precision, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "p_ROPE" = p_ROPE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(out) <- c("p_rope", class(out))
  out
}

#' @rdname p_rope
#' @export
p_rope.BFBayesFactor <- function(x, range = "default", precision = .1, ...) {
  out <- p_rope(insight::get_parameters(x), range = range, precision = precision, ...)
  out
}


#' @importFrom insight get_parameters
#' @keywords internal
.p_rope_models <- function(x, range, precision, effects, component, parameters, ...) {
  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  out <- p_rope(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), range = range, precision = precision, ...)
  out$Parameter <- .get_parameter_names(x, effects = effects, component = component, parameters = parameters)

  out
}




#' @rdname p_rope
#' @export
p_rope.stanreg <- function(x, range = "default", precision = .1, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .p_rope_models(
    x = x,
    range = range,
    precision = precision,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @rdname p_rope
#' @export
p_rope.brmsfit <- function(x, range = "default", precision = .1, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .p_rope_models(
    x = x,
    range = range,
    precision = precision,
    effects = effects,
    component = component,
    parameters = parameters,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}






#' @rdname as.numeric.p_direction
#' @method as.numeric p_rope
#' @export
as.numeric.p_rope <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$p_ROPE)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_rope
#' @export
as.double.p_rope <- as.numeric.p_rope
