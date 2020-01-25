#' Maximum HDI level inside/outside ROPE (MHDIOR)
#'
#' The MHDIOR (pronounced 'em-eich-dior') is an exploratory and non-validated index representing the maximum percentage of \link[=hdi]{HDI} that does not contain (or is entirely contained, in which case the value is prefixed with a negative sign), in the negligible values space defined by the \link[=rope]{ROPE}. It differs from the ROPE percentage, \emph{i.e.}, from the proportion of a given CI in the ROPE, as it represents the maximum CI values needed to reach a ROPE proportion of 0\% or 100\%. Whether the index reflects the ROPE reaching 0\% or 100\% is indicated through the sign: a negative sign is added to indicate that the probability corresponds to the probability of a not significant effect (a percentage in ROPE of 100\%). For instance, a MHDIOR of 97\% means that there is a probability of .97 that a parameter (described by its posterior distribution) is outside the ROPE. In other words, the 97\% HDI is the maximum HDI level for which the percentage in ROPE is 0\%. On the contrary, a ROPE-based p of -97\% indicates that there is a probability of .97 that the parameter is inside the ROPE (percentage in ROPE of 100\%). A value close to 0\% would indicate that the mode of the distribution falls perfectly at the edge of the ROPE, in which case the percentage of HDI needed to be on either side of the ROPE becomes infinitely small. Negative values do not refer to negative values \emph{per se}, simply indicating that the value corresponds to non-significance rather than significance.
#'
#'
#' @inheritParams rope
#' @param precision The precision by which to explore the ROPE space (in percentage). Lower values increase the precision of the returned p value but can be quite computationaly costly.
#'
#' @inheritParams hdi
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#'
#' # precision = 1 is used to speed up examples...
#'
#' mhdior(
#'   x = rnorm(1000, mean = 1, sd = 1),
#'   range = c(-0.1, 0.1),
#'   precision = 1
#' )
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' mhdior(df, precision = 1)
#'
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' mhdior(model, precision = 1)
#'
#' library(emmeans)
#' mhdior(emtrends(model, ~1, "wt"))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' mhdior(model)
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' mhdior(bf)
#' }
#'
#' @importFrom stats na.omit
#' @export
mhdior <- function(x, ...) {
  UseMethod("mhdior")
}






#' @rdname mhdior
#' @export
mhdior.numeric <- function(x, range = "default", precision = .1, ...) {

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
      p <- 1
    } else {
      p <- -1
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
    p <- as.numeric(unlist(p)) / 100
    p <- h0 * p
    # p <- 1/p  # Convert to probability
  }

  class(p) <- c("mhdior", class(p))
  p
}





#' @rdname mhdior
#' @export
mhdior.data.frame <- function(x, range = "default", precision = .1, ...) {
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    mhdior <- mhdior(x[, 1], range = range, precision = precision, ...)
  } else {
    mhdior <- sapply(x, mhdior, range = range, precision = precision, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "mhdior" = mhdior,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(out) <- c("mhdior", class(out))
  out
}

#' @rdname mhdior
#' @export
mhdior.emmGrid <- function(x, range = "default", precision = .1, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }
  xdf <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(x, names = FALSE)))

  out <- mhdior(xdf, range = range, precision = precision, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @rdname mhdior
#' @export
mhdior.BFBayesFactor <- function(x, range = "default", precision = .1, ...) {
  out <- mhdior(insight::get_parameters(x), range = range, precision = precision, ...)
  out
}


#' @importFrom insight get_parameters
#' @keywords internal
.mhdior_models <- function(x, range, precision, effects, component, parameters, ...) {
  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  out <- .prepare_output(
    mhdior(insight::get_parameters(x, effects = effects, parameters = parameters), range = range, precision = precision, ...),
    insight::clean_parameters(x)
  )

  class(out) <- unique(c("mhdior", class(out)))
  out
}




#' @rdname mhdior
#' @export
mhdior.stanreg <- function(x, range = "default", precision = .1, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- .mhdior_models(
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

#' @rdname mhdior
#' @export
mhdior.brmsfit <- function(x, range = "default", precision = .1, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .mhdior_models(
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
#' @method as.numeric mhdior
#' @export
as.numeric.mhdior <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$mhdior)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double mhdior
#' @export
as.double.mhdior <- as.numeric.mhdior
