#' ROPE-based p-value
#'
#' The ROPE-based p-value represents the maximum percentage of \link[=hdi]{HDI} that does not contain (positive values) or is entirely contained (negative values) in the negligible values space defined by the \link[=rope]{ROPE}. It differs from the ROPE, i.e., the proportion of a given CI in the ROPE, by representing the maximum CI to reach a ROPE proportion of 0\% (positive values) or 100\% (negative values). A ROPE-based p of 97\% means that there is a probability of .97 that a parameter (described by its posterior distribution) is outside the ROPE. On the contrary, a ROPE-based p of -97\% means that there is also a probability of 0.97 that the parameter is inside the ROPE.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a `stanreg` or `brmsfit` model.
#' @param bounds ROPE's lower and higher bounds. Shoudd be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{c(0.1, 0.1)} if input is a vector and \code{x +- 0.1*SD(response)} if a Bayesian model is provided.
#' @param precision The precision by which to explore the ROPE space (in percentage). Lower values increase the precision of the returned p value but can be quite computationaly costly.
#'
#' @examples
#' library(bayestestR)
#'
#' p_rope(posterior = rnorm(1000, mean = 1, sd = 1), bounds = c(-0.1, 0.1))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_rope(model)
#'
#' # Will fail until get_predictors is implemented.
#' # library(brms)
#' # model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' # p_rope(model)
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats na.omit
#' @export
p_rope <- function(posterior, bounds = "default", precision = .1) {
  UseMethod("p_rope")
}


#' @export
print.p_rope <- function(x, ...) {
  cat(sprintf("p (ROPE) = %.2f%%", x))
}


#' @export
p_rope.numeric <- function(posterior, bounds = "default", precision = .1) {

  # This implementation is very clunky

  if (all(bounds == "default")) {
    bounds <- c(-0.1, 0.1)
  } else if (!all(is.numeric(bounds)) | length(bounds) != 2) {
    stop("bounds should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }



  rope_df <- rope(posterior, bounds, ci = seq(0, 1, by = precision / 100), verbose = FALSE)
  rope_df <- na.omit(rope_df)

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
    p <- h0 * p * 100
    # p <- 1/p  # Convert to probability
  }

  class(p) <- c("p_rope", class(p))
  return(p)
}




#' @importFrom insight find_parameters get_parameters
#' @importFrom stats sd
#' @keywords internal
.p_rope_models <- function(posterior, bounds = "default", precision = .1) {
  if (all(bounds == "default")) {
    bounds <- c(-0.1 * sd(insight::get_response(posterior)), 0.1 * sd(insight::get_response(posterior)))
  } else if (!all(is.numeric(bounds)) | length(bounds) != 2) {
    stop("bounds should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  out <- data.frame(
    "Parameter" = insight::find_parameters(posterior),
    "p_ROPE" = sapply(insight::get_parameters(posterior), bounds = bounds, p_rope, precision = precision, simplify = TRUE),
    row.names = NULL
  )
  return(out)
}

#' @export
p_rope.stanreg <- .p_rope_models

#' @export
p_rope.brmsfit <- .p_rope_models
