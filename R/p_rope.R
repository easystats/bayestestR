#' ROPE-based p-value
#'
#' The ROPE-based p-value represents the maximum percentage of \link[=hdi]{HDI} that does not contain (positive values) or is entirely contained (negative values) in the negligible values space defined by the \link[=rope]{ROPE}. It differs from the ROPE, i.e., the proportion of a given CI in the ROPE, by representing the maximum CI to reach a ROPE proportion of 0\% (positive values) or 100\% (negative values). A ROPE-based p of 97\% means that there is a probability of .97 that a parameter (described by its posterior distribution) is outside the ROPE. On the contrary, a ROPE-based p of -97\% means that there is also a probability of 0.97 that the parameter is inside the ROPE.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a `stanreg` or `brmsfit` model.
#' @param bounds ROPE's lower and higher bounds.
#' @param precision The precision by which to explore the ROPE space. Lower values increase the precision of the returned p value but can be quite computationaly costly.
#'
#' @examples
#' library(bayestestR)
#'
#' p_rope(posterior = rnorm(1000, mean = 1, sd = 1), bounds = c(-0.1, 0.1))
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_rope(model)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_rope(model)
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats na.omit
#' @export
p_rope <- function(posterior, bounds = c(-0.1, 0.1), precision = 0.1) {
  UseMethod("p_rope")
}


#' @export
p_rope.numeric <- function(posterior, bounds = c(-0.1, 0.1), precision = 0.1) {
  rope_values <- rope(posterior, bounds, CI = seq(0, 100, by = precision), verbose = FALSE)
  rope_values <- rope_values[!is.na(rope_values)]
  rope_values <- sapply(rope_values, as.numeric)

  if (all(rope_values == min(rope_values))) {
    if (rope_values[1] == 0) {
      return(100)
    } else {
      return(-100)
    }
  }

  min_rope <- min(rope_values)
  if (rope_values[1] == min_rope) {
    name_min2 <- names(rope_values[rope_values != min_rope][1])
    CI_position <- match(name_min2, names(rope_values)) - 1
    if (CI_position > 1) CI_position <- CI_position - 1
    p <- names(rope_values[CI_position])
    h0 <- 1
  } else {
    name_max <- names(rope_values[rope_values != max(rope_values)][1])
    CI_position <- match(name_max, names(rope_values))
    if (CI_position > 1) CI_position <- CI_position - 1
    p <- names(rope_values[CI_position])
    h0 <- -1
  }

  p <- as.numeric(unlist(strsplit(p, "CI_", fixed = TRUE))[2])
  p <- h0 * p
  # p <- 1/p  # Convert to probability
  return(p)
}


#' @export
p_rope.stanreg <- function(posterior, bounds = c(-0.1, 0.1), precision = 0.1) {
  return(sapply(as.data.frame(posterior), p_rope, bounds = bounds, precision = precision, simplify = FALSE))
}

#' @export
p_rope.brmsfit <- function(posterior, bounds = c(-0.1, 0.1), precision = 0.1) {
  return(sapply(as.data.frame(posterior), p_rope, bounds = bounds, precision = precision, simplify = FALSE))
}
