#' Probability of Direction (pd)
#'
#' Compute the \strong{Probability of Direction} (\strong{pd}, also known as the Maximum Probability of Effect - \emph{MPE}). It varies between 50\% and 100\% and can be interpreted as the probability (expressed in percentage) that a parameter (described by its posterior distribution) is strictly positive or negative (whichever is the most probable). It is mathematically defined as the proportion of the posterior distribution that is of the median's sign. Altough differently expressed, this index is fairly similar (\emph{i.e.}, is strongly correlated) to the frequentist \strong{p-value}.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @inheritParams hdi
#'
#' @details \strong{Relationship with the p-value}: In most cases, it seems that the \emph{pd} corresponds to the frequentist one-sided \emph{p}-value through the formula `p-value = (1-pd/100)`. Thus, a `pd` of `95\%`, `97.5\%` `99.5\%` and `99.95\%` corresponds approximately to a two-sided \emph{p}-value of respectively `.1`, `.05`, `.01` and `.001`. See the \href{https://easystats.github.io/bayestestR/articles/4_Guidelines.html}{\emph{reporting guidelines}}.
#'
#' @examples
#' library(bayestestR)
#' 
#' # Simulate a posterior distribution of mean 1 and SD 1
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' p_direction(posterior)
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_direction(model)
#' 
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_direction(model)
#' }
#' 
#' @export
p_direction <- function(posterior, ...) {
  UseMethod("p_direction")
}


#' @export
print.p_direction <- function(x, ...) {
  cat(sprintf("pd = %.2f%%", x))
}


#' @rdname p_direction
#' @export
p_direction.numeric <- function(posterior, ...) {
  p_direction <- 100 * max(
    c(
      length(posterior[posterior > 0]) / length(posterior), # pd positive
      length(posterior[posterior < 0]) / length(posterior) # pd negative
    )
  )

  class(p_direction) <- c("p_direction", class(p_direction))
  p_direction
}


#' @importFrom insight get_parameters
#' @keywords internal
.p_direction_models <- function(posterior, effects, component, parameters) {
  data.frame(
    "Parameter" = .get_parameter_names(posterior, effects = effects, component = component, parameters = parameters),
    "pd" = sapply(insight::get_parameters(posterior, effects = effects, component = component, parameters = parameters), p_direction, simplify = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' @rdname p_direction
#' @export
p_direction.stanreg <- function(posterior, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  .p_direction_models(
    posterior = posterior,
    effects = effects,
    component = "conditional",
    parameters = parameters
  )
}

#' @rdname p_direction
#' @export
p_direction.brmsfit <- function(posterior, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .p_direction_models(
    posterior = posterior,
    effects = effects,
    component = component,
    parameters = parameters
  )
}
