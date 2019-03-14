#' Probability of Direction (pd)
#'
#' Compute the Probability of Direction (pd, also known as the Maximum Probability of Effect - MPE). It varies between 50\% and 100\% and can be interpreted as the probability (expressed in percentage) that a parameter (described by its posterior distribution) is strictly positive or negative (consistently with the median's sign). It is defined as the proportion of the posterior distribution that is of the median's sign. Altough differently expressed, this index is fairly similar to the frequentist p-value (i.e., is strongly correlated).
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#'
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
p_direction <- function(posterior) {
  UseMethod("p_direction")
}


#' @export
print.p_direction <- function(x, ...) {
  cat(sprintf("pd = %.2f%%", x))
}


#' @export
p_direction.numeric <- function(posterior) {
  p_direction <- 100 * max(
    c(
      length(posterior[posterior > 0]) / length(posterior), # pd positive
      length(posterior[posterior < 0]) / length(posterior) # pd negative
    )
  )

  class(p_direction) <- c("p_direction", class(p_direction))
  return(p_direction)
}




#' @importFrom insight find_parameters get_parameters
#' @keywords internal
.p_direction_models <- function(posterior) {
  out <- data.frame(
    "Parameter" = insight::find_parameters(posterior)[["conditional"]],
    "pd" = sapply(insight::get_parameters(posterior), p_direction, simplify = TRUE),
    row.names = NULL
  )
  return(out)
}

#' @export
p_direction.stanreg <- .p_direction_models

#' @export
p_direction.brmsfit <- .p_direction_models
