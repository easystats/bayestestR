#' Probability of Direction (p)
#'
#' Compute the Probability of Direction (p, also known as the Maximum Probability of Effect - MPE), a Bayesian equivalent of the p-value (altough differently expressed). It varies between 50\% and 100\% and can be interpreted as the probability that a parameter (described by its posterior distribution) is positive or negative (following  the median's sign). It is defined as the proportion of the posterior distribution of the median's sign. It is used as an index of effect existence, i.e., whether the probability that the effect is in the same direction than the point-estimate (independently of the effect's size or significance). This p-value is fairly similar to its frequentist counterpart (i.e., is strongly correlated).
#'
#' @param posterior vector representing a posterior distribution.
#'
#'
#' @examples
#' library(bayestestR)
#' 
#' # Simulate a posterior distribution of mean 1 and SD 1
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' # Compute the p
#' p_direction(posterior)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
p_direction <- function(posterior) {
  p_direction <- 100 * max(
    c(
      length(posterior[posterior > 0]) / length(posterior), # pd positive
      length(posterior[posterior < 0]) / length(posterior) # pd negative
    )
  )
  return(p_direction)
}
