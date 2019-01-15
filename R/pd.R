#' Probability of Direction (pd)
#'
#' Compute the Probability of Direction (pd, also known as the Maximum Probability of Effect - MPE). It varies between 50\% and 100\% and can be interpreted as the probability that a parameter (described by its posterior distribution) is positive or negative (following  the median’s sign). It is defined as the proportion of the posterior distribution of the median's sign.
#'
#' @param posterior vector representing a posterior distribution.
#'
#'
#' @examples
#' library(bayestestR)
#' 
#' # Simulate a posterior distribution of mean 1 and SD 1
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' # Compute the pd
#' pd(posterior)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
pd <- function(posterior) {
  pd <- 100 * max(
    c(
      length(posterior[posterior > 0]) / length(posterior), # PD positive
      length(posterior[posterior < 0]) / length(posterior) # PD negative
    )
  )
  return(pd)
}
