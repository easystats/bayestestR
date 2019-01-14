#' Compute Maximum Probability of Effect (MPE).
#'
#' Compute the Maximum Probability of Effect (MPE), i.e., the proportion of posterior distribution that is of the same sign as the median. In other words, it corresponds to the maximum probability that the effect is different from 0 in the median’s direction.
#'
#' @param posterior Posterior Distribution.
#'
#' @return list containing the MPE and its values.
#'
#' @examples
#' library(psycho)
#' library(rstanarm)
#' 
#' fit <- rstanarm::stan_glm(rating ~ advance, data = attitude)
#' posterior <- psycho::analyze(fit)$values$effects$advance$posterior
#' mpe <- psycho::mpe(posterior)
#' print(mpe$MPE)
#' print(mpe$values)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
mpe <- function(posterior) {
  median <- median(posterior)
  if (median >= 0) {
    MPE <- length(posterior[posterior >= 0]) / length(posterior) * 100
    if (MPE == 100) {
      MPE_values <- c(min(posterior), max(posterior))
    } else {
      MPE_values <- c(0, max(posterior))
    }
  } else {
    MPE <- length(posterior[posterior < 0]) / length(posterior) * 100
    if (MPE == 100) {
      MPE_values <- c(min(posterior), max(posterior))
    } else {
      MPE_values <- c(min(posterior), 0)
    }
  }

  MPE <- list(MPE = MPE, values = MPE_values)
  return(MPE)
}
