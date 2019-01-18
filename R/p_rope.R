#' ROPE-based p-value
#'
#' The ROPE-based p-value represents the maximum Credible Interval (\link[=hdi]{HDI}) that does not contain (positive values) or is entirely contained (negative values) in the negligible values space defined by the \link[=rope]{ROPE}. A ROPE-based p of 97% means that there is a probability of .97 that a parameter (desccribed by its posterior distribution) is outside the ROPE.
#'
#' @param posterior vector representing a posterior distribution.
#' @param bounds ROPE's lower and higher bounds.
#' @param precision The precision by which to explore the ROPE space. Lower values increase the precision of the returned p value but can be quite computationnaly costly.
#'
#' @examples
#' library(bayestestR)
#'
#' p_rope(posterior=rnorm(1000, mean = 1, sd = 1), bounds = c(-0.1, 0.1))

#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats na.omit
#' @export
p_rope <- function(posterior, bounds = c(-0.1, 0.1), precision=0.1) {

  rope_values <- rope(posterior, bounds, CI=seq(0, 100, by=precision), verbose=FALSE)
  rope_values <- na.omit(unlist(rope_values))

  if(all(rope_values == min(rope_values))){
    if(rope_values[1] == 0){
      return(100)
    } else{
      return(-100)
    }
  }

  min_rope <- min(rope_values)
  if(rope_values[1] == min_rope){
    name_min2 <- names(rope_values[rope_values != min_rope][1])
    CI_position <- match(name_min2, names(rope_values))-1
    if(CI_position>1) CI_position <- CI_position-1
    p <- names(rope_values[CI_position])
    h0 <- 1
  } else{
    name_max <- names(rope_values[rope_values != max(rope_values)][1])
    CI_position <- match(name_max, names(rope_values))
    if(CI_position>1) CI_position <- CI_position-1
    p <- names(rope_values[CI_position])
    h0 <- -1
  }

  p <- as.numeric(unlist(strsplit(p, "CI_", fixed=TRUE))[2])
  p <- h0 * p
  # p <- 1/p  # Convert to probability
  return(p)
}
