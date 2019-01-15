#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion of a posterior distribution that lies within a region of practical equivalence.
#'
#' @param posterior Posterior Distribution.
#' @param bounds Rope lower and higher bounds.
#' @param CI The credible interval to use.
#' @param overlap Compute rope overlap (EXPERIMENTAL).
#'
#'
#' @return list containing rope indices
#'
#' @examples
#' library(psycho)
#' 
#' posterior <- rnorm(1000, 0, 0.01)
#' results <- rope(posterior)
#' results$decision
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
rope <- function(posterior, bounds = c(-0.1, 0.1), CI = 95, overlap = FALSE) {


  # Basic rope --------------------------------------------------------------


  HDI_area <- HDI(posterior, CI / 100)
  HDI_area <- posterior[dplyr::between(
    posterior,
    HDI_area$values$HDImin,
    HDI_area$values$HDImax
  )]

  area_within <- HDI_area[dplyr::between(HDI_area, bounds[1], bounds[2])]
  area_outside <- HDI_area[!dplyr::between(HDI_area, bounds[1], bounds[2])]

  p_within <- length(area_within) / length(posterior)
  p_outside <- length(area_outside) / length(posterior)

  rope_decision <- ifelse(p_within == 0, "Accept",
    ifelse(p_outside == 0, "Reject", "Undecided")
  )



  # Rope Overlap ------------------------------------------------------------
  if (overlap == TRUE) {
    sd <- abs(bounds[1] - bounds[2]) / 2
    sd <- sd / 3
    norm <- rnorm_perfect(length(posterior), 0, sd)
    rope_overlap <- overlap(posterior, norm) * 100
    output <- list(rope_decision = rope_decision, rope_probability = p_within, rope_overlap = rope_overlap)
  } else {
    output <- list(rope_decision = rope_decision, rope_probability = p_within)
  }



  return(output)
}
