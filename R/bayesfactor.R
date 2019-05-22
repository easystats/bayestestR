#' Bayes Factors (BF)
#'
#' This is a wrapper function for See \code{\link{bayesfactor_savagedickey}}, \code{\link{bayesfactor_models}}
#' and \code{\link{bayesfactor_inclusion}}.
#' \cr\cr
#' For a complete overview of these functions, read the \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{Bayes factor vignette}.
#'
#' @param ... A numeric vector, model object(s), or the output from \code{bayesfactor_models}.
#' @inheritParams bayesfactor_savagedickey
#' @inheritParams bayesfactor_models
#' @inheritParams bayesfactor_inclusion
#'
#' @return Some type of Bayes factor, depending on the input. See \code{\link{bayesfactor_savagedickey}}, \code{\link{bayesfactor_models}} or \code{\link{bayesfactor_inclusion}}
#' @export
bayesfactor <-
  function(...,
           prior = NULL,
           direction = "two-sided",
           hypothesis = 0,
           effects = c("fixed", "random", "all"),
           verbose = TRUE,
           denominator = 1,
           match_models = FALSE,
           prior_odds = NULL) {
    mods <- list(...)

    if (length(mods) > 1) {
      bayesfactor_models(..., denominator = denominator)
    } else if (is(mods[[1]],"bayesfactor_models") || is(mods[[1]],"BFBayesFactor")) {
      bayesfactor_inclusion(..., match_models = match_models, prior_odds = prior_odds)
    } else {
      bayesfactor_savagedickey(...,prior = prior, direction = direction,
                               hypothesis = hypothesis, effects = effects,
                               verbose = verbose)
    }
  }