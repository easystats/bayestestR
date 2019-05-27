#' Bayes Factors (BF)
#'
#' This function compte the Bayes factors (BFs) that are appropriate to the input.
#' For vectors or single models, it will compute \code{\link[=bayesfactor_savagedickey]{Savage-Dickey's BFs}}. For multiple models, it will return the BF corresponding to \code{\link[=bayesfactor_models]{comparison between models}} and if a comparison is passed, it will compute the \code{\link[=bayesfactor_inclusion]{inclusion BF}}.
#' \cr\cr
#' For a complete overview of these functions, read the \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{Bayes factor vignette}.
#'
#' @param ... A numeric vector, model object(s), or the output from \code{bayesfactor_models}.
#' @inheritParams bayesfactor_savagedickey
#' @inheritParams bayesfactor_models
#' @inheritParams bayesfactor_inclusion
#'
#' @return Some type of Bayes factor, depending on the input. See \code{\link{bayesfactor_savagedickey}}, \code{\link{bayesfactor_models}} or \code{\link{bayesfactor_inclusion}}
#'
#'
#' @examples
#' library(bayestestR)
#'
#' # Vectors
#' prior <- distribution_normal(1000, mean = 0, sd = 1)
#' posterior <- distribution_normal(1000, mean = .5, sd = .3)
#'
#' bayesfactor(posterior, prior = prior)
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' library(rstanarm)
#' model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#' bayesfactor(model)
#' }
#'
#' # Frequentist models
#' # ---------------
#' m0 <- lm(extra ~ 1, data = sleep)
#' m1 <- lm(extra ~ group, data = sleep)
#' m2 <- lm(extra ~ group + ID, data = sleep)
#'
#' comparison <- bayesfactor(m0, m1, m2)
#' comparison
#'
#' bayesfactor(comparison)
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
    } else if (inherits(mods[[1]], "bayesfactor_models")) {
      bayesfactor_inclusion(..., match_models = match_models, prior_odds = prior_odds)
    } else if (inherits(mods[[1]], "BFBayesFactor")) {
      if (class(mods[[1]]@numerator[[1]]) == "BFlinearModel") {
        bayesfactor_inclusion(..., match_models = match_models, prior_odds = prior_odds)
      } else {
        bayesfactor_models(...)
      }
    } else {
      bayesfactor_savagedickey(...,
        prior = prior, direction = direction,
        hypothesis = hypothesis, effects = effects,
        verbose = verbose
      )
    }
  }
