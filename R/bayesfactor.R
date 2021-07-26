#' Bayes Factors (BF)
#'
#' This function compte the Bayes factors (BFs) that are appropriate to the
#' input. For vectors or single models, it will compute [`BFs for single
#' parameters()`][bayesfactor_parameters], or is `hypothesis` is specified,
#' [`BFs for restricted models()`][bayesfactor_restricted]. For multiple models,
#' it will return the BF corresponding to [`comparison between
#' models()`][bayesfactor_models] and if a model comparison is passed, it will
#' compute the [`inclusion BF()`][bayesfactor_inclusion].
#' \cr\cr
#' For a complete overview of these functions, read the [Bayes factor vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).
#'
#' @param ... A numeric vector, model object(s), or the output from
#'   `bayesfactor_models`.
#' @inheritParams bayesfactor_parameters
#' @inheritParams bayesfactor_restricted
#' @inheritParams bayesfactor_models
#' @inheritParams bayesfactor_inclusion
#'
#' @return Some type of Bayes factor, depending on the input. See [bayesfactor_parameters()], [bayesfactor_models()] or [bayesfactor_inclusion()]
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(bayestestR)
#'
#' if (require("logspline")) {
#'   prior <- distribution_normal(1000, mean = 0, sd = 1)
#'   posterior <- distribution_normal(1000, mean = .5, sd = .3)
#'
#'   bayesfactor(posterior, prior = prior)
#' }
#' \dontrun{
#' # rstanarm models
#' # ---------------
#' if (require("rstanarm")) {
#'   model <- stan_lmer(extra ~ group + (1 | ID), data = sleep)
#'   bayesfactor(model)
#' }
#' }
#'
#' if (require("logspline")) {
#'   # Frequentist models
#'   # ---------------
#'   m0 <- lm(extra ~ 1, data = sleep)
#'   m1 <- lm(extra ~ group, data = sleep)
#'   m2 <- lm(extra ~ group + ID, data = sleep)
#'
#'   comparison <- bayesfactor(m0, m1, m2)
#'   comparison
#'
#'   bayesfactor(comparison)
#' }
#' @export
bayesfactor <- function(...,
                        prior = NULL,
                        direction = "two-sided",
                        null = 0,
                        hypothesis = NULL,
                        effects = c("fixed", "random", "all"),
                        verbose = TRUE,
                        denominator = 1,
                        match_models = FALSE,
                        prior_odds = NULL) {
  mods <- list(...)
  effects <- match.arg(effects)

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
  } else if (!is.null(hypothesis)) {
    bayesfactor_restricted(...,
      prior = prior,
      verbose = verbose,
      effects = effects
    )
  } else {
    bayesfactor_parameters(
      ...,
      prior = prior,
      direction = direction,
      null = null,
      effects = effects,
      verbose = verbose
    )
  }
}
