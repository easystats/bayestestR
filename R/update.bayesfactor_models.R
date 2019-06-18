#' Update bayesfactor_models
#'
#' @param object A \code{\link{bayesfactor_models}} object.
#' @param subset Vector of model indices to keep or remove.
#' @param reference Index of model to rereference to, or \code{"top"} to reference to the best model, or \code{"bottom"} to reference to the worst model.
#' @param ... Currently not used.
#'
#' @examples
#' library(lme4)
#' lmer1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' lmer2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
#' lmer3 <- lmer(
#'   Sepal.Length ~ Petal.Length + (Petal.Length | Species) + (1 | Petal.Width),
#'   data = iris
#' )
#'
#' m <- bayesfactor_models(lmer1, lmer2, lmer3, denominator = 1)
#' m
#'
#' update(m, reference = "bottom")
#' @export
update.bayesfactor_models <- function(object, subset = NULL, reference = NULL, ...) {
  if (!is.null(reference)) {
    if (reference == "top") {
      reference <- which.max(object$BF)
    } else if (reference == "bottom") {
      reference <- which.min(object$BF)
    }
    object$BF <- object$BF / object$BF[reference]
    attr(object, "denominator") <- reference
  }

  denominator <- attr(object, "denominator")

  if (!is.null(subset)) {
    object_subset <- object[subset, ]

    if (denominator %in% subset) {
      attr(object_subset, "denominator") <- which(denominator == subset)
    } else {
      object_subset <- rbind(object[denominator, ], object_subset)
      attr(object_subset, "denominator") <- 1
    }
    object <- object_subset
  }
  object
}
