#' Convert BIC indices to Bayes Factors via the BIC-approximation method.
#'
#' @param bic A vector of BIC values.
#' @param denominator The BIC value to use as a denominator (to test against).
#'
#' @examples
#' bic1 <- BIC(lm(Sepal.Length ~ 1, data = iris))
#' bic2 <- BIC(lm(Sepal.Length ~ Species, data = iris))
#' bic3 <- BIC(lm(Sepal.Length ~ Species + Petal.Length, data = iris))
#' bic4 <- BIC(lm(Sepal.Length ~ Species * Petal.Length, data = iris))
#'
#' bic_to_bf(c(bic1, bic2, bic3, bic4), denominator = bic1)
#'
#' @return The Bayes Factors corresponding to the BIC values against the denominator.
#'
#' @export
bic_to_bf <- function(bic, denominator) {
  exp((bic - denominator) / (-2))
}
