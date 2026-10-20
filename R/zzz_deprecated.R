#' Shortest Probability Interval (SPI)
#'
#' @description
#' `spi()` is deprecated. Use `hdi(method = "density")` instead, which returns
#' the same interval. See [hdi()] for how the interval is computed.
#'
#' @param x Vector representing a posterior distribution, or a data frame of such
#'   vectors. Can also be a Bayesian model. See [hdi()].
#' @param ... Arguments passed to [hdi()].
#'
#' @inherit ci return
#'
#' @references
#' Liu, Y., Gelman, A., & Zheng, T. (2015). Simulation-efficient shortest probability intervals. Statistics and Computing, 25(4), 809–819. https://doi.org/10.1007/s11222-015-9563-8
#'
#' @examplesIf require("quadprog")
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' # instead of spi(posterior), use:
#' hdi(posterior, method = "density")
#'
#' @export
spi <- function(x, ...) {
  .Deprecated(
    new = "hdi",
    package = "bayestestR",
    msg = "`spi()` is deprecated. Use `hdi(method = \"density\")` instead."
  )
  cl <- match.call()
  cl[[1]] <- quote(bayestestR::hdi)
  cl$method <- "density"
  eval.parent(cl)
}
