#' @keywords internal
.clean_emmeans_draws <- function(x, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }

  if(!is.null(attributes(x)$misc$predict.type)){
    x <- emmeans::regrid(x, transform=attributes(x)$misc$predict.type, ...)
  }

  draws <- emmeans::as.mcmc.emmGrid(
    x,
    names = FALSE,
    sep.chains = FALSE,
    NE.include = TRUE,
    ...
  )
  data.frame(draws, check.names = FALSE)
}
