#' @keywords internal
.clean_emmeans_draws <- function(x, ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }

  draws <- emmeans::as.mcmc.emmGrid(
    x,
    names = FALSE,
    sep.chains = FALSE
  )
  data.frame(draws, check.names = FALSE)
}