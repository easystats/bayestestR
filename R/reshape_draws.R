#' Reshape estimations with Bayesian posterior draws to long format
#'
#' Reshape a wide data.frame of posterior draws (or iterations) as columns to long format.
#'
#' @param draws data.frame containing posterior draws obtained from \code{estimate_response} or \code{estimate_link}.
#' @param prefix The prefix of the draws (for instance, \code{"iter_"} for columns named as \code{iter_1, iter_2, iter_3}). Case sensitive. If more than one are provided, will search for the first one that matches.
#' @examples
#' \donttest{
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ am, data = mtcars, refresh = 0)
#'   draws <- as.data.frame(insight::get_predicted(model))
#'   reshape_draws(draws)
#' }
#' }
#' @return Data frame of reshaped draws in long format.
#' @importFrom stats reshape
#' @export
reshape_draws <- function(draws, prefix = c("Draw_", "draw_", "iter_")) {

  # Find columns' name
  prefix <- prefix[sapply(prefix, function(x) sum(grepl(x, names(draws))) > 1)][1]
  if (is.na(prefix) || is.null(prefix)) {
    stop("Couldn't find columns corresponding to draws in your dataframe, please specify the correct prefix.")
  }

  # Drop "_" if it ends with it
  newname <- ifelse(endsWith(prefix, "_"), substr(prefix, 1, nchar(prefix) - 1), prefix)

  # Create Index column
  draws$Index <- 1:nrow(draws)

  # Reshape
  long <- stats::reshape(draws,
    varying = names(draws)[grepl(prefix, names(draws))],
    idvar = "Index",
    v.names = "Value",
    timevar = newname,
    direction = "long"
  )
  row.names(long) <- NULL

  long
}
