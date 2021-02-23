#' Reshape estimations with multiple iterations (draws) to long format
#'
#' Reshape a wide data.frame of iterations (such as posterior draws or bootsrapped samples) as columns to long format. Instead of having all iterations as columns (e.g., \code{iter_1, iter_2, ...}), will return 3 columns with the \code{\*_index} (the previous index of the row), the \code{\*_group} (the iteration number) and the \code{\*_value} (the value of said iteration).
#'
#' @param draws data.frame containing posterior draws obtained from \code{estimate_response} or \code{estimate_link}.
#' @param prefix The prefix of the draws (for instance, \code{"iter_"} for columns named as \code{iter_1, iter_2, iter_3}). If more than one are provided, will search for the first one that matches.
#' @examples
#' \donttest{
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ am, data = mtcars, refresh = 0)
#'   draws <- insight::get_predicted(model)
#'   long_format <- reshape_draws(draws)
#'   head(long_format)
#' }
#' }
#' @return Data frame of reshaped draws in long format.
#' @importFrom stats reshape
#' @export
reshape_draws <- function(draws, prefix = c("draw", "iter", "iteration", "sim")) {

  # Find columns' name
  prefix <- prefix[min(which(sapply(tolower(prefix), function(x) sum(grepl(x, tolower(names(draws)))) > 1)))]

  if (is.na(prefix) || is.null(prefix)) {
    stop("Couldn't find columns corresponding to draws in your dataframe, please specify the correct prefix.")
  }

  # Get column names
  iter_cols <- tolower(names(draws))[grepl(prefix, tolower(names(draws)))]

  # Drop "_" if prefix ends with it
  newname <- ifelse(endsWith(prefix, "_"), substr(prefix, 1, nchar(prefix) - 1), prefix)

  # Create Index column
  draws[[paste0(newname, "_index")]] <- 1:nrow(draws)

  # Reshape
  long <- stats::reshape(draws,
    varying = iter_cols,
    idvar = paste0(newname, "_index"),
    v.names = paste0(newname, "_value"),
    timevar = paste0(newname, "_group"),
    direction = "long"
  )
  row.names(long) <- NULL

  class(long) <- class(long)[which(class(long) == "data.frame"):length(class(long))]
  long
}
