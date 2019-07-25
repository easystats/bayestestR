#' Reshape CI between wide/long formats
#'
#' Reshape CI between wide/long formats.
#'
#' @param x A data.frame containing \code{CI_low} and \code{CI_high}.
#'
#' @examples
#' library(bayestestR)
#'
#' x <- data.frame(replicate(4, rnorm(100)))
#' x <- ci(x, ci = c(0.68, 0.89, 0.95))
#' reshape_ci(x)
#' reshape_ci(reshape_ci(x))
#'
#' x <- data.frame(replicate(4, rnorm(100)))
#' x <- describe_posterior(x, ci = c(0.68, 0.89, 0.95))
#' reshape_ci(x)
#' reshape_ci(reshape_ci(x))
#' @export
reshape_ci <- function(x){
  # Long to wide ----------------
  if("CI_low" %in% names(x) & "CI_high" %in% names(x) & "CI" %in% names(x)){

    ci_position <- which(names(x) == "CI")

    # Reshape
    if(length(unique(x$CI)) > 1){
      if(!"Parameter" %in% names(x)) x$Parameter <- as.character(1:nrow(x))
      x <- reshape(x,
                   idvar = "Parameter",
                   timevar = "CI",
                   direction="wide",
                   v.names=c("CI_low", "CI_high"),
                   sep = "_")
      row.names(x) <- NULL
    }

    # Replace at the right place
    ci_colname <- names(x)[c(grepl("CI_low_*", names(x)) | grepl("CI_high_*", names(x)))]
    x <- cbind(x[names(x)[0:(ci_position-1)][!names(x)[0:(ci_position-1)] %in% ci_colname]],
               x[ci_colname],
               x[names(x)[ci_position:(length(names(x))-1)][!names(x)[ci_position:(length(names(x))-1)] %in% ci_colname]])

    # Wide to long --------------
  } else{
    lows <- grepl("CI_low_*", names(x))
    highs <- grepl("CI_high_*", names(x))
    ci <- as.numeric(gsub("CI_low_", "", names(x)[lows]))
    if(paste0(ci, collapse="-") != paste0(gsub("CI_high_", "", names(x)[highs]), collapse="-")){
      stop("Something went wrong in the CIs reshaping.")
      return(x)
    }
    if(sum(lows) > 1 & sum(highs) > 1){
      low <- reshape(x[!highs],
                     direction="long",
                     varying=list(names(x)[lows]),
                     sep = "_",
                     timevar="CI",
                     v.names="CI_low",
                     times = ci)
      high <- reshape(x[!lows],
                      direction="long",
                      varying=list(names(x)[highs]),
                      sep = "_",
                      timevar="CI",
                      v.names="CI_high",
                      times = ci)
      x <- merge(low, high)
      x$id <- NULL
      x <- x[order(x$Parameter),]
      row.names(x) <- NULL
    }

    # Replace at the right place
    ci_position <- which(lows)[1]
    ci_colname <- c("CI", "CI_low", "CI_high")
    x <- cbind(x[names(x)[0:(ci_position-1)][!names(x)[0:(ci_position-1)] %in% ci_colname]],
               x[ci_colname],
               x[names(x)[ci_position:(length(names(x))-1)][!names(x)[ci_position:(length(names(x))-1)] %in% ci_colname]])

  }
  x
}
