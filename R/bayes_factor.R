#' Compute Bayes Factor (BF)
#'
#' Compute likelihood ratio between two competing hypotheses
#'
#' @param x Vector representing a posterior distribution
#' @param method The method to be used. Currently only \code{"savage-dickey"} is supported
#' @param ... Arguments passed to the specific methods
#' @param prior Vector representing a prior distribution
#' @param null Value to be tested
#' @param direction Test type. One of \code{-1} (left tailed), \code{0} (defult; two tailed) or \code{1} (right tailed)
#'
#' @references
#' Wagenmakers, E. J., Lodewyckx, T., Kuriyal, H., & Grasman, R. (2010). Bayesian
#' hypothesis testing for psychologists: A tutorial on the Savage-Dickey method.
#' Cognitive psychology, 60(3), 158-189.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @export
bayes_factor <- function(x,method = "savage-dickey",...){
  UseMethod("bayes_factor")
}

#' @describeIn bayes_factor alias
#' @export
bf <- function(x,...) {
  bayes_factor(x,...)
}

#' @rdname bayes_factor
#' @export
bayes_factor.numeric <- function(x,method = "savage-dickey",...){
  method <- match.arg(method)
  if (method=="savage-dickey") {
    bf_val <- savage_dickey_bf(x,...)
  } else {
    stop("Other methods not supported yet.")
  }

  return(bf_val)
}

#' @rdname bayes_factor
#' @export
#' @import logspline
savage_dickey_bf <- function(x,prior,null = 0,direction = 0){
  if (requireNamespace("logspline")) {
    f_post <- suppressWarnings(logspline(x))
    f_prior <- suppressWarnings(logspline(prior))

    d_post <- dlogspline(null,f_post)
    d_prior <- dlogspline(null,f_prior)

    norm_post <- norm_prior <- 1
    if (direction < 0) {
      norm_post <- plogspline(null,f_post)
      norm_prior <- plogspline(null,f_prior)
    } else if (direction > 0) {
      norm_post <- 1-plogspline(null,f_post)
      norm_prior <- 1-plogspline(null,f_prior)
    }
  } else {
    warning('Consider installing the logspline package for more robust estimate')
    d_post <- density_at(x,null)
    d_prior <- density_at(prior,null)

    norm_post <- norm_prior <- 1
    if (direction < 0) {
      norm_post <- mean(x < null)
      norm_prior <- mean(prior < null)
    } else if (direction > 0) {
      norm_post <- 1 - mean(x < 0)
      norm_prior <- 1 - mean(prior < null)
    }
  }

  bf_val <- (d_prior/norm_prior)/(d_post/norm_post)
  class(bf_val) <- c('bf_val',class(bf_val))
  attr(bf_val,'method') <- 'savage-dickey'
  return(bf_val)
}

#' @export
print.bf_val <- function(x,digits = 2, logBF = FALSE) {
  method <- attr(x,'method')

  df <- data.frame(BF = x,
                   row.names = names(x))
  colnames(df) <- "Bayes Factor"

  if (logBF) {
    df[[1]] <- log(df[[1]])
    colnames(df) <- "Bayes Factor (log)"
  }

  print.data.frame(df,digits = digits)
  cat("---\n")
  cat(paste0("Method: ", method,'\n'))

  return(invisible(x))
}