#' @export
plot.equivalence_test <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from equivalence-test. Please install it.")
  }
  NextMethod()
}


#' @export
plot.p_direction <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from p_direction(). Please install it.")
  }
  NextMethod()
}


#' @export
plot.rope <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot ROPE. Please install it.")
  }
  NextMethod()
}


#' @export
plot.hdi <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot HDI Please install it.")
  }
  NextMethod()
}


#' @export
plot.ci <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot credible intervals. Please install it.")
  }
  NextMethod()
}

#' @export
plot.bayesfactor_savagedickey <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot Savage-Dickey Bayes factor. Please install it.")
  }
  NextMethod()
}
