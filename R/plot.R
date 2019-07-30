#' @export
plot.equivalence_test <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from equivalence-test. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.p_direction <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from p_direction(). Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.point_estimate <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot point-estimates. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.map_estimate <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot point-estimates. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.rope <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot ROPE. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.hdi <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot HDI. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.eti <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot credible intervals. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}

#' @export
plot.bayesfactor_parameters <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot Savage-Dickey Bayes factor. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}

#' @export
plot.bayesfactor_models <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot models' Bayes factors. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}

#' @export
plot.estimate_density <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot densities. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}
