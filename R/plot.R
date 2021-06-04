#' @export
plot.equivalence_test <- function(x, ...) {
  insight::check_if_installed("see", "to plot results from equivalence-test")
  NextMethod()
}


#' @export
plot.p_direction <- function(x, ...) {
  insight::check_if_installed("see", "to plot results from p_direction()")
  NextMethod()
}


#' @export
plot.point_estimate <- function(x, ...) {
  insight::check_if_installed("see", "to plot point-estimates")
  NextMethod()
}


#' @export
plot.map_estimate <- function(x, ...) {
  insight::check_if_installed("see", "to plot point-estimates")
  NextMethod()
}


#' @export
plot.rope <- function(x, ...) {
  insight::check_if_installed("see", "to plot ROPE")
  NextMethod()
}


#' @export
plot.bayestestR_hdi <- function(x, ...) {
  insight::check_if_installed("see", "to plot HDI")
  NextMethod()
}


#' @export
plot.bayestestR_eti <- function(x, ...) {
  insight::check_if_installed("see", "to plot credible intervals")
  NextMethod()
}

#' @export
plot.bayestestR_si <- function(x, ...) {
  insight::check_if_installed("see", "to plot support intervals")
  NextMethod()
}

#' @export
plot.bayesfactor_parameters <- function(x, ...) {
  insight::check_if_installed("see", "to plot Savage-Dickey Bayes factor")
  NextMethod()
}

#' @export
plot.bayesfactor_models <- function(x, ...) {
  insight::check_if_installed("see", "to plot models' Bayes factors")
  NextMethod()
}

#' @export
plot.estimate_density <- function(x, ...) {
  insight::check_if_installed("see", "to plot densities")
  NextMethod()
}

#' @export
plot.estimate_density_df <- function(x, ...) {
  insight::check_if_installed("see", "to plot models' densities")
  NextMethod()
}

#' @export
plot.p_significance <- function(x, ...) {
  insight::check_if_installed("see", "to plot practical significance")
  NextMethod()
}
