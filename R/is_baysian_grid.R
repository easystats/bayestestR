#' @keywords internal
.is_baysian_grid <- function(x) {
  UseMethod(".is_baysian_grid")
}


#' @keywords internal
.is_baysian_grid.emmGrid <- function(x) {
  if (inherits(x, "emm_list")) {
    x <- x[[1]]
  }
  post.beta <- methods::slot(x, "post.beta")
  !(all(dim(post.beta) == 1) && is.na(post.beta))
}


#' @keywords internal
.is_baysian_grid.emm_list <- .is_baysian_grid.emmGrid


#' @keywords internal
.is_baysian_grid.slopes <- function(x) {
  insight::check_if_installed("marginaleffects")
  !is.null(suppressWarnings(marginaleffects::get_draws(x, "PxD")))
}


#' @keywords internal
.is_baysian_grid.predictions <- .is_baysian_grid.slopes


#' @keywords internal
.is_baysian_grid.comparisons <- .is_baysian_grid.slopes
