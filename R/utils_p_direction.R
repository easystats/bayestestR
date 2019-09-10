#' @importFrom insight get_parameters
#' @keywords internal
.compute_pd_brmsfit <- function(x, effects, component, parameters, method, ...) {
  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  .get_point_estimate <- function(.x, .y) {
    parms <- insight::get_parameters(x, effects = .x, component = .y, parameters = parameters)
    tmp <- p_direction(parms, method = method, ...)

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_brms(
        tmp,
        group = .x,
        component = .y,
        cols = c("pd", "Component", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    tmp
  }

  list <- mapply(.get_point_estimate, eff, com, SIMPLIFY = FALSE)

  .select_effects_component(
    do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE)),
    effects = effects,
    component = component
  )
}




#' @importFrom insight get_parameters
#' @keywords internal
.compute_pd_stanreg <- function(x, effects, parameters, method, ...) {
  list <- lapply(c("fixed", "random"), function(.x) {
    parms <- insight::get_parameters(x, effects = .x, parameters = parameters)
    tmp <- p_direction(parms, method = method, ...)

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_stanreg(
        tmp,
        group = .x,
        cols = c("pd", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    tmp
  })

  .select_effects_component(
    do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE)),
    effects = effects,
    component = NULL
  )
}
