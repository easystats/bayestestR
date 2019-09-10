#' @importFrom insight get_parameters
#' @keywords internal
.compute_pointestimate_brmsfit <- function(x, effects, component, parameters, centrality, dispersion) {
  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  .get_point_estimate <- function(.x, .y) {
    parms <- insight::get_parameters(x, effects = .x, component = .y, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      point_estimate,
      centrality = centrality,
      dispersion = dispersion,
      simplify = FALSE
    ))

    if (!.is_empty_object(tmp)) {
      # capitalize first letter
      substr(centrality[1], 1, 1) <- toupper(substr(centrality[1], 1, 1))
      if (centrality == "All")
        cols <- c("Median", "Mean", "MAP", "Component", "Group")
      else
        cols <- c(centrality, "Component", "Group")

      tmp <- .clean_up_tmp_brms(
        tmp,
        group = .x,
        component = .y,
        cols = cols,
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
.compute_pointestimate_stanreg <- function(x, effects, parameters, centrality, dispersion) {
  list <- lapply(c("fixed", "random"), function(.x) {
    parms <- insight::get_parameters(x, effects = .x, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      point_estimate,
      centrality = centrality,
      dispersion = dispersion,
      simplify = FALSE
    ))

    if (!.is_empty_object(tmp)) {
      # capitalize first letter
      substr(centrality[1], 1, 1) <- toupper(substr(centrality[1], 1, 1))
      if (centrality == "All")
        cols <- c("Median", "Mean", "MAP", "Group")
      else
        cols <- c(centrality, "Group")

      tmp <- .clean_up_tmp_stanreg(
        tmp,
        group = .x,
        cols = cols,
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
