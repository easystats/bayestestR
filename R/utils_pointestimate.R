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
  dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))

  dat <- switch(
    effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  dat <- switch(
    component,
    conditional = .select_rows(dat, "Component", "conditional"),
    zi = ,
    zero_inflated = .select_rows(dat, "Component", "zero_inflated"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- .remove_column(dat, "Group")
  }

  if (all(dat$Component == dat$Component[1])) {
    dat <- .remove_column(dat, "Component")
  }

  dat
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

  dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))

  dat <- switch(
    effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- .remove_column(dat, "Group")
  }

  dat
}
