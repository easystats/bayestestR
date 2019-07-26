#' @keywords internal
.check_ci_argument <- function(x, ci, verbose = TRUE) {
  if (ci > 1) {
    if (verbose) {
      warning("`ci` should be less than 1, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  if (ci == 1) {
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = min(x, na.rm = TRUE),
      "CI_high" = max(x, na.rm = TRUE)
    ))
  }

  if (anyNA(x)) {
    if (verbose) {
      warning("The posterior contains NAs, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  if (length(x) < 3) {
    if (verbose) {
      warning("The posterior is too short, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  NULL
}




#' @importFrom insight get_parameters
#' @keywords internal
.compute_interval_stanreg <- function(x, ci, effects, parameters, verbose, fun) {
  list <- lapply(c("fixed", "random"), function(.x) {
    parms <- insight::get_parameters(x, effects = .x, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      get(fun, asNamespace("bayestestR")),
      ci = ci,
      verbose = verbose,
      simplify = FALSE
    ))

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_stanreg(
        tmp,
        group = .x,
        cols = c("CI", "CI_low", "CI_high", "Group"),
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

  class(dat) <- unique(c(paste0("bayestestR_", fun), paste0("see_", fun), class(dat)))
  dat
}




#' @importFrom insight get_parameters
#' @keywords internal
.compute_interval_brmsfit <- function(x, ci, effects, component, parameters, verbose, fun) {
  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  .get_hdi <- function(.x, .y) {
    parms <- insight::get_parameters(x, effects = .x, component = .y, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      get(fun, asNamespace("bayestestR")),
      ci = ci,
      verbose = verbose,
      simplify = FALSE
    ))

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_brms(
        tmp,
        group = .x,
        component = .y,
        cols = c("CI", "CI_low", "CI_high", "Component", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    tmp
  }

  list <- mapply(.get_hdi, eff, com, SIMPLIFY = FALSE)
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

  class(dat) <- unique(c(paste0("bayestestR_", fun), paste0("see_", fun), class(dat)))
  dat
}



#' @keywords internal
.compute_interval_dataframe <- function(x, ci, verbose, fun) {
  numeric_variables <- sapply(x, is.numeric, simplify = TRUE)

  out <- .compact_list(lapply(
    x[, numeric_variables, drop = FALSE],
    get(fun, asNamespace("bayestestR")),
    ci = ci,
    verbose = verbose
  ))

  dat <- data.frame(
    Parameter = rep(names(out), each = length(ci)),
    do.call(rbind, out),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  class(dat) <- unique(c(paste0("bayestestR_", fun), paste0("see_", fun), class(dat)))
  dat
}






