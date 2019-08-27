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



#' @keywords internal
.compute_interval_simMerMod <- function(x, ci, effects, parameters, verbose, fun) {
  fixed <- fixed.data <- NULL
  random <- random.data <- NULL

  if (effects %in% c("fixed", "all")) {
    fixed.data <- as.data.frame(x@fixef)
    fixed <- .compute_interval_dataframe(fixed.data, ci, verbose, fun)
    fixed$Group <- "fixed"
  }

  if (effects %in% c("random", "all")) {
    random.data <- .prepare_ranef_armsim(x)
    random <- .compute_interval_dataframe(random.data, ci, verbose, fun)
    random$Group <- "random"
  }

  d <- do.call(rbind, list(fixed, random))

  if (!is.null(parameters)) {
    keep <- grepl(pattern = parameters, x = d$Parameter, perl = TRUE)
    if (any(keep)) d <- d[keep, ]

    if (!is.null(fixed.data)) {
      keep <- grepl(pattern = parameters, x = colnames(fixed.data), perl = TRUE)
      if (any(keep)) fixed.data <- fixed.data[, keep, drop = FALSE]
    }

    if (!is.null(random.data)) {
      keep <- grepl(pattern = parameters, x = colnames(random.data), perl = TRUE)
      if (any(keep)) random.data <- random.data[, keep, drop = FALSE]
    }
  }

  if (length(unique(d$Group)) == 1) {
    d <- .remove_column(d, "Group")
  }

  list(result = d, data = do.call(cbind, .compact_list(list(fixed.data, random.data))))
}



#' @keywords internal
.compute_interval_sim <- function(x, ci, parameters, verbose, fun) {
  d <- fixed.data <- NULL

  fixed.data <- as.data.frame(x@coef)
  d <- .compute_interval_dataframe(fixed.data, ci, verbose, fun)

  if (!is.null(parameters)) {
    keep <- grepl(pattern = parameters, x = d$Parameter, perl = TRUE)
    if (any(keep)) d <- d[keep, ]
  }

  list(result = d, data = fixed.data)
}



#' @keywords internal
.prepare_ranef_armsim <- function(x) {
  re <- x@ranef
  f <- data.frame()

  for (i in 1:length(re)) {
    dn <- dimnames(re[[i]])[[2]]
    l <- lapply(1:length(dn), function(j) {
      d <- as.data.frame(re[[i]][, j, ])
      colnames(d) <- sprintf("%s.%s", colnames(d), dn[j])
      d
    })
    if (ncol(f) == 0)
      f <- do.call(cbind, l)
    else
      f <- cbind(f, do.call(cbind, l))
  }

  f
}
