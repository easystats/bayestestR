#' @keywords internal
.check_ci_argument <- function(x, ci, verbose = TRUE) {
  if (ci > 1) {
    if (verbose) {
      warning("`ci` should be less than 1, returning NAs.")
    }
    return(data.frame(
      "CI" = ci,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  if (ci == 1) {
    return(data.frame(
      "CI" = ci,
      "CI_low" = min(x, na.rm = TRUE),
      "CI_high" = max(x, na.rm = TRUE)
    ))
  }

  if (anyNA(x)) {
    if (verbose) {
      warning("The posterior contains NAs, returning NAs.")
    }
    return(data.frame(
      "CI" = ci,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  if (length(x) < 3) {
    if (verbose) {
      warning("The posterior is too short, returning NAs.")
    }
    return(data.frame(
      "CI" = ci,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  NULL
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
    fixed.data <- insight::get_parameters(x, effects = "fixed", parameters = parameters)
    fixed <- .compute_interval_dataframe(fixed.data, ci, verbose, fun)
    fixed$Group <- "fixed"
  }

  if (effects %in% c("random", "all")) {
    random.data <- insight::get_parameters(x, effects = "random", parameters = parameters)
    random <- .compute_interval_dataframe(random.data, ci, verbose, fun)
    random$Group <- "random"
  }

  d <- do.call(rbind, list(fixed, random))

  if (length(unique(d$Group)) == 1) {
    d <- .remove_column(d, "Group")
  }

  list(result = d, data = do.call(cbind, .compact_list(list(fixed.data, random.data))))
}



#' @keywords internal
.compute_interval_sim <- function(x, ci, parameters, verbose, fun) {
  fixed.data <- insight::get_parameters(x, parameters = parameters)
  d <- .compute_interval_dataframe(fixed.data, ci, verbose, fun)
  list(result = d, data = fixed.data)
}
