#' @keywords internal
.check_ci_fun <- function(dots) {
  ci_fun <- "hdi"
  if (identical(dots$ci_method, "spi")) {
    ci_fun <- "spi"
  }
  ci_fun
}


#' @keywords internal
.check_ci_argument <- function(x, ci, verbose = TRUE) {
  if (ci > 1) {
    if (verbose) {
      insight::format_warning("`ci` should be less than 1, returning NAs.")
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


  if (length(x) < 3) {
    if (verbose) {
      insight::format_warning("The posterior is too short, returning NAs.")
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
  numeric_variables <- vapply(x, is.numeric, TRUE)

  out <- insight::compact_list(lapply(
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

  # rename for SPI, should be HDI
  if (identical(fun, "spi")) {
    class(dat) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_spi", class(dat)))
  } else {
    class(dat) <- unique(c(paste0("bayestestR_", fun), paste0("see_", fun), class(dat)))
  }

  dat
}


#' @keywords internal
.compute_interval_simMerMod <- function(x, ci, effects, parameters, verbose, fun) {
  fixed <- fixed.data <- NULL
  random <- random.data <- NULL

  if (effects %in% c("fixed", "all")) {
    fixed.data <- insight::get_parameters(x, effects = "fixed", parameters = parameters, verbose = verbose)
    fixed <- .compute_interval_dataframe(fixed.data, ci, verbose, fun)
    fixed$Group <- "fixed"
  }

  if (effects %in% c("random", "all")) {
    random.data <- insight::get_parameters(x, effects = "random", parameters = parameters, verbose = verbose)
    random <- .compute_interval_dataframe(random.data, ci, verbose, fun)
    random$Group <- "random"
  }

  d <- do.call(rbind, list(fixed, random))

  if (length(unique(d$Group)) == 1) {
    d <- datawizard::data_remove(d, "Group", verbose = FALSE)
  }

  list(result = d, data = do.call(cbind, insight::compact_list(list(fixed.data, random.data))))
}


#' @keywords internal
.compute_interval_sim <- function(x, ci, parameters, verbose, fun) {
  fixed.data <- insight::get_parameters(x, parameters = parameters, verbose = verbose)
  d <- .compute_interval_dataframe(fixed.data, ci, verbose, fun)
  list(result = d, data = fixed.data)
}
