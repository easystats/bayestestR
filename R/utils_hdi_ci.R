#' @keywords internal
.check_hdi_method <- function(method, method_missing, dots) {
  # `ci_method = "spi"` in `...` is the older way to ask for the density
  # method, it is used only if `method` was not given
  if (method_missing && identical(tolower(dots$ci_method), "spi")) {
    method <- "density"
  }
  insight::validate_argument(method, c("empirical", "density"))
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
.compute_interval_dataframe <- function(x, ci, verbose, fun, method = "empirical") {
  numeric_variables <- vapply(x, is.numeric, TRUE)
  ci_fun <- get(fun, asNamespace("bayestestR"))
  # only hdi() has a `method` argument
  is_hdi <- identical(fun, "hdi")

  out <- insight::compact_list(lapply(
    x[, numeric_variables, drop = FALSE],
    function(i) {
      if (is_hdi) {
        ci_fun(i, ci = ci, verbose = verbose, method = method)
      } else {
        ci_fun(i, ci = ci, verbose = verbose)
      }
    }
  ))

  dat <- data.frame(
    Parameter = rep(names(out), each = length(ci)),
    do.call(rbind, out),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # the density method keeps the SPI label when printed
  if (is_hdi && identical(method, "density")) {
    class(dat) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_spi", class(dat)))
  } else {
    class(dat) <- unique(c(paste0("bayestestR_", fun), paste0("see_", fun), class(dat)))
  }

  dat
}


#' @keywords internal
.compute_interval_simMerMod <- function(
  x,
  ci,
  effects,
  parameters,
  verbose,
  fun,
  method = "empirical"
) {
  fixed <- fixed.data <- NULL
  random <- random.data <- NULL

  if (effects %in% c("fixed", "all")) {
    fixed.data <- insight::get_parameters(x, effects = "fixed", parameters = parameters, verbose = verbose)
    fixed <- .compute_interval_dataframe(fixed.data, ci, verbose, fun, method)
    fixed$Group <- "fixed"
  }

  if (effects %in% c("random", "all")) {
    random.data <- insight::get_parameters(x, effects = "random", parameters = parameters, verbose = verbose)
    random <- .compute_interval_dataframe(random.data, ci, verbose, fun, method)
    random$Group <- "random"
  }

  d <- do.call(rbind, list(fixed, random))

  if (length(unique(d$Group)) == 1) {
    d <- datawizard::data_remove(d, "Group", verbose = FALSE)
  }

  list(result = d, data = do.call(cbind, insight::compact_list(list(fixed.data, random.data))))
}


#' @keywords internal
.compute_interval_sim <- function(
  x,
  ci,
  parameters,
  verbose,
  fun,
  method = "empirical"
) {
  fixed.data <- insight::get_parameters(x, parameters = parameters, verbose = verbose)
  d <- .compute_interval_dataframe(fixed.data, ci, verbose, fun, method)
  list(result = d, data = fixed.data)
}
