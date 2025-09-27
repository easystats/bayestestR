#' @keywords internal
.clean_up_tmp_stanreg <- function(tmp, group, cols, parms) {
  tmp$Group <- group
  tmp$Parameter <- rep(parms, each = nrow(tmp) / length(parms))
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  # tmp$Parameter <- gsub("b\\[(.*) (.*)\\]", "\\2", tmp$Parameter)
  tmp
}


#' @keywords internal
.clean_up_tmp_brms <- function(tmp, group, component, cols, parms) {
  tmp$Group <- group
  tmp$Component <- component
  tmp$Parameter <- rep(parms, each = nrow(tmp) / length(parms))
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  # tmp$Parameter <- gsub("r_(.*)\\.(.*)\\.", "\\1", tmp$Parameter)
  tmp
}


# .get_cleaned_parameters -------------------------------------------------

#' @keywords internal
.get_cleaned_parameters <- function(x, ...) {
  dots <- list(...)
  if ("cleaned_parameters" %in% names(dots)) {
    return(dots$cleaned_parameters)
  }

  UseMethod(".get_cleaned_parameters")
}

#' @keywords internal
.get_cleaned_parameters.default <- function(x, ...) {
  insight::clean_parameters(x)
}

#' @keywords internal
.get_cleaned_parameters.stanfit <- function(x, ...) {
  NULL
}

.get_cleaned_parameters.CmdStanFit <- .get_cleaned_parameters.stanfit



