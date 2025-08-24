#' @keywords internal
.append_datagrid <- function(results, object, long = FALSE) {
  UseMethod(".append_datagrid", object = object)
}


#' @keywords internal
.append_datagrid.emmGrid <- function(results, object, long = FALSE) {
  # results is assumed to be a data frame with "Parameter" column
  # object is an emmeans / marginaleffects that results is based on

  all_attrs <- attributes(results) # save attributes for later
  all_class <- class(results)

  # extract model info. if we have categorical, add "group" variable
  if (inherits(object, c("emmGrid", "emm_list"))) {
    model <- attributes(object)$model
  } else {
    insight::check_if_installed("marginaleffects")
    model <- marginaleffects::components(object, "model")
  }
  if (!long && !is.null(model)) {
    m_info <- insight::model_info(model, response = 1, verbose = FALSE)
    # check if we have ordinal and alike
    if (!is.null(m_info)) {
      has_response_levels <- isTRUE(
        m_info$is_categorical |
          m_info$is_mixture |
          m_info$is_ordinal |
          m_info$is_multinomial |
          m_info$is_cumulative
      )
    } else {
      has_response_levels <- FALSE
    }

    if ((has_response_levels || isTRUE(insight::is_multivariate(model))) && "group" %in% colnames(object)) {
      results <- .safe(
        cbind(data.frame(group = object$group), results),
        results
      )
    }
  }

  datagrid <- insight::get_datagrid(object)
  grid_names <- colnames(datagrid)

  if (long || nrow(datagrid) < nrow(results)) {
    datagrid$Parameter <- unique(results$Parameter)
    results <- datawizard::data_merge(datagrid, results, by = "Parameter")
    results$Parameter <- NULL
    class(results) <- all_class
  } else {
    results[colnames(datagrid)] <- datagrid
    results$Parameter <- NULL
    results <- results[, c(grid_names, setdiff(colnames(results), grid_names)), drop = FALSE]

    # add back attributes
    most_attrs <- all_attrs[setdiff(names(all_attrs), names(attributes(datagrid)))]
    attributes(results)[names(most_attrs)] <- most_attrs
  }

  attr(results, "idvars") <- grid_names
  results
}


#' @keywords internal
.append_datagrid.emm_list <- .append_datagrid.emmGrid


#' @keywords internal
.append_datagrid.slopes <- .append_datagrid.emmGrid


#' @keywords internal
.append_datagrid.predictions <- .append_datagrid.emmGrid


#' @keywords internal
.append_datagrid.comparisons <- .append_datagrid.emmGrid


#' @keywords internal
.append_datagrid.data.frame <- function(results, object, long = FALSE) {
  # results is assumed to be a data frame with "Parameter" column
  # object is a data frame with an rvar column that results is based on

  all_attrs <- attributes(results) # save attributes for later
  all_class <- class(results)

  is_rvar <- vapply(object, inherits, FUN.VALUE = logical(1), "rvar")
  grid_names <- colnames(object)[!is_rvar]
  datagrid <- data.frame(object[, grid_names, drop = FALSE])

  if (long || nrow(datagrid) < nrow(results)) {
    datagrid$Parameter <- unique(results$Parameter)
    results <- datawizard::data_merge(datagrid, results, by = "Parameter")
    results$Parameter <- NULL
    class(results) <- all_class
  } else {
    results[grid_names] <- object[grid_names]
    results$Parameter <- NULL
    results <- results[, c(grid_names, setdiff(colnames(results), grid_names)), drop = FALSE]

    # add back attributes
    most_attrs <- all_attrs[setdiff(names(all_attrs), names(attributes(object)))]
    attributes(results)[names(most_attrs)] <- most_attrs
  }

  attr(results, "idvars") <- grid_names
  results
}
