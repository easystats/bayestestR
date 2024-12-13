# small wrapper around this commonly used try-catch
.safe <- function(code, on_error = NULL) {
  tryCatch(code, error = function(e) on_error)
}

# select rows where values in "variable" match "value"
#' @keywords internal
.select_rows <- function(data, variable, value) {
  data[which(data[[variable]] == value), ]
}

#' select numerics columns
#' @keywords internal
.select_nums <- function(x) {
  x[unlist(lapply(x, is.numeric))]
}

#' @keywords internal
.retrieve_model <- function(x) {
  # retrieve model
  obj_name <- attr(x, "object_name", exact = TRUE)
  model <- NULL

  if (!is.null(obj_name)) {
    # first try, parent frame
    model <- .safe(get(obj_name, envir = parent.frame()))

    if (is.null(model)) {
      # second try, global env
      model <- .safe(get(obj_name, envir = globalenv()))
    }

    if (is.null(model)) {
      # last try
      model <- .dynGet(obj_name, ifnotfound = NULL)
    }
  }
  model
}

#' @keywords internal
.dynGet <- function(x,
                    ifnotfound = stop(gettextf("%s not found", sQuote(x)), domain = NA, call. = FALSE),
                    minframe = 1L,
                    inherits = FALSE) {
  x <- insight::safe_deparse(x)
  n <- sys.nframe()
  myObj <- structure(list(.b = as.raw(7)), foo = 47L)
  while (n > minframe) {
    n <- n - 1L
    env <- sys.frame(n)
    r <- get0(x, envir = env, inherits = inherits, ifnotfound = myObj)
    if (!identical(r, myObj)) {
      return(r)
    }
  }
  ifnotfound
}

#' @keywords internal
.get_direction <- function(direction) {
  if (length(direction) > 1) {
    insight::format_warning("Using first 'direction' value.")
  }

  if (is.numeric(direction[1])) {
    return(sign(direction[1]))
  }

  Value <- c(
    left = -1,
    right = 1,
    "two-sided" = 0,
    twosided = 0,
    "one-sided" = 1,
    onesided = 1,
    "<" = -1,
    ">" = 1,
    "=" = 0,
    "==" = 0,
    "-1" = -1,
    "0" = 0,
    "1" = 1,
    "+1" = 1
  )

  direction <- Value[tolower(direction[1])]

  if (is.na(direction)) {
    insight::format_error("Unrecognized 'direction' argument.")
  }
  direction
}


#' @keywords internal
.prepare_output <- function(temp,
                            cleaned_parameters,
                            is_stan_mv = FALSE,
                            is_brms_mv = FALSE) {
  if (is.null(cleaned_parameters)) {
    return(temp)
  }
  if (isTRUE(is_stan_mv)) {
    # for models with multiple responses, we create a separate response column
    temp$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", temp$Parameter)
    # from the parameter names, we can now remove the name of the respone variables
    for (i in unique(temp$Response)) {
      temp$Parameter <- gsub(sprintf("%s|", i), "", temp$Parameter, fixed = TRUE)
    }
    merge_by <- c("Parameter", "Effects", "Component", "Response")
    remove_cols <- c("Group", "Cleaned_Parameter", "Function", ".roworder")
  } else if (isTRUE(is_brms_mv)) {
    # for models with multiple responses, we create a separate response column
    temp$Response <- gsub("(.*)_(.*)_(.*)", "\\2", temp$Parameter)
    merge_by <- c("Parameter", "Effects", "Component", "Response")
    remove_cols <- c("Group", "Cleaned_Parameter", "Function", ".roworder")
  } else {
    # By default, we only merge by these three columns
    merge_by <- c("Parameter", "Effects", "Component")
    remove_cols <- c("Group", "Cleaned_Parameter", "Response", "Function", ".roworder")
  }

  # in "temp", we have the data frame from the related functions (like
  # `point_estimate()`, `ci()`  etc.). "cleaned_parameters" is a data frame
  # only with original parameter names, model components and "cleaned"
  # parameter names (retrieved from `insight::clean_parameters()`).

  merge_by <- intersect(merge_by, colnames(temp))
  temp$.roworder <- seq_len(nrow(temp))
  out <- merge(x = temp, y = cleaned_parameters, by = merge_by, all.x = TRUE)

  # hope this works for stanmvreg...
  if ((isTRUE(is_stan_mv) || isTRUE(is_brms_mv)) && all(is.na(out$Effects)) && all(is.na(out$Component))) {
    out$Effects <- cleaned_parameters$Effects[seq_len(nrow(out))]
    out$Component <- cleaned_parameters$Component[seq_len(nrow(out))]
  }
  # this here is required for multiple response models...
  if (all(is.na(out$Effects)) || all(is.na(out$Component))) {
    out <- out[!duplicated(out$.roworder), ]
  } else {
    out <- out[!is.na(out$Effects) & !is.na(out$Component) & !duplicated(out$.roworder), ]
  }
  attr(out, "Cleaned_Parameter") <- out$Cleaned_Parameter[order(out$.roworder)]
  datawizard::data_remove(out[order(out$.roworder), ], remove_cols, verbose = FALSE)
}


#' @keywords internal
.merge_and_sort <- function(x, y, by, all) {
  if (is.null(ncol(y))) {
    return(x)
  }
  x$.rowid <- seq_len(nrow(x))
  x <- merge(x, y, by = by, all = all)
  datawizard::data_remove(x[order(x$.rowid), ], ".rowid", verbose = FALSE)
}


# returns the variables that were used for grouping data frames (dplyr::group_var())
#' @keywords internal
.group_vars <- function(x) {
  # dplyr < 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    ## TODO fix for dplyr < 0.8
    attr(x, "vars", exact = TRUE)
  } else {
    setdiff(colnames(grps), ".rows")
  }
}

#' @keywords internal
.is_baysian_grid <- function(x) {
  UseMethod(".is_baysian_grid")
}

#' @keywords internal
.is_baysian_grid.emmGrid <- function(x) {
  if (inherits(x, "emm_list")) {
    x <- x[[1]]
  }
  post.beta <- methods::slot(x, "post.beta")
  !(all(dim(post.beta) == 1) && is.na(post.beta))
}

#' @keywords internal
.is_baysian_grid.emm_list <- .is_baysian_grid.emmGrid

#' @keywords internal
.is_baysian_grid.slopes <- function(x) {
  !is.null(attr(x, "posterior_draws"))
}

#' @keywords internal
.is_baysian_grid.predictions <- .is_baysian_grid.slopes

#' @keywords internal
.is_baysian_grid.comparisons <- .is_baysian_grid.slopes

# safe add cleaned parameter names to a model object
.add_clean_parameters_attribute <- function(params, model) {
  cp <- tryCatch(
    {
      insight::clean_parameters(model)
    },
    error = function(e) {
      NULL
    }
  )
  attr(params, "clean_parameters") <- cp
  params
}

#' @keywords internal
.append_datagrid <- function(results, object, long = FALSE) {
  UseMethod(".append_datagrid", object = object)
}

#' @keywords internal
.append_datagrid.emmGrid <- function(results, object, long = FALSE) {
  # results is assumed to be a data frame with "Parameter" column
  # object is an emmeans / marginalefeects that results is based on

  all_attrs <- attributes(results) # save attributes for later
  all_class <- class(results)

  datagrid <- insight::get_datagrid(object)
  grid_names <- colnames(datagrid)

  if (long) {
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

.append_datagrid.emm_list <- .append_datagrid.emmGrid

.append_datagrid.slopes <- .append_datagrid.emmGrid

.append_datagrid.predictions <- .append_datagrid.emmGrid

.append_datagrid.comparisons <- .append_datagrid.emmGrid

.append_datagrid.data.frame <- function(results, object, long = FALSE) {
  # results is assumed to be a data frame with "Parameter" column
  # object is a data frame with an rvar column that results is based on

  all_attrs <- attributes(results) # save attributes for later
  all_class <- class(results)

  is_rvar <- vapply(object, inherits, FUN.VALUE = logical(1), "rvar")
  grid_names <- colnames(object)[!is_rvar]
  datagrid <- data.frame(object[, grid_names, drop = FALSE])

  if (long) {
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


#' @keywords internal
.get_marginaleffects_draws <- function(object) {
  # errors and checks are handled by marginaleffects
  insight::check_if_installed("marginaleffects", minimum_version = "0.24.0")
  data.frame(marginaleffects::get_draws(object, shape = "DxP"))
}

#' @keywords internal
.possibly_extract_rvar_col <- function(df, rvar_col) {
  if (missing(rvar_col) || is.null(rvar_col)) {
    return(NULL)
  }

  if (is.character(rvar_col) &&
    length(rvar_col) == 1L &&
    rvar_col %in% colnames(df) &&
    inherits(df[[rvar_col]], "rvar")) {
    return(df[[rvar_col]])
  }

  insight::format_error("The `rvar_col` argument must be a single, valid column name.")
}
