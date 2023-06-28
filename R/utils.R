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
.get_direction <- function(direction) {
  if (length(direction) > 1) {
    insight::format_warning("Using first 'direction' value.")
  }

  if (is.numeric(direction[1])) {
    return(sign(direction[1]))
  }

  Value <- c(
    "left" = -1,
    "right" = 1,
    "two-sided" = 0,
    "twosided" = 0,
    "one-sided" = 1,
    "onesided" = 1,
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
    temp$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", temp$Parameter)
    for (i in unique(temp$Response)) {
      temp$Parameter <- gsub(sprintf("%s|", i), "", temp$Parameter, fixed = TRUE)
    }
    merge_by <- c("Parameter", "Effects", "Component", "Response")
    remove_cols <- c("Group", "Cleaned_Parameter", "Function", ".roworder")
  } else if (isTRUE(is_brms_mv)) {
    temp$Response <- gsub("(.*)_(.*)_(.*)", "\\2", temp$Parameter)
    # temp$Parameter <- gsub("(.*)_(.*)_(.*)", "\\1_\\3", temp$Parameter)
    merge_by <- c("Parameter", "Effects", "Component", "Response")
    remove_cols <- c("Group", "Cleaned_Parameter", "Function", ".roworder")
  } else {
    merge_by <- c("Parameter", "Effects", "Component")
    remove_cols <- c("Group", "Cleaned_Parameter", "Response", "Function", ".roworder")
  }
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
.is_baysian_emmeans <- function(x) {
  if (inherits(x, "emm_list")) {
    x <- x[[1]]
  }
  post.beta <- methods::slot(x, "post.beta")
  !(all(dim(post.beta) == 1) && is.na(post.beta))
}


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
