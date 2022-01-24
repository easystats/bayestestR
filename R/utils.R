# trim leading / trailing whitespace
.trim <- function(x) gsub("^\\s+|\\s+$", "", x)


# safe depare, also for very long strings
.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), .trim, simplify = TRUE), collapse = "")
}


# remove NULL elements from lists
#' @keywords internal
.compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL", na.rm = TRUE))]

# is string empty?
#' @keywords internal
.is_empty_object <- function(x) {
  if (is.list(x)) {
    x <- tryCatch(
      {
        .compact_list(x)
      },
      error = function(x) {
        x
      }
    )
  }
  # this is an ugly fix because of ugly tibbles
  if (inherits(x, c("tbl_df", "tbl"))) x <- as.data.frame(x)
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}


# select rows where values in "variable" match "value"
#' @keywords internal
.select_rows <- function(data, variable, value) {
  data[which(data[[variable]] == value), ]
}

# remove column
#' @keywords internal
.remove_column <- function(data, variables) {
  data[variables] <- NULL
  data
}


#' @keywords internal
.to_long <- function(x, names_to = "key", values_to = "value", columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]
  dat <- stats::reshape(
    as.data.frame(x),
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = names_to,
    v.names = values_to,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[values_to]])) {
    dat[[values_to]] <- as.character(dat[[values_to]])
  }

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}

#' select numerics columns
#' @keywords internal
.select_nums <- function(x) {
  x[unlist(lapply(x, is.numeric))]
}



## TODO remove?!?

# #' Used in describe_posterior
# #' @keywords internal
# .reorder_rows <- function(x, out, ci = NULL) {
#   if (!is.data.frame(out) || nrow(out) == 1) {
#     return(out)
#   }
#
#   if (is.null(ci)) {
#     refdata <- point_estimate(x, centrality = "median", dispersion = FALSE)
#     order <- refdata$Parameter
#     out <- out[match(order, out$Parameter), ]
#   } else {
#     uncertainty <- ci(x, ci = ci)
#     order <- paste0(uncertainty$Parameter, uncertainty$CI)
#     out <- out[match(order, paste0(out$Parameter, out$CI)), ]
#   }
#   rownames(out) <- NULL
#   out
# }


#' @keywords internal
.get_direction <- function(direction) {
  if (length(direction) > 1) warning("Using first 'direction' value.")

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
    stop("Unrecognized 'direction' argument.")
  }
  direction
}


#' @keywords internal
.prepare_output <- function(temp, cleaned_parameters, is_stan_mv = FALSE, is_brms_mv = FALSE) {
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
  temp$.roworder <- 1:nrow(temp)
  out <- merge(x = temp, y = cleaned_parameters, by = merge_by, all.x = TRUE)
  # hope this works for stanmvreg...
  if ((isTRUE(is_stan_mv) || isTRUE(is_brms_mv)) && all(is.na(out$Effects)) && all(is.na(out$Component))) {
    out$Effects <- cleaned_parameters$Effects[1:nrow(out)]
    out$Component <- cleaned_parameters$Component[1:nrow(out)]
  }
  # this here is required for multiple response models...
  if (all(is.na(out$Effects)) || all(is.na(out$Component))) {
    out <- out[!duplicated(out$.roworder), ]
  } else {
    out <- out[!is.na(out$Effects) & !is.na(out$Component) & !duplicated(out$.roworder), ]
  }
  attr(out, "Cleaned_Parameter") <- out$Cleaned_Parameter[order(out$.roworder)]
  .remove_column(out[order(out$.roworder), ], remove_cols)
}


#' @keywords internal
.merge_and_sort <- function(x, y, by, all) {
  if (is.null(ncol(y))) {
    return(x)
  }
  x$.rowid <- 1:nrow(x)
  x <- merge(x, y, by = by, all = all)
  .remove_column(x[order(x$.rowid), ], ".rowid")
}



# returns the row-indices for grouped data frames
#' @keywords internal
.group_indices <- function(x) {
  # dplyr < 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    attr(x, "indices", exact = TRUE)
  } else {
    grps[[".rows"]]
  }
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


.n_unique <- function(x, na.rm = TRUE) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- stats::na.omit(x)
  length(unique(x))
}
