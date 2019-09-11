# trim leading / trailing whitespace
.trim <- function(x) gsub("^\\s+|\\s+$", "", x)


# safe depare, also for very long strings
.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), .trim, simplify = TRUE), collapse = "")
}


# has object an element with given name?
#' @keywords internal
.obj_has_name <- function(x, name) {
  name %in% names(x)
}

# remove NULL elements from lists
#' @keywords internal
.compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]

# is string empty?
#' @keywords internal
.is_empty_object <- function(x) {
  if (is.list(x)) {
    x <- tryCatch({
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


#' @importFrom stats reshape
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

#' #' Used in describe_posterior
#' #' @keywords internal
#' .reorder_rows <- function(x, out, ci = NULL) {
#'   if (!is.data.frame(out) || nrow(out) == 1) {
#'     return(out)
#'   }
#'
#'   if (is.null(ci)) {
#'     refdata <- point_estimate(x, centrality = "median", dispersion = FALSE)
#'     order <- refdata$Parameter
#'     out <- out[match(order, out$Parameter), ]
#'   } else {
#'     uncertainty <- ci(x, ci = ci)
#'     order <- paste0(uncertainty$Parameter, uncertainty$CI)
#'     out <- out[match(order, paste0(out$Parameter, out$CI)), ]
#'   }
#'   rownames(out) <- NULL
#'   out
#' }


#' @keywords internal
.get_direction <- function(direction) {
  if (length(direction) > 1) warning("Using first 'direction' value.")

  if (is.numeric(direction[1])) {
    return(direction[1])
  }

  Value <- c(
    "left"      = -1,
    "right"     =  1,
    "two-sided" =  0,
    "twosided"  =  0,
    "one-sided" =  1,
    "onesided"  =  1,
    "<"         = -1,
    ">"         =  1,
    "="         =  0,
    "=="        =  0,
    "-1"        = -1,
    "0"         =  0,
    "1"         =  1,
    "+1"        =  1
  )

  direction <- Value[tolower(direction[1])]

  if (is.na(direction)) {
    stop("Unrecognized 'direction' argument.")
  }
  direction
}



.prepare_output <- function(temp, cleaned_parameters) {
  merge_by <- intersect(c("Parameter", "Effects", "Component"), colnames(temp))
  temp$.roworder <- 1:nrow(temp)
  out <- merge(x = temp, y = cleaned_parameters, by = merge_by, all.x = TRUE)
  attr(out, "Cleaned_Parameter") <- out$Cleaned_Parameter[order(out$.roworder)]
  .remove_column(out[order(out$.roworder), ], c("Group", "Cleaned_Parameter", "Response", ".roworder"))
}


.merge_and_sort <- function(x, y, by, all) {
  x$.rowid <- 1:nrow(x)
  x <- merge(x, y, by = by, all = all)
  .remove_column(x[order(x$.rowid), ], ".rowid")
}


## TODO remove?!?

#' #' @keywords internal
#' #' This function can be used to add the component and effects columns to results
#' #' tables. E.g.
#' #' resHDI <- hdi(model, effects = "all")
#' #' tabs <- .get_eff_com(model)
#' #' merge(x = resBF, y = tabs, by = "Parameter", all.x = TRUE)
#' .get_eff_com <- function(model, effects = NULL, component = NULL, parameters = NULL) {
#'   if (inherits(model, "brmsfit")) {
#'     eff <- c("fixed", "fixed", "random", "random")
#'     com <- c("conditional", "zero_inflated", "conditional", "zero_inflated")
#'   } else {
#'     eff <- c("fixed", "random")
#'     com <- c("conditional", "conditional")
#'   }
#'
#'   .get_tab <- function(.x, .y) {
#'     parms <- insight::get_parameters(model, effects = .x, component = .y, parameters = parameters)
#'
#'     if (nrow(parms) > 0 && ncol(parms) > 0)
#'       data.frame(Parameter = colnames(parms), Group = .x, Component = .y)
#'     else
#'       NULL
#'   }
#'
#'   tab <- mapply(.get_tab, eff, com, SIMPLIFY = FALSE)
#'
#'   .select_effects_component(
#'     do.call(rbind, tab),
#'     effects = effects,
#'     component = component
#'   )
#' }
#'
#'
#'
#' .select_effects_component <- function(dat, effects, component = NULL) {
#'   if ("Group" %in% colnames(dat)) {
#'     dat <- switch(
#'       effects,
#'       fixed = .select_rows(dat, "Group", "fixed"),
#'       random = .select_rows(dat, "Group", "random"),
#'       dat
#'     )
#'   }
#'
#'   if (!is.null(component) && "Component" %in% colnames(dat)) {
#'     dat <- switch(
#'       component,
#'       conditional = .select_rows(dat, "Component", "conditional"),
#'       zi = ,
#'       zero_inflated = .select_rows(dat, "Component", "zero_inflated"),
#'       dat
#'     )
#'   }
#'
#'   if ("Group" %in% colnames(dat) && all(dat$Group == dat$Group[1])) {
#'     dat <- .remove_column(dat, "Group")
#'   }
#'
#'   if ("Component" %in% colnames(dat) && all(dat$Component == dat$Component[1])) {
#'     dat <- .remove_column(dat, "Component")
#'   }
#'
#'   rownames(dat) <- NULL
#'   dat
#' }