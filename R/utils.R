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



#' Used in describe_posterior
#' @keywords internal
.reoder_rows <- function(x, out, ci = NULL) {
  if (!is.data.frame(out) || nrow(out) == 1) {
    return(out)
  }

  if (is.null(ci)) {
    refdata <- point_estimate(x, centrality = "median", dispersion = FALSE)
    order <- refdata$Parameter
    out <- out[match(order, out$Parameter), ]
  } else {
    uncertainty <- ci(x, ci = ci)
    order <- paste0(uncertainty$Parameter, uncertainty$CI)
    out <- out[match(order, paste0(out$Parameter, out$CI)), ]
  }
  rownames(out) <- NULL
  out
}


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

#' @keywords internal
#' This function can be used to add the component and effects columns to results
#' tables. E.g.
#' resHDI <- hdi(model, effects = "all")
#' tabs <- .get_eff_com(model)
#' merge(x = resBF, y = tabs, by = "Parameter", all.x = TRUE)
.get_eff_com <- function(model, effects, component, parameters) {
  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  .get_tab <- function(.x, .y) {
    parms <- insight::get_parameters(model, effects = .x, component = .y)

    data.frame(Parameter = colnames(parms), Effect = .x, Component = .y)
  }

  tab <- mapply(.get_tab, eff, com, SIMPLIFY = FALSE)
  tab <- do.call(rbind,tab)
}
