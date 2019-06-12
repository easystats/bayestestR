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
  data[, -which(colnames(data) %in% variables), drop = FALSE]
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
