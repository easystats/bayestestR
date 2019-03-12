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


# select rows where values in "variabl" match "value"
#' @keywords internal
.select_rows <- function(data, variable, value) {
  data[which(data[[variable]] == value), ]
}

# remove column
#' @keywords internal
.remove_column <- function(data, variable) {
  data[, -which(colnames(data) == variable), drop = FALSE]
}
