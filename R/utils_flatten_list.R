#' Flatten a list
#'
#' @param object A list.
#' @param name Name of column of keys in the case the output is a dataframe.
#' @keywords internal
flatten_list <- function(object, name = "name") {
  if (length(object) == 1) {
    object[[1]]
  } else if (all(sapply(object, is.data.frame))) {
    if (is.null(names(object))) {
      as.data.frame(t(sapply(object, rbind)))
    } else {
      tryCatch({
        rn <- names(object)
        object <- do.call(rbind, object)
        object[name] <- rn
        object[c(name, setdiff(names(object), name))]
      }, warning = function(w) {
        object
      }, error = function(e) {
        object
      })
    }
  } else {
    object
  }
}


# remove NULL elements from lists
compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]

#' @keywords internal
# is string empty?
.is_empty_object <- function(x) {
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}
