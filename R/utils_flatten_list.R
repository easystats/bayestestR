#' Flatten a list
#'
#' @param object A list.
#' @param name Name of column of keys in the case the output is a dataframe.
#' @export
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
