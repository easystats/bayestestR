#' Flatten a list
#'
#' @param list A list.
#' @param name Name of column of keys in the case the output is a dataframe.
#' @export
flatten_list <- function(list, name = "name") {
  if (length(list) == 1) {
    return(list[[1]])
  } else
  if (all(sapply(list, is.data.frame))) {
    if (is.null(names(list))) {
      return(as.data.frame(t(sapply(list, rbind))))
    } else {
      tryCatch({
        df <- data.frame()
        for (i in names(list)) {
          list[[i]][name] <- i
          df <- rbind(df, list[[i]])
        }
        df <- df[c(name, names(df)[names(df) != name])]
        return(as.data.frame(df))
      }, warning = function(w) {
        return(list)
      }, error = function(e) {
        return(list)
      })
    }
  } else {
    return(list)
  }
}
