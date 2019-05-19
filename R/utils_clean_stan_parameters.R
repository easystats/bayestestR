#' @keywords internal
.clean_up_tmp_stanreg <- function(tmp, group, cols, parms) {
  tmp$Group <- group
  tmp$Parameter <- rep(parms, each = nrow(tmp) / length(parms))
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  # tmp$Parameter <- gsub("b\\[(.*) (.*)\\]", "\\2", tmp$Parameter)
  tmp
}


#' @keywords internal
.clean_up_tmp_brms <- function(tmp, group, component, cols, parms) {
  tmp$Group <- group
  tmp$Component <- component
  tmp$Parameter <- rep(parms, each = nrow(tmp) / length(parms))
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  # tmp$Parameter <- gsub("r_(.*)\\.(.*)\\.", "\\1", tmp$Parameter)
  tmp
}
