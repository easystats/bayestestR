#' @keywords internal
.clean_up_tmp_stanreg <- function(tmp, x, cols, parms) {
  tmp$Group <- x
  tmp$Parameter <- rep(parms, each = nrow(tmp) / length(parms))
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  tmp$Parameter <- gsub("b\\[(.*) (.*)\\]", "\\2", tmp$Parameter)
  tmp
}


#' @keywords internal
.clean_up_tmp_brms <- function(tmp, x, y, cols, parms) {
  tmp$Group <- x
  tmp$Component <- y
  tmp$Parameter <- rep(parms, each = nrow(tmp) / length(parms))
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  tmp$Parameter <- gsub("r_(.*)\\.(.*)\\.", "\\1", tmp$Parameter)
  tmp
}
