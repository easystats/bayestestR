#' @keywords internal
.clean_up_tmp_stanreg <- function(tmp, x, cols) {
  tmp$Group <- x
  tmp$Parameter <- rownames(tmp)
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  tmp$Parameter <- gsub("b\\[(.*) (.*)\\]", "\\2", tmp$Parameter)
  .clean_parameters(tmp)
}


#' @keywords internal
.clean_up_tmp_brms <- function(tmp, x, y, cols) {
  tmp$Group <- x
  tmp$Component <- y
  tmp$Parameter <- rownames(tmp)
  rownames(tmp) <- NULL
  tmp <- tmp[, c("Parameter", cols)]
  # clean random effects notation from parameters
  tmp$Parameter <- gsub("r_(.*)\\.(.*)\\.", "\\1", tmp$Parameter)
  .clean_parameters(tmp)
}


#' @keywords internal
.clean_parameters <- function(x) {
  removers <- grep("^(prior_|sd_|cor_|lp__)", x$Parameter)

  if (length(removers)) {
    x <- x[-removers, ]
  }

  if (nrow(x) == 0)
    NULL
  else
    x
}
