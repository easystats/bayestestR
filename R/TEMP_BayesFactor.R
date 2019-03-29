#' #' @keywords internal
#' .extract_posterior_BF <- function(x, iterations = 4000, ...){
#'   # Should we adapt this as get_parameters and put it in insight?
#'
#'   if (!requireNamespace("BayesFactor")) {
#'     warning("This function needs `BayesFactor` to be installed... installing now.")
#'     install.packages("BayesFactor")
#'     requireNamespace("BayesFactor")
#'   }
#'
#'   posterior <- BayesFactor::posterior(x, iterations = iterations, progress = FALSE, ...)
#'   posterior <- as.data.frame(posterior)
#'   return(x)
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#' #' @param iterations For BayesFactor models, the number of posterior draws.
#' #' @rdname hdi
#' #' @export
#' hdi.BayesFactor <- function(x, ci = .90, iterations = 4000, ...) {
#'   posteriors <- .extract_posterior_BF(x, iterations=iterations, ...)
#'   .compute_interval_stanreg(x, ci, effects, parameters, verbose, fun = "hdi")
#' }
#'
#'
#'
#'
#'
#' #' @importFrom insight get_parameters
#' #' @keywords internal
#' .compute_interval_BayesFactor <- function(x, ci, effects, parameters, verbose, fun) {
#'   list <- lapply(c("fixed", "random"), function(.x) {
#'     parms <- .extract_posterior_BF(x, effects = .x, parameters = parameters)
#'     tmp <- do.call(rbind, sapply(
#'       parms,
#'       get(fun, asNamespace("bayestestR")),
#'       ci = ci,
#'       verbose = verbose,
#'       simplify = FALSE
#'     ))
#'
#'     if (!.is_empty_object(tmp)) {
#'       tmp <- .clean_up_tmp_stanreg(
#'         tmp,
#'         .x,
#'         cols = c("CI", "CI_low", "CI_high", "Group"),
#'         parms = names(parms)
#'       )
#'     } else {
#'       tmp <- NULL
#'     }
#'
#'     tmp
#'   })
#'
#'   dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))
#'
#'   dat <- switch(
#'     effects,
#'     fixed = .select_rows(dat, "Group", "fixed"),
#'     random = .select_rows(dat, "Group", "random"),
#'     dat
#'   )
#'
#'   if (all(dat$Group == dat$Group[1])) {
#'     dat <- .remove_column(dat, "Group")
#'   }
#'
#'   class(dat) <- c(fun, class(dat))
#'   dat
#' }