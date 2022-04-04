# helper ------------------------------


.posterior_draws_to_df <- function(x) {
  UseMethod(".posterior_draws_to_df")
}

.posterior_draws_to_df.default <- function(x) {
  stop(paste0("Objects of class '%s' are not yet supported.", class(x)[1]))
}

.posterior_draws_to_df.data.frame <- function(x) {
  x
}

.posterior_draws_to_df.draws_df <- function(x) {
  insight::check_if_installed("posterior")
  datawizard::data_remove(as.data.frame(posterior::as_draws_df(x)), c(".chain", ".iteration", ".draw"))
}

.posterior_draws_to_df.draws_matrix <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_array <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_list <- .posterior_draws_to_df.draws_df

.posterior_draws_to_df.draws_rvars <- .posterior_draws_to_df.draws_df
