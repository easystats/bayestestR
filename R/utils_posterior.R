# helper ------------------------------

.posterior_draws_to_df <- function(x) {
  UseMethod(".posterior_draws_to_df")
}

.posterior_draws_to_df.draws_array <- function(x) {
  cn <- dimnames(x)$variable
  old_dim <- dim(x)
  dim(x) <- c(old_dim[1] * old_dim[2], old_dim[3])
  out <- as.data.frame(x)
  colnames(out) <- cn
  out
}

.posterior_draws_to_df.draws_list <- function(x) {
  do.call(rbind.data.frame, x)
}

.posterior_draws_to_df.draws_df <- function(x) {
  datawizard::data_remove(as.data.frame(x), c(".chain", ".iteration", ".draw"))
}

.posterior_draws_to_df.draws_matrix <- .posterior_draws_to_df.draws_df
