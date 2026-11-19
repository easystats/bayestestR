# Temporary stand-ins until these methods are available in {insight}
#' @exportS3Method insight::find_algorithm
find_algorithm.stanli_fit <- function(x, ...) {
  list(
    algorithm = "NUTS",
    chains = x$chains,
    iterations = x$samples + x$warmup,
    warmup = x$warmup,
    thin = x$thin
  )
}

#' @exportS3Method insight::get_parameters
get_parameters.stanli_fit <- function(
  x,
  parameters = NULL,
  summary = FALSE,
  ...
) {
  out <- .posterior_draws_to_df(x)
  if (!is.null(parameters)) {
    out <- out[grepl(parameters, colnames(out), perl = TRUE)]
  }
  if (summary) {
    out <- data.frame(
      Parameter = colnames(out),
      Mean = vapply(out, mean, numeric(1)),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }
  out
}
