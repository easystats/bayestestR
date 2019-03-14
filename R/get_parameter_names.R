#' @importFrom insight find_parameters
.get_parameter_names <- function(posterior, effects, component, parameters) {
  pars <- insight::find_parameters(posterior, parameters = parameters)

  pars <- switch(
    effects,
    "fixed" = pars[c("conditional", "zero_inflated")],
    "random" = pars[c("random", "zero_inflated_random")],
    "all" = pars
  )

  pars <- switch(
    component,
    "conditional" = pars[c("conditional", "random")],
    "zi" = ,
    "zero_inflated" = pars[c("zero_inflated", "zero_inflated_random")],
    "all" = pars
  )

  unlist(pars)
}
