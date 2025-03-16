#' Probability of being in the ROPE
#'
#' Compute the proportion of the whole posterior distribution that doesn't lie within a region of practical equivalence (ROPE). It is equivalent to running `rope(..., ci = 1)`.
#'
#' @inheritParams rope
#'
#' @inheritSection hdi Model components
#'
#' @examples
#' library(bayestestR)
#'
#' p_rope(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' p_rope(x = mtcars, range = c(-0.1, 0.1))
#' @export
p_rope <- function(x, ...) {
  UseMethod("p_rope")
}


#' @method as.double p_rope
#' @export
as.double.p_rope <- function(x, ...) {
  x
}


#' @export
p_rope.default <- function(x, ...) {
  NULL
}


#' @rdname p_rope
#' @export
p_rope.numeric <- function(x, range = "default", verbose = TRUE, ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, verbose = verbose, ...))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
#' @rdname p_rope
#' @inheritParams p_direction
p_rope.data.frame <- function(x, range = "default", rvar_col = NULL, verbose = TRUE, ...) {
  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::p_rope
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    obj_name <- insight::safe_deparse_symbol(substitute(x))
    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x))
  }

  out <- .p_rope(rope(x, range = range, ci = 1, verbose = verbose, ...))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_rope.draws <- function(x, range = "default", verbose = TRUE, ...) {
  p_rope(.posterior_draws_to_df(x), range = range, verbose = verbose, ...)
}

#' @export
p_rope.rvar <- p_rope.draws


#' @export
p_rope.emmGrid <- function(x, range = "default", verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x)
  out <- p_rope(xdf, range = range, verbose = verbose)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_rope.emm_list <- p_rope.emmGrid

#' @export
p_rope.slopes <- function(x, range = "default", verbose = TRUE, ...) {
  xrvar <- .get_marginaleffects_draws(x)
  out <- p_rope(xrvar, range = range, verbose = verbose)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_rope.comparisons <- p_rope.slopes

#' @export
p_rope.predictions <- p_rope.slopes

#' @export
p_rope.BFBayesFactor <- p_rope.numeric

#' @export
p_rope.MCMCglmm <- p_rope.numeric


#' @export
p_rope.stanreg <- function(x,
                           range = "default",
                           effects = "fixed",
                           component = "location",
                           parameters = NULL,
                           verbose = TRUE,
                           ...) {
  out <- .p_rope(rope(
    x,
    range = range,
    ci = 1,
    effects = effects,
    component = component,
    parameters = parameters,
    verbose = verbose,
    ...
  ))
  out <- .add_clean_parameters_attribute(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_rope.stanfit <- p_rope.stanreg

#' @export
p_rope.blavaan <- p_rope.stanreg


#' @rdname p_rope
#' @export
p_rope.brmsfit <- function(x,
                           range = "default",
                           effects = "fixed",
                           component = "conditional",
                           parameters = NULL,
                           verbose = TRUE,
                           ...) {
  out <- .p_rope(rope(
    x,
    range = range,
    ci = 1,
    effects = effects,
    component = component,
    parameters = parameters,
    verbose = verbose,
    ...
  ))
  out <- .add_clean_parameters_attribute(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_rope.sim.merMod <- p_rope.stanreg


#' @export
p_rope.sim <- function(x, range = "default", parameters = NULL, verbose = TRUE, ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, parameters = parameters, verbose = verbose, ...))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_rope.bamlss <- function(x,
                          range = "default",
                          component = "all",
                          parameters = NULL,
                          verbose = TRUE,
                          ...) {
  out <- .p_rope(rope(
    x,
    range = range,
    ci = 1,
    effects = "all",
    component = component,
    parameters = parameters,
    verbose = verbose,
    ...
  ))
  out <- .add_clean_parameters_attribute(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_rope.mcmc <- function(x, range = "default", parameters = NULL, verbose = TRUE, ...) {
  out <- .p_rope(rope(
    x,
    range = range,
    ci = 1,
    parameters = parameters,
    verbose = verbose,
    ...
  ))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_rope.bcplm <- p_rope.mcmc

#' @export
p_rope.BGGM <- p_rope.mcmc

#' @export
p_rope.blrm <- p_rope.mcmc

#' @export
p_rope.mcmc.list <- p_rope.mcmc


# Internal ----------------------------------------------------------------


#' @keywords internal
.p_rope <- function(rope_rez) {
  cols <- c("Parameter", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Effects", "Component")
  out <- as.data.frame(rope_rez)[cols[cols %in% names(rope_rez)]]
  names(out)[names(out) == "ROPE_Percentage"] <- "p_ROPE"

  class(out) <- c("p_rope", "see_p_rope", "data.frame")
  out
}
