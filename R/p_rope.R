#' Probability of not being in ROPE
#'
#' Compute the proportion of the posterior distribution that doesn't lie within a region of practical equivalence (ROPE). It is equivalent to running \code{rope(..., ci = 1)}.
#'
#' @inheritParams rope
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



#' @rdname p_rope
#' @export
p_rope.default <- function(x, ...) {
  NULL
}


#' @rdname p_rope
#' @export
p_rope.numeric <- function(x, range = "default", ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, ...))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @rdname p_rope
#' @export
p_rope.data.frame <- p_rope.numeric

#' @rdname p_rope
#' @export
p_rope.emmGrid <- function(x, range = "default", ...) {
  xdf <- insight::get_parameters(x)

  out <- p_rope(xdf, range = range)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_rope.emm_list <- p_rope.emmGrid

#' @rdname p_rope
#' @export
p_rope.BFBayesFactor <- p_rope.numeric

#' @rdname p_rope
#' @export
p_rope.MCMCglmm <- p_rope.numeric


#' @rdname p_rope
#' @export
p_rope.stanreg <- function(x, range = "default", effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, effects = effects, parameters = parameters, ...))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_rope.stanfit <- p_rope.stanreg


#' @rdname p_rope
#' @export
p_rope.brmsfit <- function(x, range = "default", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, effects = effects, component = component, parameters = parameters, ...))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @export
p_rope.sim.merMod <- p_rope.stanreg


#' @export
p_rope.sim <- function(x, range = "default", parameters = NULL, ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, parameters = parameters, ...))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_rope.mcmc <- function(x, range = "default", parameters = NULL, ...) {
  out <- .p_rope(rope(x, range = range, ci = 1, parameters = parameters, ...))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_rope.bcplm <- p_rope.mcmc




# Internal ----------------------------------------------------------------


#' @keywords internal
.p_rope <- function(rope_rez) {
  cols <- c("Parameter", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Effects", "Component")
  out <- as.data.frame(rope_rez[cols[cols %in% names(rope_rez)]])
  names(out)[names(out) == "ROPE_Percentage"] <- "p_ROPE"

  class(out) <- c("p_rope", "see_p_rope", "data.frame")
  out
}