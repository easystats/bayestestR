#' Maximum A Posteriori probability estimate (MAP)
#'
#' Find the **Highest Maximum A Posteriori probability estimate (MAP)** of a
#' posterior, i.e., the value associated with the highest probability density
#' (the "peak" of the posterior distribution). In other words, it is an estimation
#' of the *mode* for continuous parameters. Note that this function relies on
#' [`estimate_density()`], which by default uses a different smoothing bandwidth
#' (`"SJ"`) compared to the legacy default implemented the base R [`density()`]
#' function (`"nrd0"`).
#'
#' @inheritParams hdi
#' @inheritParams estimate_density
#'
#' @return A numeric value if `x` is a vector. If `x` is a model-object,
#' returns a data frame with following columns:
#'
#' - `Parameter`: The model parameter(s), if `x` is a model-object. If `x` is a
#'   vector, this column is missing.
#' - `MAP_Estimate`: The MAP estimate for the posterior or each model parameter.
#'
#' @examplesIf require("rstanarm") && require("brms")
#' \donttest{
#' library(bayestestR)
#'
#' posterior <- rnorm(10000)
#' map_estimate(posterior)
#'
#' plot(density(posterior))
#' abline(v = as.numeric(map_estimate(posterior)), col = "red")
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' map_estimate(model)
#' }
#'
#' @export
map_estimate <- function(x, ...) {
  UseMethod("map_estimate")
}


# numeric -----------------------

#' @rdname map_estimate
#' @export
map_estimate.numeric <- function(x, precision = 2^10, method = "kernel", ...) {
  out <- map_estimate(data.frame(x = x),
    precision,
    method = method, ...
  )
  attr(out, "data") <- x
  out
}

.map_estimate <- function(x, precision = 2^10, method = "kernel", ...) {
  d <- estimate_density(x, precision = precision, method = method, ...)

  out <- d$x[which.max(d$y)]
  attr(out, "MAP_density") <- max(d$y)
  out
}


# other models -----------------------

#' @export
map_estimate.bayesQR <- function(x, precision = 2^10, method = "kernel", ...) {
  x <- insight::get_parameters(x)
  map_estimate(x, precision = precision, method = method)
}

#' @export
map_estimate.BGGM <- map_estimate.bayesQR

#' @export
map_estimate.mcmc <- map_estimate.bayesQR

#' @export
map_estimate.bamlss <- map_estimate.bayesQR

#' @export
map_estimate.bcplm <- map_estimate.bayesQR

#' @export
map_estimate.blrm <- map_estimate.bayesQR

#' @export
map_estimate.mcmc.list <- map_estimate.bayesQR


# stan / posterior models -----------------------

#' @keywords internal
.map_estimate_models <- function(x, precision, method, ...) {
  l <- sapply(x, .map_estimate, precision = precision, method = method, simplify = FALSE, ...)

  out <- data.frame(
    Parameter = colnames(x),
    MAP_Estimate = unlist(l, use.names = FALSE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .add_clean_parameters_attribute(out, x)
  attr(out, "MAP_density") <- sapply(l, attr, "MAP_density")
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "centrality") <- "map"
  class(out) <- unique(c("map_estimate", "see_point_estimate", class(out)))
  out
}


#' @rdname map_estimate
#' @export
map_estimate.stanreg <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    precision = precision,
    method = method
  )
}

#' @export
map_estimate.stanfit <- map_estimate.stanreg

#' @export
map_estimate.blavaan <- map_estimate.stanreg


#' @rdname map_estimate
#' @export
map_estimate.brmsfit <- function(x, precision = 2^10, method = "kernel", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  .map_estimate_models(
    x = insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    precision = precision, method = method
  )
}


#' @rdname map_estimate
#' @inheritParams p_direction
#' @export
map_estimate.data.frame <- function(x, precision = 2^10, method = "kernel", rvar_col = NULL, ...) {
  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::map_estimate
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    obj_name <- insight::safe_deparse_symbol(substitute(x))
    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x))
  }

  .map_estimate_models(x, precision = precision, method = method)
}


#' @export
map_estimate.draws <- function(x, precision = 2^10, method = "kernel", ...) {
  .map_estimate_models(.posterior_draws_to_df(x), precision = precision, method = method)
}

#' @export
map_estimate.rvar <- map_estimate.draws


#' @export
map_estimate.emmGrid <- function(x, precision = 2^10, method = "kernel", ...) {
  xdf <- insight::get_parameters(x)
  out <- .map_estimate_models(xdf, precision = precision, method = method)
  .append_datagrid(out, x)
}

#' @export
map_estimate.emm_list <- map_estimate.emmGrid

#' @export
map_estimate.slopes <- function(x, precision = 2^10, method = "kernel", ...) {
  xrvar <- .get_marginaleffects_draws(x)
  out <- map_estimate(xrvar, precision = precision, method = method, ...)
  .append_datagrid(out, x)
}

#' @export
map_estimate.comparisons <- map_estimate.slopes

#' @export
map_estimate.predictions <- map_estimate.slopes


#' @rdname map_estimate
#' @export
map_estimate.get_predicted <- function(x,
                                       precision = 2^10,
                                       method = "kernel",
                                       use_iterations = FALSE,
                                       verbose = TRUE,
                                       ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- map_estimate(
        as.data.frame(t(attributes(x)$iterations)),
        precision = precision,
        method = method,
        verbose = verbose,
        ...
      )
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- map_estimate(as.numeric(x),
      precision = precision,
      method = method,
      verbose = verbose,
      ...
    )
  }
  out
}


# Methods -----------------------------------------------------------------

#' @rdname as.numeric.p_direction
#' @method as.numeric map_estimate
#' @export
as.numeric.map_estimate <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    me <- as.numeric(as.vector(x$MAP_Estimate))
    names(me) <- x$Parameter
    me
  } else {
    as.vector(x)
  }
}


#' @method as.double map_estimate
#' @export
as.double.map_estimate <- as.numeric.map_estimate
