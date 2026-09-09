#' Bayesian p-value based on the density at the Maximum A Posteriori (MAP)
#'
#' Compute a Bayesian equivalent of the *p*-value, related to the odds that a
#' parameter (described by its posterior distribution) has against the null
#' hypothesis (*h0*) using Mills' (2014, 2017) *Objective Bayesian Hypothesis
#' Testing* framework. It corresponds to the density value at the null (e.g., 0)
#' divided by the density at the Maximum A Posteriori (MAP).
#'
#' @details Note that this method is sensitive to the density estimation `method`
#' (see the section in the examples below).
#'
#' ## Strengths and Limitations
#'
#' **Strengths:** Straightforward computation. Objective property of the posterior
#' distribution.
#'
#' **Limitations:** Limited information favoring the null hypothesis. Relates
#' on density approximation. Indirect relationship between mathematical
#' definition and interpretation. Only suitable for weak / very diffused priors.
#'
#' @inheritParams hdi
#' @inheritParams density_at
#' @inheritParams pd
#'
#' @inheritSection hdi Model components
#'
#' @examplesIf require("rstanarm") && require("emmeans") && require("brms") && require("BayesFactor")
#' library(bayestestR)
#'
#' p_map(rnorm(1000, 0, 1))
#' p_map(rnorm(1000, 10, 1))
#' \donttest{
#' model <- suppressWarnings(
#'   rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' p_map(model)
#'
#' p_map(suppressWarnings(
#'   emmeans::emtrends(model, ~1, "wt", data = mtcars)
#' ))
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_map(model)
#'
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' p_map(bf)
#'
#' # ---------------------------------------
#' # Robustness to density estimation method
#' set.seed(333)
#' data <- data.frame()
#' for (iteration in 1:250) {
#'   x <- rnorm(1000, 1, 1)
#'   result <- data.frame(
#'     Kernel = as.numeric(p_map(x, method = "kernel")),
#'     KernSmooth = as.numeric(p_map(x, method = "KernSmooth")),
#'     logspline = as.numeric(p_map(x, method = "logspline"))
#'   )
#'   data <- rbind(data, result)
#' }
#' data$KernSmooth <- data$Kernel - data$KernSmooth
#' data$logspline <- data$Kernel - data$logspline
#'
#' summary(data$KernSmooth)
#' summary(data$logspline)
#' boxplot(data[c("KernSmooth", "logspline")])
#' }
#' @seealso [Jeff Mill's talk](https://www.youtube.com/watch?v=Ip8Ci5KUVRc)
#'
#' @references
#' - Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of Effect Existence and Significance in the Bayesian Framework. Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
#' - Mills, J. A. (2018). Objective Bayesian Precise Hypothesis Testing. University of Cincinnati.
#'
#' @export
p_map <- function(x, ...) {
  UseMethod("p_map")
}

#' @rdname p_map
#' @export
p_pointnull <- p_map


#' @rdname p_map
#' @export
p_map.numeric <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  p_map(data.frame(Posterior = x), null = null, precision = precision, method = method, ...)
}


#' @rdname p_map
#' @export
p_map.get_predicted <- function(x,
                                null = 0,
                                precision = 2^10,
                                method = "kernel",
                                use_iterations = FALSE,
                                verbose = TRUE,
                                ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- p_map(
        as.data.frame(t(attributes(x)$iterations)),
        null = null,
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
    out <- p_map(as.numeric(x),
      null = null,
      precision = precision,
      method = method,
      verbose = verbose,
      ...
    )
  }
  out
}


#' @export
#' @rdname p_map
#' @inheritParams p_direction
p_map.data.frame <- function(x, null = 0, precision = 2^10, method = "kernel", rvar_col = NULL, ...) {
  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::p_map
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    obj_name <- insight::safe_deparse_symbol(substitute(x))
    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x))
  }

  x <- .select_nums(x)

  if (ncol(x) == 1) {
    p_MAP <- .p_map(x[, 1], null = null, precision = precision, method = method, ...)
  } else {
    p_MAP <- sapply(x, .p_map, null = null, precision = precision, method = method, simplify = TRUE, ...)
  }

  out <- data.frame(
    Parameter = names(x),
    p_MAP = p_MAP,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(out) <- c("p_map", class(out))
  out
}


#' @export
p_map.draws <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  p_map(.posterior_draws_to_df(x), null = null, precision = precision, method = method, ...)
}

#' @export
p_map.rvar <- p_map.draws


#' @export
p_map.emmGrid <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  xdf <- insight::get_parameters(x)
  out <- p_map(xdf, null = null, precision = precision, method = method, ...)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_map.emm_list <- p_map.emmGrid

#' @export
p_map.slopes <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  xrvar <- .get_marginaleffects_draws(x)
  out <- p_map(xrvar, null = null, precision = precision, method = method, ...)
  out <- .append_datagrid(out, x)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_map.comparisons <- p_map.slopes

#' @export
p_map.predictions <- p_map.slopes


#' @keywords internal
.p_map_models <- function(x, null, precision, method, effects, component, parameters, ...) {
  p_map(
    insight::get_parameters(
      x,
      effects = effects,
      component = component,
      parameters = parameters
    ),
    null = null,
    precision = precision,
    method = method,
    ...
  )
}


#' @export
p_map.mcmc <- function(x,
                       null = 0,
                       precision = 2^10,
                       method = "kernel",
                       parameters = NULL,
                       ...) {
  out <- .p_map_models(
    x = x,
    null = null,
    precision = precision,
    method = method,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}


#' @export
p_map.bcplm <- p_map.mcmc

#' @export
p_map.blrm <- p_map.mcmc

#' @export
p_map.mcmc.list <- p_map.mcmc

#' @export
p_map.BGGM <- p_map.mcmc


#' @export
p_map.bamlss <- function(x,
                         null = 0,
                         precision = 2^10,
                         method = "kernel",
                         component = "all",
                         parameters = NULL,
                         ...) {
  out <- .p_map_models(
    x = x,
    null = null,
    precision = precision,
    method = method,
    effects = "all",
    component = component,
    parameters = parameters,
    ...
  )

  out <- .add_clean_parameters_attribute(out, x)
  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}


#' @export
p_map.sim.merMod <- function(x,
                             null = 0,
                             precision = 2^10,
                             method = "kernel",
                             effects = "fixed",
                             parameters = NULL,
                             ...) {
  out <- .p_map_models(
    x = x,
    null = null,
    precision = precision,
    method = method,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(
    x,
    effects = effects,
    parameters = parameters
  )
  out
}


#' @export
p_map.sim <- function(x, null = 0, precision = 2^10, method = "kernel",
                      parameters = NULL, ...) {
  out <- .p_map_models(
    x = x,
    null = null,
    precision = precision,
    method = method,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    ...
  )

  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}


#' @export
p_map.stanreg <- function(x,
                          null = 0,
                          precision = 2^10,
                          method = "kernel",
                          effects = "fixed",
                          component = "location",
                          parameters = NULL,
                          ...) {
  cleaned_parameters <- .get_cleaned_parameters(x, ...)

  out <- .prepare_output(
    p_map(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters
      ),
      null = null,
      precision = precision,
      method = method
    ),
    cleaned_parameters,
    inherits(x, "stanmvreg")
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("p_map", class(out)))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
p_map.stanfit <- p_map.stanreg

#' @export
p_map.CmdStanFit <- p_map.stanreg


#' @export
p_map.blavaan <- p_map.stanreg


#' @rdname p_map
#' @export
p_map.brmsfit <- function(x,
                          null = 0,
                          precision = 2^10,
                          method = "kernel",
                          effects = "fixed",
                          component = "conditional",
                          parameters = NULL,
                          ...) {
  cleaned_parameters <- .get_cleaned_parameters(x, ...)

  out <- .prepare_output(
    p_map(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters,
        ...
      ),
      null = null,
      precision = precision,
      method = method,
      ...
    ),
    cleaned_parameters
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("p_map", class(out)))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_map.BFBayesFactor <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  out <- p_map(insight::get_parameters(x), null = null, precision = precision, method = method, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_map.MCMCglmm <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  nF <- x$Fixed$nfl
  out <- p_map(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), null = null, precision = precision, method = method, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
p_map.bayesQR <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  out <- p_map(insight::get_parameters(x), null = null, precision = precision, method = method, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @keywords internal
.p_map <- function(x, null = 0, precision = 2^10, method = "kernel", ...) {
  # Density at MAP
  map <- attributes(map_estimate(x, precision = precision, method = method, ...))$MAP_density

  # Density at 0
  d_0 <- density_at(x, null, precision = precision, method = method, ...)
  if (is.na(d_0)) d_0 <- 0

  # Odds
  p <- d_0 / map
  p
}


#' @rdname as.numeric.p_direction
#' @method as.numeric p_map
#' @export
as.numeric.p_map <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    return(as.numeric(as.vector(x$p_MAP)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_map
#' @export
as.double.p_map <- as.numeric.p_map
