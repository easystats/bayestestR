#' Equal-Tailed Interval (ETI)
#'
#' Compute the **Equal-Tailed Interval (ETI)** of posterior distributions using
#' the quantiles method. The probability of being below this interval is equal
#' to the probability of being above it. The ETI can be used in the context of
#' uncertainty characterisation of posterior distributions as
#' **Credible Interval (CI)**.
#'
#' @inheritParams hdi
#' @inherit ci return
#' @inherit hdi details
#' @inherit hdi seealso
#' @family ci
#'
#' @inheritSection hdi Model components
#'
#' @examplesIf require("rstanarm") && require("emmeans") && require("brms") && require("BayesFactor")
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' eti(posterior)
#' eti(posterior, ci = c(0.80, 0.89, 0.95))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' eti(df)
#' eti(df, ci = c(0.80, 0.89, 0.95))
#' \donttest{
#' model <- suppressWarnings(
#'   rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' eti(model)
#' eti(model, ci = c(0.80, 0.89, 0.95))
#'
#' eti(emmeans::emtrends(model, ~1, "wt", data = mtcars))
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' eti(model)
#' eti(model, ci = c(0.80, 0.89, 0.95))
#'
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' eti(bf)
#' eti(bf, ci = c(0.80, 0.89, 0.95))
#' }
#'
#' @export
eti <- function(x, ...) {
  UseMethod("eti")
}


#' @export
eti.default <- function(x, ...) {
  insight::format_error(paste0("'eti()' is not yet implemented for objects of class '", class(x)[1], "'."))
}


#' @rdname eti
#' @export
eti.numeric <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .eti(x = x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_eti", "see_eti", "bayestestR_ci", "see_ci", class(out)))
  attr(out, "data") <- x
  out
}


#' @export
#' @rdname eti
#' @inheritParams p_direction
eti.data.frame <- function(x, ci = 0.95, rvar_col = NULL, verbose = TRUE, ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::eti
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x, long = length(ci) > 1L))
  }


  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "object_name") <- obj_name
  dat
}


#' @export
eti.draws <- function(x, ci = 0.95, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = .posterior_draws_to_df(x), ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
eti.rvar <- eti.draws


#' @export
eti.MCMCglmm <- function(x, ci = 0.95, verbose = TRUE, ...) {
  nF <- x$Fixed$nfl
  d <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
eti.mcmc <- function(x, ci = 0.95, verbose = TRUE, ...) {
  d <- as.data.frame(x)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
eti.bamlss <- function(x, ci = 0.95, component = "all", verbose = TRUE, ...) {
  d <- insight::get_parameters(x, component = component)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
eti.bcplm <- function(x, ci = 0.95, verbose = TRUE, ...) {
  d <- insight::get_parameters(x)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "eti")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
eti.bayesQR <- eti.bcplm

#' @export
eti.blrm <- eti.bcplm

#' @export
eti.mcmc.list <- eti.bcplm

#' @export
eti.BGGM <- eti.bcplm


#' @export
eti.sim.merMod <- function(x,
                           ci = 0.95,
                           effects = "fixed",
                           parameters = NULL,
                           verbose = TRUE,
                           ...) {
  dat <- .compute_interval_simMerMod(
    x = x,
    ci = ci,
    effects = effects,
    parameters = parameters,
    verbose = verbose,
    fun = "eti"
  )
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}


#' @export
eti.sim <- function(x, ci = 0.95, parameters = NULL, verbose = TRUE, ...) {
  dat <- .compute_interval_sim(
    x = x,
    ci = ci,
    parameters = parameters,
    verbose = verbose,
    fun = "eti"
  )
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}


#' @export
eti.emmGrid <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x)
  dat <- eti(xdf, ci = ci, verbose = verbose, ...)
  dat <- .append_datagrid(dat, x, long = length(ci) > 1L)
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
eti.emm_list <- eti.emmGrid


#' @export
eti.slopes <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xrvar <- .get_marginaleffects_draws(x)
  dat <- eti(xrvar, ci = ci, verbose = verbose, ...)
  dat <- .append_datagrid(dat, x, long = length(ci) > 1L)
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
eti.comparisons <- eti.slopes

#' @export
eti.predictions <- eti.slopes


#' @export
eti.stanreg <- function(x,
                        ci = 0.95,
                        effects = "fixed",
                        component = "location",
                        parameters = NULL,
                        verbose = TRUE,
                        ...) {
  out <- .prepare_output(
    eti(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters
      ),
      ci = ci,
      verbose = verbose,
      ...
    ),
    insight::clean_parameters(x),
    inherits(x, "stanmvreg")
  )

  class(out) <- unique(c("bayestestR_eti", "see_eti", class(out)))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
eti.stanfit <- eti.stanreg

#' @export
eti.blavaan <- eti.stanreg


#' @rdname eti
#' @export
eti.brmsfit <- function(x,
                        ci = 0.95,
                        effects = "fixed",
                        component = "conditional",
                        parameters = NULL,
                        verbose = TRUE,
                        ...) {
  out <- .prepare_output(
    eti(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters
      ),
      ci = ci,
      verbose = verbose,
      ...
    ),
    insight::clean_parameters(x)
  )

  class(out) <- unique(c("bayestestR_eti", "see_eti", class(out)))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
eti.BFBayesFactor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- eti(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @rdname eti
#' @export
eti.get_predicted <- function(x, ci = 0.95, use_iterations = FALSE, verbose = TRUE, ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- eti(as.data.frame(t(attributes(x)$iterations)), ci = ci, verbose = verbose, ...)
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- eti(as.numeric(x), ci = ci, verbose = verbose, ...)
  }
  out
}


# Helper ------------------------------------------------------------------


.eti <- function(x, ci, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  results <- as.vector(stats::quantile(
    x,
    probs = c((1 - ci) / 2, (1 + ci) / 2),
    names = FALSE,
    na.rm = TRUE
  ))

  data.frame(
    CI = ci,
    CI_low = results[1],
    CI_high = results[2]
  )
}
