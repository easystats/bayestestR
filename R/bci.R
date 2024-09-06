#' Bias Corrected and Accelerated Interval (BCa)
#'
#' Compute the **Bias Corrected and Accelerated Interval (BCa)** of posterior
#' distributions.
#'
#' @inheritParams hdi
#' @inherit ci return
#' @inherit hdi details
#' @inherit hdi seealso
#' @family ci
#'
#' @references
#' DiCiccio, T. J. and B. Efron. (1996). Bootstrap Confidence Intervals.
#' Statistical Science. 11(3): 189–212. 10.1214/ss/1032280214
#'
#' @examples
#' posterior <- rnorm(1000)
#' bci(posterior)
#' bci(posterior, ci = c(0.80, 0.89, 0.95))
#' @export
bci <- function(x, ...) {
  UseMethod("bci")
}

#' @rdname bci
#' @export
bcai <- bci



#' @rdname bci
#' @export
bci.numeric <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .bci(x = x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_eti", "see_eti", "bayestestR_ci", "see_ci", class(out)))
  attr(out, "data") <- x
  out
}



#' @rdname bci
#' @inheritParams p_direction
#' @export
bci.data.frame <- function(x, ci = 0.95, rvar_col = NULL, verbose = TRUE, ...) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::bci
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x, long = length(ci) > 1L))
  }

  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "bci")
  attr(dat, "object_name") <- obj_name
  dat
}



#' @export
bci.draws <- function(x, ci = 0.95, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = .posterior_draws_to_df(x), ci = ci, verbose = verbose, fun = "bci")
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
bci.rvar <- bci.draws


#' @rdname bci
#' @export
bci.MCMCglmm <- function(x, ci = 0.95, verbose = TRUE, ...) {
  nF <- x$Fixed$nfl
  d <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "bci")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}



#' @export
bci.mcmc <- function(x, ci = 0.95, verbose = TRUE, ...) {
  d <- as.data.frame(x)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "bci")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}



#' @export
bci.bamlss <- function(x,
                       ci = 0.95,
                       component = c("all", "conditional", "location"),
                       verbose = TRUE,
                       ...) {
  component <- match.arg(component)
  d <- insight::get_parameters(x, component = component)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "bci")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}



#' @export
bci.bcplm <- function(x, ci = 0.95, verbose = TRUE, ...) {
  d <- insight::get_parameters(x)
  dat <- .compute_interval_dataframe(x = d, ci = ci, verbose = verbose, fun = "bci")
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
bci.bayesQR <- bci.bcplm

#' @export
bci.blrm <- bci.bcplm

#' @export
bci.mcmc.list <- bci.bcplm

#' @export
bci.BGGM <- bci.bcplm



#' @rdname bci
#' @export
bci.sim.merMod <- function(x,
                           ci = 0.95,
                           effects = c("fixed", "random", "all"),
                           parameters = NULL,
                           verbose = TRUE,
                           ...) {
  effects <- match.arg(effects)
  dat <- .compute_interval_simMerMod(
    x = x,
    ci = ci,
    effects = effects,
    parameters = parameters,
    verbose = verbose,
    fun = "bci"
  )
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}



#' @rdname bci
#' @export
bci.sim <- function(x, ci = 0.95, parameters = NULL, verbose = TRUE, ...) {
  dat <- .compute_interval_sim(
    x = x,
    ci = ci,
    parameters = parameters,
    verbose = verbose,
    fun = "bci"
  )
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}



#' @rdname bci
#' @export
bci.emmGrid <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x)
  dat <- bci(xdf, ci = ci, verbose = verbose, ...)
  dat <- .append_datagrid(dat, x, long = length(ci) > 1L)
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
bci.emm_list <- bci.emmGrid

#' @rdname bci
#' @export
bci.slopes <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xrvar <- .get_marginaleffects_draws(x)
  dat <- bci(xrvar, ci = ci, verbose = verbose, ...)
  dat <- .append_datagrid(dat, x, long = length(ci) > 1L)
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
bci.comparisons <- bci.slopes

#' @export
bci.predictions <- bci.slopes

#' @rdname bci
#' @export
bci.stanreg <- function(x,
                        ci = 0.95,
                        effects = c("fixed", "random", "all"),
                        component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                        parameters = NULL,
                        verbose = TRUE,
                        ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .prepare_output(
    bci(
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
bci.stanfit <- bci.stanreg

#' @export
bci.blavaan <- bci.stanreg



#' @rdname bci
#' @export
bci.brmsfit <- function(x, ci = 0.95, effects = c("fixed", "random", "all"),
                        component = c("conditional", "zi", "zero_inflated", "all"),
                        parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .prepare_output(
    bci(
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



#' @rdname bci
#' @export
bci.BFBayesFactor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- bci(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @rdname bci
#' @export
bci.get_predicted <- function(x, ci = 0.95, use_iterations = FALSE, verbose = TRUE, ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- bci(as.data.frame(t(attributes(x)$iterations)), ci = ci, verbose = verbose, ...)
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- bci(as.numeric(x), ci = ci, verbose = verbose, ...)
  }
  out
}

# Helper ------------------------------------------------------------------


.bci <- function(x, ci, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  low <- (1 - ci) / 2
  high <- 1 - low
  sims <- length(x)
  z.inv <- length(x[x < mean(x, na.rm = TRUE)]) / sims

  z <- stats::qnorm(z.inv)
  U <- (sims - 1) * (mean(x, na.rm = TRUE) - x)
  top <- sum(U^3)
  under <- 6 * (sum(U^2))^1.5
  a <- top / under

  lower.inv <- stats::pnorm(z + (z + stats::qnorm(low)) / (1 - a * (z + stats::qnorm(low))))
  lower <- stats::quantile(x, lower.inv, names = FALSE, na.rm = TRUE)
  upper.inv <- stats::pnorm(z + (z + stats::qnorm(high)) / (1 - a * (z + stats::qnorm(high))))
  upper <- stats::quantile(x, upper.inv, names = FALSE, na.rm = TRUE)

  data.frame(
    CI = ci,
    CI_low = lower,
    CI_high = upper
  )
}
