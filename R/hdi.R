#' Highest Density Interval (HDI)
#'
#' Compute the Highest Density Interval (HDI) of a posterior distribution, i.e., the interval which contains all points within the interval have a higher probability density than points outside the interval. The HDI is used in the context of Bayesian posterior characterisation as Credible Interval (CI).
#'
#' @details Unlike equal-tailed intervals that typically exclude 2.5\% from each tail
#'   of the distribution, the HDI is \emph{not} equal-tailed and therefor always
#'   includes the mode(s) of posterior distributions.
#'   \cr \cr
#'   By default, \code{hdi()} returns the 90\% intervals (\code{ci = 0.9}),
#'   deemed to be more stable than, for instance, 95\% intervals (\cite{Kruschke, 2015}).
#'   An effective sample size of at least 10.000 is recommended if 95\% intervals
#'   should be computed (\cite{Kruschke 2015, p. 183ff}).
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param ci Value or vector of HDI probability (between 0 and 1) to be estimated. Named Credible Interval (CI) for consistency.
#' @param effects Should results for fixed effects, random effects or both be returned?
#'   Only applies to mixed models. May be abbreviated.
#' @param component Should results for all parameters, parameters for the conditional model
#'   or the zero-inflated part of the modelbe returned? May be abbreviated. Only
#'   applies to \pkg{brms}-models.
#' @param parameters Regular expression pattern that describes the parameters that
#'   should be returned. Meta-parameters (like \code{lp__} or \code{prior_}) are
#'   filtered by default, so only parameters that typically appear in the
#'   \code{summary()} are returned. Use \code{parameters} to select specific parameters
#'   for the output.
#' @param verbose Toggle off warnings.
#' @param ... Currently not used.
#'
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' hdi(posterior, ci = .90)
#' hdi(posterior, ci = c(.80, .90, .95))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' hdi(model)
#' hdi(model, ci = c(.80, .90, .95))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' hdi(model)
#' hdi(model, ci = c(.80, .90, .95))
#' }
#'
#' @author Credits go to \href{https://rdrr.io/cran/ggdistribute/src/R/stats.R}{ggdistribute} and \href{https://github.com/mikemeredith/HDInterval}{HDInterval}.
#'
#' @references Kruschke, J. (2015). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press.
#'
#' @export
hdi <- function(posterior, ...) {
  UseMethod("hdi")
}


#' @rdname hdi
#' @export
hdi.numeric <- function(posterior, ci = .90, verbose = TRUE, ...) {
  do.call(rbind, lapply(ci, function(i) {
    .hdi(posterior, ci = i, verbose = verbose)
  }))
}


#' @importFrom insight get_parameters
#' @rdname hdi
#' @export
hdi.stanreg <- function(posterior, ci = .90, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)

  list <- lapply(c("fixed", "random"), function(x) {
    parms <- insight::get_parameters(posterior, effects = x, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      hdi,
      ci = ci,
      verbose = verbose,
      simplify = FALSE)
    )

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_stanreg(
        tmp,
        x,
        cols = c("CI", "CI_low", "CI_high", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    tmp
  })

  dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))

  dat <- switch(
    effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- .remove_column(dat, "Group")
  }

  class(dat) <- c("hdi", class(dat))
  dat
}


#' @rdname hdi
#' @export
hdi.brmsfit <- function(posterior, ci = .90, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  .get_hdi <- function(x, y) {
    parms <- insight::get_parameters(posterior, effects = x, component = y, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      hdi,
      ci = ci,
      verbose = verbose,
      simplify = FALSE)
    )

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_brms(
        tmp,
        x,
        y,
        cols = c("CI", "CI_low", "CI_high", "Component", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    tmp
  }

  list <- mapply(.get_hdi, eff, com, SIMPLIFY = FALSE)
  dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))

  dat <- switch(
    effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  dat <- switch(
    component,
    conditional = .select_rows(dat, "Component", "conditional"),
    zi = ,
    zero_inflated = .select_rows(dat, "Component", "zero_inflated"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- .remove_column(dat, "Group")
  }

  if (all(dat$Component == dat$Component[1])) {
    dat <- .remove_column(dat, "Component")
  }

  class(dat) <- c("hdi", class(dat))
  dat
}











#' @keywords internal
.hdi <- function(x, ci = .90, verbose = TRUE) {
  if (ci > 1) {
    if (verbose) {
      warning("HDI: `ci` should be less than 1, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }


  if (ci == 1) {
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = min(x, na.rm = TRUE),
      "CI_high" = max(x, na.rm = TRUE)
    ))
  }

  if (anyNA(x)) {
    if (verbose) {
      warning("HDI: the posterior contains NAs, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  N <- length(x)
  if (N < 3) {
    if (verbose) {
      warning("HDI: the posterior is too short, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  # x_sorted <- sort(x)
  x_sorted <- sort.int(x, method = "quick")  # removes NA/NaN, but not Inf
  window_size <- ceiling(ci * length(x_sorted))

  if (window_size < 2) {
    if (verbose) {
      warning("HDI: `ci` is too small or x does not contain enough data points, returning NAs.")
    }
    return(data.frame(
      "CI" = ci * 100,
      "CI_low" = NA,
      "CI_high" = NA
    ))
  }

  nCIs <- length(x_sorted) - window_size
  ci.width <- sapply(1:nCIs, function(.x) x_sorted[.x + window_size] - x_sorted[.x])

  data.frame(
    "CI" = ci * 100,
    "CI_low" = x_sorted[which.min(ci.width)],
    "CI_high" = x_sorted[which.min(ci.width) + window_size]
  )
}

