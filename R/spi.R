#' Shortest Probability Interval (SPI)
#'
#' Compute the **Shortest Probability Interval (SPI)** of posterior distributions.
#'
#' @inheritParams hdi
#' @inherit ci return
#' @inherit hdi details
#' @inherit hdi seealso
#' @family ci
#'
#' @references http://www.stat.columbia.edu/~gelman/research/published/spin.pdf
#'
#' @examples
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' spi(posterior)
#' spi(posterior, ci = c(.80, .89, .95))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' spi(df)
#' spi(df, ci = c(.80, .89, .95))
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' spi(model)
#' }
#'
#' @export
spi <- function(x, ...) {
  UseMethod("spi")
}


#' @rdname spi
#' @export
spi.numeric <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .spi(x = x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_ci", "see_ci", class(out)))
  attr(out, "data") <- x
  out
}


#' @rdname spi
#' @export
spi.data.frame <- function(x, ci = 0.95, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "spi")
  attr(dat, "object_name") <- insight::safe_deparse(substitute(x))
  dat
}


#' @export
spi.MCMCglmm <- function(x, ci = 0.95, verbose = TRUE, ...) {
  hdi(x, ci = ci, verbose = verbose, ci_method = "spi", ...)
}


#' @export
spi.bamlss <- function(x,
                       ci = 0.95,
                       component = c("all", "conditional", "location"),
                       verbose = TRUE,
                       ...) {
  component <- match.arg(component)
  hdi(x, ci = ci, component = component, verbose = verbose, ci_method = "spi")
}


#' @export
spi.mcmc <- function(x, ci = 0.95, verbose = TRUE, ...) {
  hdi(x, ci = ci, verbose = verbose, ci_method = "spi", ...)
}

#' @export
spi.bcplm <- spi.mcmc

#' @export
spi.bayesQR <- spi.mcmc

#' @export
spi.blrm <- spi.mcmc

#' @export
spi.mcmc.list <- spi.mcmc

#' @export
spi.BGGM <- spi.mcmc

#' @export
spi.sim.merMod <- function(x,
                           ci = 0.95,
                           effects = c("fixed", "random", "all"),
                           parameters = NULL,
                           verbose = TRUE,
                           ...) {
  effects <- match.arg(effects)
  hdi(x, ci = ci, effects = effects, parameters = parameters, verbose = verbose, ci_method = "spi", ...)
}

#' @export
spi.sim <- function(x, ci = 0.95, parameters = NULL, verbose = TRUE, ...) {
  hdi(x, ci = ci, parameters = parameters, verbose = verbose, ci_method = "spi", ...)
}

#' @export
spi.emmGrid <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x)
  out <- spi(xdf, ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- insight::safe_deparse(substitute(x))
  out
}

#' @export
spi.emm_list <- spi.emmGrid


#' @rdname spi
#' @export
spi.stanreg <- function(x,
                        ci = 0.95,
                        effects = c("fixed", "random", "all"),
                        component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                        parameters = NULL,
                        verbose = TRUE,
                        ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    spi(
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
    cleaned_parameters,
    inherits(x, "stanmvreg")
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "object_name") <- insight::safe_deparse(substitute(x))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", class(out)))
  out
}

#' @export
spi.stanfit <- spi.stanreg

#' @export
spi.blavaan <- spi.stanreg


#' @rdname spi
#' @export
spi.brmsfit <- function(x,
                        ci = 0.95,
                        effects = c("fixed", "random", "all"),
                        component = c("conditional", "zi", "zero_inflated", "all"),
                        parameters = NULL,
                        verbose = TRUE,
                        ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    spi(
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
    cleaned_parameters
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "object_name") <- insight::safe_deparse(substitute(x))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", class(out)))
  out
}


#' @export
spi.BFBayesFactor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- spi(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- insight::safe_deparse(substitute(x))
  out
}


#' @export
spi.get_predicted <- function(x, ...) {
  if ("iterations" %in% names(attributes(x))) {
    out <- spi(as.data.frame(t(attributes(x)$iterations)), ...)
  } else {
    stop("No iterations present in the output.")
  }
  attr(out, "object_name") <- insight::safe_deparse(substitute(x))
  out
}




# Helper ------------------------------------------------------------------


.spi <- function(x, ci, verbose = TRUE) {
  insight::check_if_installed("SPIn")
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  results <- SPIn::SPIn(x, conf = ci)

  data.frame(
    "CI" = ci,
    "CI_low" = results$spin[1],
    "CI_high" = results$spin[2]
  )
}
