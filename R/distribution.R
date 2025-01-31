#' Empirical Distributions
#'
#' Generate a sequence of n-quantiles, i.e., a sample of size `n` with a
#' near-perfect distribution.
#'
#' @param type Can be any of the names from base R's
#'   [Distributions][stats::Distributions], like `"cauchy"`, `"pois"` or
#'   `"beta"`.
#' @param random Generate near-perfect or random (simple wrappers for the base R
#'   `r*` functions) distributions.
#' @param xi For tweedie distributions, the value of `xi` such that the variance
#' is `var(Y) = phi * mu^xi`.
#' @param power Alias for `xi`.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams tweedie::rtweedie
#'
#' @details
#' When `random = FALSE`, these function return `q*(ppoints(n), ...)`.
#'
#' @examples
#' library(bayestestR)
#' x <- distribution(n = 10)
#' plot(density(x))
#'
#' x <- distribution(type = "gamma", n = 100, shape = 2)
#' plot(density(x))
#' @export
distribution <- function(type = "normal", ...) {
  basr_r_distributions <- c(
    "beta", "binom", "binomial", "cauchy", "chisq", "chisquared", "exp", "f",
    "gamma", "geom", "hyper", "lnorm", "multinom", "nbinom", "normal", "gaussian",
    "pois", "poisson", "student", "t", "student_t", "unif", "uniform", "weibull"
  )

  switch(match.arg(arg = type, choices = basr_r_distributions),
    beta = distribution_beta(...),
    binom = ,
    binomial = distribution_binomial(...),
    cauchy = distribution_cauchy(...),
    chisq = ,
    chisquared = distribution_chisquared(...),
    gamma = distribution_gamma(...),
    gaussian = ,
    normal = distribution_normal(...),
    nbinom = distribution_nbinom(...),
    poisson = distribution_poisson(...),
    t = ,
    student = ,
    student_t = distribution_student(...),
    uniform = distribution_uniform(...),
    distribution_custom(type = type, ...)
  )
}


#' @rdname distribution
#' @inheritParams distribution
#' @export
distribution_custom <- function(n, type = "norm", ..., random = FALSE) {
  if (random) {
    f <- match.fun(paste0("r", type))
    f(n, ...)
  } else {
    f <- match.fun(paste0("q", type))
    f(stats::ppoints(n), ...)
  }
}


#' @rdname distribution
#' @inheritParams stats::rbeta
#' @export
distribution_beta <- function(n, shape1, shape2, ncp = 0, random = FALSE, ...) {
  if (random) {
    stats::rbeta(n, shape1, shape2, ncp = ncp)
  } else {
    stats::qbeta(stats::ppoints(n), shape1, shape2, ncp = ncp, ...)
  }
}

#' @rdname distribution
#' @inheritParams stats::rbinom
#' @export
distribution_binomial <- function(n, size = 1, prob = 0.5, random = FALSE, ...) {
  if (random) {
    stats::rbinom(n, size, prob)
  } else {
    stats::qbinom(stats::ppoints(n), size, prob, ...)
  }
}


#' @rdname distribution
#' @export
distribution_binom <- distribution_binomial


#' @rdname distribution
#' @inheritParams stats::rcauchy
#' @export
distribution_cauchy <- function(n, location = 0, scale = 1, random = FALSE, ...) {
  if (random) {
    stats::rcauchy(n, location, scale)
  } else {
    stats::qcauchy(stats::ppoints(n), location, scale, ...)
  }
}

#' @rdname distribution
#' @inheritParams stats::rchisq
#' @export
distribution_chisquared <- function(n, df, ncp = 0, random = FALSE, ...) {
  if (random) {
    stats::rchisq(n, df, ncp)
  } else {
    stats::qchisq(stats::ppoints(n), df, ncp, ...)
  }
}

#' @rdname distribution
#' @export
distribution_chisq <- distribution_chisquared


#' @rdname distribution
#' @inheritParams stats::rgamma
#' @param shape Shape parameter.
#' @export
distribution_gamma <- function(n, shape, scale = 1, random = FALSE, ...) {
  if (random) {
    stats::rgamma(n = n, shape = shape, scale = scale)
  } else {
    stats::qgamma(p = stats::ppoints(n), shape = shape, scale = scale)
  }
}


#' @rdname distribution
#' @inheritParams stats::rnorm
#' @export
distribution_mixture_normal <- function(n, mean = c(-3, 3), sd = 1, random = FALSE, ...) {
  n <- round(n / length(mean))
  sd <- sd
  if (length(sd) != length(mean)) {
    sd <- rep_len(sd, length(mean))
  }


  x <- NULL
  for (i in seq_along(mean)) {
    x <- c(x, distribution_normal(n = n, mean = mean[i], sd = sd[i], random = random))
  }
  x
}

#' @rdname distribution
#' @inheritParams stats::rnorm
#' @export
distribution_normal <- function(n, mean = 0, sd = 1, random = FALSE, ...) {
  if (random) {
    stats::rnorm(n, mean, sd)
  } else {
    stats::qnorm(stats::ppoints(n), mean, sd, ...)
  }
}

#' @rdname distribution
#' @export
distribution_gaussian <- distribution_normal


#' @rdname distribution
#' @inheritParams stats::rnbinom
#' @param phi Corresponding to `glmmTMB`'s implementation of nbinom
#'   distribution, where `size=mu/phi`.
#' @export
distribution_nbinom <- function(n, size, prob, mu, phi, random = FALSE, ...) {
  if (missing(size)) {
    size <- mu / phi
  }

  if (random) {
    stats::rnbinom(n, size, prob, mu)
  } else {
    stats::qnbinom(stats::ppoints(n), size, prob, mu, ...)
  }
}


#' @rdname distribution
#' @inheritParams stats::rpois
#' @export
distribution_poisson <- function(n, lambda = 1, random = FALSE, ...) {
  if (random) {
    stats::rpois(n, lambda)
  } else {
    stats::qpois(stats::ppoints(n), lambda, ...)
  }
}


#' @rdname distribution
#' @inheritParams stats::rt
#' @export
distribution_student <- function(n, df, ncp, random = FALSE, ...) {
  if (random) {
    stats::rt(n, df, ncp)
  } else {
    stats::qt(stats::ppoints(n), df, ncp, ...)
  }
}

#' @rdname distribution
#' @export
distribution_t <- distribution_student

#' @rdname distribution
#' @export
distribution_student_t <- distribution_student

#' @rdname distribution
#' @inheritParams tweedie::rtweedie
#' @export
distribution_tweedie <- function(n,
                                 xi = NULL,
                                 mu,
                                 phi,
                                 power = NULL,
                                 random = FALSE,
                                 ...) {
  insight::check_if_installed("tweedie")

  if (random) {
    tweedie::rtweedie(
      n = n,
      xi = xi,
      mu = mu,
      phi = phi,
      power = power
    )
  } else {
    tweedie::qtweedie(
      p = stats::ppoints(n),
      xi = xi,
      mu = mu,
      phi = phi,
      power = power
    )
  }
}

#' @rdname distribution
#' @inheritParams stats::runif
#' @export
distribution_uniform <- function(n, min = 0, max = 1, random = FALSE, ...) {
  if (random) {
    stats::runif(n, min, max)
  } else {
    stats::qunif(stats::ppoints(n), min, max, ...)
  }
}


#' @rdname distribution
#' @inheritParams stats::rnorm
#' @export
rnorm_perfect <- function(n, mean = 0, sd = 1) {
  .Deprecated("distribution_normal")
  stats::qnorm(stats::ppoints(n), mean, sd)
}
