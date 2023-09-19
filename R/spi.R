#' Shortest Probability Interval (SPI)
#'
#' Compute the **Shortest Probability Interval (SPI)** of posterior distributions.
#' The SPI is a more computationally stable HDI. The implementation is based on
#' the algorithm from the **SPIn** package.
#'
#' @inheritParams hdi
#' @inherit ci return
#' @inherit hdi seealso
#' @family ci
#'
#' @note The code to compute the SPI was adapted from the **SPIn** package,
#' and slightly modified to be more robust for Stan models. Thus, credits go
#' to Ying Liu for the original SPI algorithm and R implementation.
#'
#' @details The SPI is an alternative method to the HDI ([hdi()]) to quantify
#' uncertainty of (posterior) distributions. The SPI is said to be more stable
#' than the HDI, because, the _"HDI can be noisy (that is, have a high Monte Carlo error)"_
#' (Liu et al. 2015). Furthermore, the HDI is sensitive to additional assumptions,
#' in particular assumptions related to the different estimation methods, which
#' can make the HDI less accurate or reliable.
#'
#' @references
#' Liu, Y., Gelman, A., & Zheng, T. (2015). Simulation-efficient shortest probability intervals. Statistics and Computing, 25(4), 809–819. https://doi.org/10.1007/s11222-015-9563-8
#'
#' @examplesIf requireNamespace("quadprog", quietly = TRUE)
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' spi(posterior)
#' spi(posterior, ci = c(0.80, 0.89, 0.95))
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' spi(df)
#' spi(df, ci = c(0.80, 0.89, 0.95))
#' \donttest{
#' library(rstanarm)
#' model <- suppressWarnings(
#'   stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' spi(model)
#' }
#'
#' @export
spi <- function(x, ...) {
  UseMethod("spi")
}


#' @export
spi.default <- function(x, ...) {
  insight::format_error(paste0("'spi()' is not yet implemented for objects of class '", class(x)[1], "'."))
}


#' @rdname spi
#' @export
spi.numeric <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- do.call(rbind, lapply(ci, function(i) {
    .spi(x = x, ci = i, verbose = verbose)
  }))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_ci", "see_ci", "bayestestR_spi", class(out)))
  attr(out, "data") <- x
  out
}


#' @export
spi.data.frame <- function(x, ci = 0.95, verbose = TRUE, ...) {
  dat <- .compute_interval_dataframe(x = x, ci = ci, verbose = verbose, fun = "spi")
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_spi", class(out)))
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- unique(c("bayestestR_hdi", "see_hdi", "bayestestR_spi", class(out)))
  out
}


#' @export
spi.BFBayesFactor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- spi(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @export
spi.get_predicted <- function(x, ...) {
  if ("iterations" %in% names(attributes(x))) {
    out <- spi(as.data.frame(t(attributes(x)$iterations)), ...)
  } else {
    insight::format_error("No iterations present in the output.")
  }
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}




# Helper ------------------------------------------------------------------

# Code taken (and slightly simplified) from:
# SPIn::SPIn()
# Author: Ying Liu yliu@stat.columbia.edu
# Reference: Simulation efficient shortest probability intervals. (arXiv:1302.2142)
# Code licensed under License: GPL (>= 2)
.spi <- function(x, ci, verbose = TRUE) {
  insight::check_if_installed("quadprog")
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  dens <- stats::density(x)
  n.sims <- length(x)
  conf <- 1 - ci
  nn <- round(n.sims * conf)
  # sanity check for very low CI levels
  if (nn >= n.sims) {
    nn <- n.sims <- 1
  }
  x <- sort(x)
  xx <- x[(n.sims - nn):n.sims] - x[1:(nn + 1)]
  m <- min(xx)
  k <- which(xx == m)[1]
  l <- x[k]
  ui <- n.sims - nn + k - 1
  u <- x[ui]
  bw <- round((sqrt(n.sims) - 1) / 2)
  k <- which(x == l)[1]
  ui <- which(x == u)[1]


  # lower bound
  if (!anyNA(k) && all(k == 1)) {
    x.l <- l
  } else {
    x.l <- .safe(.spi_lower(bw = bw, n.sims = n.sims, k = k, l = l, dens = dens, x = x))
    frac <- 1
    while (is.null(x.l)) {
      frac <- frac - 0.1
      x.l <- .safe(.spi_lower(bw = frac * bw, n.sims = n.sims, k = k, l = l, dens = dens, x = x))
      if (frac <= 0.1) {
        insight::format_alert("Could not find a solution for the SPI lower bound.")
        x.l <- NA
      }
    }
  }

  # upper bound
  if (!anyNA(ui) && all(ui == n.sims)) {
    x.u <- u
  } else {
    x.u <- .safe(.spi_upper(bw = bw, n.sims = n.sims, ui = ui, u = u, dens = dens, x = x))
    frac <- 1
    while (is.null(x.u)) {
      frac <- frac - 0.1
      x.u <- .safe(.spi_upper(bw = frac * bw, n.sims = n.sims, ui = ui, u = u, dens = dens, x = x))
      if (frac <= 0.1) {
        insight::format_alert("Could not find a solution for the SPI upper bound.")
        x.u <- NA
      }
    }
  }

  # output
  data.frame(
    "CI" = ci,
    "CI_low" = x.l,
    "CI_high" = x.u
  )
}

.spi_lower <- function(bw, n.sims, k, l, dens, x) {
  l.l <- max(1, k - bw)
  l.u <- k + (k - l.l)
  range_ll_lu <- l.u - l.l
  range_ll_k <- k - l.l
  n.l <- range_ll_lu + 1
  D.l <- matrix(nrow = n.l, ncol = n.l)

  # create quadratic function
  p <- (l.l:l.u) / (n.sims + 1)
  q <- 1 - p
  Q <- stats::quantile(x, p)
  d.q <- rep(0, n.l)
  for (r in 1:n.l) {
    d.q[r] <- dens$y[which.min(abs(dens$x - Q[r]))]
  }
  Q. <- 1 / d.q
  diag(D.l) <- 2 * (Q^2 + p * q * Q.^2 / (n.sims + 2))
  d.l <- 2 * Q * l
  if (n.l > 1) {
    for (j in 1:(n.l - 1)) {
      for (m in (j + 1):n.l) {
        D.l[j, m] <- Q.[j] * Q.[m] * p[j] * q[m] * 2 / (n.sims + 2) + Q[j] * Q[m] * 2
        D.l[m, j] <- D.l[j, m]
      }
    }
  }

  # create constraint matrix
  A.l <- matrix(0, nrow = range_ll_lu + 3, ncol = range_ll_lu + 1)
  A.l[1, ] <- 1
  if (bw > 1 && k > 2) {
    for (j in 1:(range_ll_k - 1)) {
      if (x[l.l + j + 1] == x[l.l + j]) {
        A.l[1 + j, j + 1] <- 1
        A.l[1 + j, j + 2] <- -1
      } else {
        aa <- (x[l.l + j] - x[l.l + j - 1]) / (x[l.l + j + 1] - x[l.l + j])
        A.l[1 + j, j] <- 1
        A.l[1 + j, j + 1] <- -(aa + 1)
        A.l[1 + j, j + 2] <- aa
      }
    }

    for (j in 0:(l.u - k - 2)) {
      if (x[k + j + 1] == x[k + j + 2]) {
        A.l[range_ll_k + 1 + j, range_ll_k + 2 + j] <- 1
        A.l[range_ll_k + 1 + j, range_ll_k + 3 + j] <- -1
      } else {
        aa <- (x[k + j] - x[k + j + 1]) / (x[k + j + 1] - x[k + j + 2])
        A.l[range_ll_k + 1 + j, range_ll_k + 1 + j] <- -1
        A.l[range_ll_k + 1 + j, range_ll_k + 2 + j] <- aa + 1
        A.l[range_ll_k + 1 + j, range_ll_k + 3 + j] <- -aa
      }
    }
  }
  if (x[k + 1] == x[k]) {
    aa <- (x[k] - x[k - 1]) / (x[k + 1] - x[k] + 0.000001)
  } else {
    aa <- (x[k] - x[k - 1]) / (x[k + 1] - x[k])
  }
  A.l[range_ll_lu, range_ll_k + 1] <- aa - 1
  A.l[range_ll_lu, range_ll_k] <- 1
  A.l[range_ll_lu, range_ll_k + 2] <- -aa

  A.l[range_ll_lu + 1, range_ll_lu] <- 1
  A.l[range_ll_lu + 1, range_ll_lu + 1] <- -1
  A.l[range_ll_lu + 2, 1] <- 1
  A.l[range_ll_lu + 3, range_ll_lu + 1] <- 1
  A.l <- t(A.l)

  w.l <- quadprog::solve.QP(D.l, d.l, A.l, c(1, rep(0, range_ll_lu + 2)), range_ll_lu)
  x.l <- w.l$solution %*% x[l.l:l.u]

  return(x.l)
}

.spi_upper <- function(bw, n.sims, ui, u, dens, x) {
  u.u <- min(n.sims, ui + bw)
  u.l <- ui - (u.u - ui)
  range_ul_uu <- u.u - u.l
  range_ul_ui <- ui - u.l
  n.u <- range_ul_uu + 1
  D.u <- matrix(nrow = n.u, ncol = n.u)

  # create quadratic function
  p <- (u.l:u.u) / (n.sims + 1)
  q <- 1 - p
  Q <- stats::quantile(x, p)
  d.q <- rep(0, n.u)
  for (r in 1:n.u) {
    d.q[r] <- dens$y[which.min(abs(dens$x - Q[r]))]
  }
  Q. <- 1 / d.q
  diag(D.u) <- 2 * (Q^2 + p * q * Q.^2 / (n.sims + 2))
  d.u <- 2 * Q * u
  if (n.u > 1) {
    for (j in 1:(n.u - 1)) {
      for (m in (j + 1):n.u) {
        D.u[j, m] <- Q.[j] * Q.[m] * p[j] * q[m] * 2 / (n.sims + 2) + Q[j] * Q[m] * 2
        D.u[m, j] <- D.u[j, m]
      }
    }
  }

  # create constraint matrix
  A.u <- matrix(0, nrow = range_ul_uu + 3, ncol = range_ul_uu + 1)
  A.u[1, ] <- 1
  if (bw > 1 && range_ul_ui > 1) {
    for (j in 1:(range_ul_ui - 1)) {
      if (x[u.l + j + 1] == x[u.l + j]) {
        A.u[1 + j, j + 1] <- 1
        A.u[1 + j, j + 2] <- -1
      } else {
        aa <- (x[u.l + j] - x[u.l + j - 1]) / (x[u.l + j + 1] - x[u.l + j])
        A.u[1 + j, j] <- 1
        A.u[1 + j, j + 1] <- -(aa + 1)
        A.u[1 + j, j + 2] <- aa
      }
    }

    i <- 0
    for (j in (range_ul_ui):(range_ul_uu - 2)) {
      if (x[ui + i + 1] == x[ui + i + 2]) {
        A.u[1 + j, j + 2] <- 1
        A.u[1 + j, j + 3] <- -1
      } else {
        aa <- (x[ui + i] - x[ui + i + 1]) / (x[ui + i + 1] - x[ui + i + 2])
        A.u[1 + j, j + 1] <- -1
        A.u[1 + j, j + 2] <- aa + 1
        A.u[1 + j, j + 3] <- -aa
      }
      i <- i + 1
    }
  }
  if (x[ui + 1] == x[ui]) {
    aa <- (x[ui] - x[ui - 1]) / (x[ui + 2] - x[ui])
    A.u[range_ul_uu, range_ul_ui] <- 1
    A.u[range_ul_uu, range_ul_ui + 1] <- aa - 1
    A.u[range_ul_uu, range_ul_ui + 3] <- -aa
  } else {
    aa <- (x[ui] - x[ui - 1]) / (x[ui + 1] - x[ui])
    A.u[range_ul_uu, range_ul_ui] <- 1
    A.u[range_ul_uu, range_ul_ui + 1] <- aa - 1
    A.u[range_ul_uu, range_ul_ui + 2] <- -aa
  }

  A.u[range_ul_uu + 1, range_ul_uu] <- 1
  A.u[range_ul_uu + 1, range_ul_uu + 1] <- -1
  A.u[range_ul_uu + 2, 1] <- 1
  A.u[range_ul_uu + 3, range_ul_uu + 1] <- 1
  A.u <- t(A.u)

  w.u <- quadprog::solve.QP(D.u, d.u, A.u, c(1, rep(0, range_ul_uu + 2)), range_ul_uu)
  x.u <- w.u$solution %*% x[u.l:u.u]

  return(x.u)
}
