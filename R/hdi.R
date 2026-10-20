#' Highest Density Interval (HDI)
#'
#' Compute the **Highest Density Interval (HDI)** of posterior distributions.
#' All points within this interval have a higher probability density than points
#' outside the interval. The HDI can be used in the context of uncertainty
#' characterisation of posterior distributions as **Credible Interval (CI)**.
#'
#' @param x Vector representing a posterior distribution, or a data frame of such
#'   vectors. Can also be a Bayesian model. **bayestestR** supports a wide range
#'   of models (see, for example, `methods("hdi")`) and not all of those are
#'   documented in the 'Usage' section, because methods for other classes mostly
#'   resemble the arguments of the `.numeric` or `.data.frame`methods.
#' @param ci Value or vector of probability of the (credible) interval - CI
#'   (between 0 and 1) to be estimated. Default to `.95` (95%).
#' @param component Which type of parameters to return, such as parameters for
#' the conditional model, the zero-inflated part of the model, the dispersion
#' term, etc. See details in section _Model Components_. May be abbreviated.
#' Note that the *conditional* component also refers to the *count* or *mean*
#' component - names may differ, depending on the modeling package. There are
#' three convenient shortcuts (not applicable to *all* model classes):
#' - `component = "all"` returns all possible parameters.
#' - If `component = "location"`, location parameters such as `conditional`,
#'   `zero_inflated`, `smooth_terms`, or `instruments` are returned (everything
#'   that are fixed or random effects - depending on the `effects` argument -
#'   but no auxiliary parameters).
#' - For `component = "distributional"` (or `"auxiliary"`), components like
#'   `sigma`, `dispersion`, `beta` or `precision` (and other auxiliary
#'   parameters) are returned.
#' @param parameters Regular expression pattern that describes the parameters
#'   that should be returned. Meta-parameters (like `lp__` or `prior_`) are
#'   filtered by default, so only parameters that typically appear in the
#'   `summary()` are returned. Use `parameters` to select specific parameters
#'   for the output.
#' @param use_iterations Logical, if `TRUE` and `x` is a `get_predicted` object,
#' (returned by [`insight::get_predicted()`]), the function is applied to the
#' iterations instead of the predictions. This only applies to models that return
#' iterations for predicted values (e.g., `brmsfit` models).
#' @param verbose Toggle off warnings.
#' @param method The algorithm that finds the interval. Can be `"samples"`
#'   (default) or `"density"`. See section _Algorithms_.
#' @param ... Currently not used.
#' @inheritParams insight::get_parameters.BFBayesFactor
#' @inheritParams insight::get_parameters
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @section Model components:
#'
#' Possible values for the `component` argument depend on the model class.
#' Following are valid options:
#' - `"all"`: returns all model components, applies to all models, but will only
#'   have an effect for models with more than just the conditional model
#'   component.
#' - `"conditional"`: only returns the conditional component, i.e. "fixed
#'   effects" terms from the model. Will only have an effect for models with
#'   more than just the conditional model component.
#' - `"smooth_terms"`: returns smooth terms, only applies to GAMs (or similar
#'   models that may contain smooth terms).
#' - `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.
#' - `"location"`: returns location parameters such as `conditional`,
#'   `zero_inflated`, or `smooth_terms` (everything that are fixed or random
#'   effects - depending on the `effects` argument - but no auxiliary
#'   parameters).
#' - `"distributional"` (or `"auxiliary"`): components like `sigma`,
#'   `dispersion`, `beta` or `precision` (and other auxiliary parameters) are
#'   returned.
#'
#' For models of class `brmsfit` (package **brms**), even more options are
#' possible for the `component` argument, which are not all documented in detail
#' here. See also [`?insight::find_parameters`](https://easystats.github.io/insight/reference/find_parameters.BGGM.html).
#'
#' @section Algorithms:
#'
#' Both methods return one continuous interval for each value of `ci`. For
#' multimodal distributions, this interval can include values of low density
#' between the modes.
#'
#' - `method = "samples"` (default) sorts the `n` draws and takes the
#'   narrowest window of `ceiling(ci * n) + 1` neighboring sorted draws. The
#'   bounds are the first and the last draw of that window.
#' - `method = "density"` uses the SPIn algorithm of Liu, Gelman and Zheng
#'   (2015), adapted from the **SPIn** package. It starts from the narrowest
#'   window of the sorted draws. Then it replaces each bound with a weighted
#'   average of the sorted draws near it. The weights come from quadratic
#'   programming, with an objective that uses a kernel density estimate
#'   ([stats::density()]). Liu et al. (2015) report that these bounds have a
#'   lower Monte Carlo error than the bounds of the samples method. This
#'   method needs the **quadprog** package. The deprecated `spi()` returns
#'   the same interval.
#'
#' @details Unlike equal-tailed intervals (see [`eti()`]) that typically exclude
#' 2.5% from each tail of the distribution and always include the median, the
#' HDI is *not* equal-tailed and therefore always includes the mode(s) of posterior
#' distributions. While this can be useful to better represent the credibility
#' mass of a distribution, the HDI also has some limitations. The section
#' _Algorithms_ of [`hdi()`] explains how the HDI is computed.
#'
#' A 95% equal-tailed interval (ETI) has 2.5% of the distribution on either
#' side of its limits. It indicates the 2.5th percentile and the 97.5th
#' percentile. In symmetric distributions, the two methods of computing credible
#' intervals, the ETI and the [HDI][hdi], return similar results.
#'
#' This is not the case for skewed distributions. Indeed, it is possible that
#' parameter values in the ETI have lower credibility (are less probable) than
#' parameter values outside the ETI. This property seems undesirable as a summary
#' of the credible values in a distribution.
#'
#' On the other hand, the ETI range does change when transformations are applied
#' to the distribution (for instance, for a log odds scale to probabilities):
#' the lower and higher bounds of the transformed distribution will correspond
#' to the transformed lower and higher bounds of the original distribution.
#' On the contrary, applying transformations to the distribution will change
#' the resulting HDI.
#'
#' The [95% or 89% Credible Intervals (CI)](https://easystats.github.io/bayestestR/articles/credible_interval.html)
#' are two reasonable ranges to characterize the uncertainty related to the
#' estimation (see [here](https://easystats.github.io/bayestestR/articles/credible_interval.html)
#' for a discussion about the differences between these two values).
#'
#' @inherit ci return
#'
#' @family ci
#' @seealso Other interval functions, such as [`hdi()`], [`eti()`], [`bci()`],
#' [`si()`].
#'
#' @examplesIf all(insight::check_if_installed(c("rstanarm", "brms", "emmeans", "BayesFactor"), quietly = TRUE))
#' library(bayestestR)
#'
#' posterior <- rnorm(1000)
#' hdi(posterior, ci = 0.89)
#' hdi(posterior, ci = c(0.80, 0.90, 0.95))
#'
#' hdi(iris[1:4])
#' hdi(iris[1:4], ci = c(0.80, 0.90, 0.95))
#' \donttest{
#' model <- suppressWarnings(
#'   rstanarm::stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' hdi(model)
#' hdi(model, ci = c(0.80, 0.90, 0.95))
#'
#' hdi(emmeans::emtrends(model, ~1, "wt", data = mtcars))
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' hdi(model)
#' hdi(model, ci = c(0.80, 0.90, 0.95))
#'
#' bf <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' hdi(bf)
#' hdi(bf, ci = c(0.80, 0.90, 0.95))
#' }
#' @author Credits go to **ggdistribute** and [**HDInterval**](https://github.com/mikemeredith/HDInterval).
#' The code of `method = "density"` was adapted from the **SPIn** package, and
#' slightly modified to be more robust for Stan models. Credits go to Ying Liu
#' for the original SPI algorithm and R implementation.
#'
#' @references
#' - Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS,
#'   and Stan. Academic Press.
#' - Liu, Y., Gelman, A., & Zheng, T. (2015). Simulation-efficient shortest
#'   probability intervals. Statistics and Computing, 25(4), 809–819.
#'   https://doi.org/10.1007/s11222-015-9563-8
#' - McElreath, R. (2015). Statistical rethinking: A Bayesian course with
#'   examples in R and Stan. Chapman and Hall/CRC.
#'
#' @export
hdi <- function(x, ...) {
  UseMethod("hdi")
}


#' @export
hdi.default <- function(x, ...) {
  insight::format_error(paste0(
    "'hdi()' is not yet implemented for objects of class '",
    class(x)[1],
    "'."
  ))
}


#' @rdname hdi
#' @export
hdi.numeric <- function(x, ci = 0.95, verbose = TRUE, method = "samples", ...) {
  method <- .check_hdi_method(method, missing(method), list(...))
  hdi_fun <- if (method == "density") .spi else .hdi
  out <- do.call(
    rbind,
    lapply(ci, function(i) {
      hdi_fun(x, ci = i, verbose = verbose)
    })
  )
  class(out) <- unique(c(
    "bayestestR_hdi",
    "see_hdi",
    "bayestestR_ci",
    "see_ci",
    if (method == "density") "bayestestR_spi",
    class(out)
  ))
  attr(out, "data") <- x
  out
}


#' @rdname hdi
#' @inheritParams p_direction
#' @export
hdi.data.frame <- function(
  x,
  ci = 0.95,
  rvar_col = NULL,
  verbose = TRUE,
  method = "samples",
  ...
) {
  obj_name <- insight::safe_deparse_symbol(substitute(x))
  method <- .check_hdi_method(method, missing(method), list(...))

  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::hdi
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    cl$method <- method
    out <- eval.parent(cl)

    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    return(.append_datagrid(out, x, long = length(ci) > 1L))
  }

  dat <- .compute_interval_dataframe(
    x = x,
    ci = ci,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  attr(dat, "object_name") <- obj_name
  dat
}


#' @export
hdi.draws <- function(x, ci = 0.95, verbose = TRUE, method = "samples", ...) {
  method <- .check_hdi_method(method, missing(method), list(...))
  dat <- .compute_interval_dataframe(
    x = .posterior_draws_to_df(x),
    ci = ci,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  attr(dat, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
hdi.rvar <- hdi.draws


#' @export
hdi.MCMCglmm <- function(x, ci = 0.95, verbose = TRUE, method = "samples", ...) {
  method <- .check_hdi_method(method, missing(method), list(...))
  nF <- x$Fixed$nfl
  d <- as.data.frame(x$Sol[, 1:nF, drop = FALSE])
  dat <- .compute_interval_dataframe(
    x = d,
    ci = ci,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
hdi.bamlss <- function(
  x,
  ci = 0.95,
  component = "all",
  verbose = TRUE,
  method = "samples",
  ...
) {
  method <- .check_hdi_method(method, missing(method), list(...))
  d <- insight::get_parameters(x, component = component, verbose = verbose)
  dat <- .compute_interval_dataframe(
    x = d,
    ci = ci,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  dat <- .add_clean_parameters_attribute(dat, x)
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
hdi.mcmc <- function(x, ci = 0.95, verbose = TRUE, method = "samples", ...) {
  method <- .check_hdi_method(method, missing(method), list(...))
  d <- as.data.frame(x)
  dat <- .compute_interval_dataframe(
    x = d,
    ci = ci,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}


#' @export
hdi.bcplm <- function(x, ci = 0.95, verbose = TRUE, method = "samples", ...) {
  method <- .check_hdi_method(method, missing(method), list(...))
  d <- insight::get_parameters(x, verbose = verbose)
  dat <- .compute_interval_dataframe(
    x = d,
    ci = ci,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  attr(dat, "data") <- insight::safe_deparse_symbol(substitute(x))
  dat
}

#' @export
hdi.bayesQR <- hdi.bcplm

#' @export
hdi.blrm <- hdi.bcplm

#' @export
hdi.mcmc.list <- hdi.bcplm

#' @export
hdi.BGGM <- hdi.bcplm


#' @export
hdi.sim.merMod <- function(
  x,
  ci = 0.95,
  effects = "fixed",
  parameters = NULL,
  verbose = TRUE,
  method = "samples",
  ...
) {
  method <- .check_hdi_method(method, missing(method), list(...))

  dat <- .compute_interval_simMerMod(
    x = x,
    ci = ci,
    effects = effects,
    parameters = parameters,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}


#' @export
hdi.sim <- function(
  x,
  ci = 0.95,
  parameters = NULL,
  verbose = TRUE,
  method = "samples",
  ...
) {
  method <- .check_hdi_method(method, missing(method), list(...))
  dat <- .compute_interval_sim(
    x = x,
    ci = ci,
    parameters = parameters,
    verbose = verbose,
    fun = "hdi",
    method = method
  )
  out <- dat$result
  attr(out, "data") <- dat$data
  out
}


#' @export
hdi.emmGrid <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xdf <- insight::get_parameters(x, verbose = verbose)
  out <- hdi(xdf, ci = ci, verbose = verbose, ...)
  out <- .append_datagrid(out, x, long = length(ci) > 1L)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
hdi.emm_list <- hdi.emmGrid

#' @export
hdi.slopes <- function(x, ci = 0.95, verbose = TRUE, ...) {
  xrvar <- .get_marginaleffects_draws(x)
  out <- hdi(xrvar, ci = ci, verbose = verbose, ...)
  out <- .append_datagrid(out, x, long = length(ci) > 1L)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}

#' @export
hdi.comparisons <- hdi.slopes

#' @export
hdi.predictions <- hdi.slopes


#' @export
hdi.stanreg <- function(
  x,
  ci = 0.95,
  effects = "fixed",
  component = "location",
  parameters = NULL,
  verbose = TRUE,
  method = "samples",
  ...
) {
  method <- .check_hdi_method(method, missing(method), list(...))
  cleaned_parameters <- .get_cleaned_parameters(x, ...)

  out <- .prepare_output(
    hdi(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose
      ),
      ci = ci,
      verbose = verbose,
      method = method,
      ...
    ),
    cleaned_parameters,
    inherits(x, "stanmvreg")
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- unique(c(
    "bayestestR_hdi",
    "see_hdi",
    if (method == "density") "bayestestR_spi",
    class(out)
  ))
  out
}

#' @export
hdi.stanfit <- hdi.stanreg

#' @export
hdi.CmdStanFit <- hdi.stanreg

#' @export
hdi.blavaan <- hdi.stanreg


#' @rdname hdi
#' @export
hdi.brmsfit <- function(
  x,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  parameters = NULL,
  verbose = TRUE,
  method = "samples",
  ...
) {
  method <- .check_hdi_method(method, missing(method), list(...))
  cleaned_parameters <- .get_cleaned_parameters(x, ...)

  out <- .prepare_output(
    hdi(
      insight::get_parameters(
        x,
        effects = effects,
        component = component,
        parameters = parameters,
        verbose = verbose
      ),
      ci = ci,
      verbose = verbose,
      method = method,
      ...
    ),
    cleaned_parameters
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- unique(c(
    "bayestestR_hdi",
    "see_hdi",
    if (method == "density") "bayestestR_spi",
    class(out)
  ))
  out
}


#' @export
hdi.BFBayesFactor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  out <- hdi(insight::get_parameters(x), ci = ci, verbose = verbose, ...)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  out
}


#' @rdname hdi
#' @export
hdi.get_predicted <- function(x, ci = 0.95, use_iterations = FALSE, verbose = TRUE, ...) {
  if (isTRUE(use_iterations)) {
    if ("iterations" %in% names(attributes(x))) {
      out <- hdi(
        as.data.frame(t(attributes(x)$iterations)),
        ci = ci,
        verbose = verbose,
        ...
      )
    } else {
      insight::format_error("No iterations present in the output.")
    }
    attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  } else {
    out <- hdi(as.numeric(x), ci = ci, verbose = verbose, ...)
  }
  out
}


# Helper ------------------------------------------------------------------

#' @keywords internal
.hdi <- function(x, ci = 0.95, verbose = TRUE) {
  check_ci <- .check_ci_argument(x, ci, verbose)

  if (!is.null(check_ci)) {
    return(check_ci)
  }

  # removes NA/NaN, but not Inf
  x_sorted <- unname(sort.int(x, method = "quick"))
  # See https://github.com/easystats/bayestestR/issues/39
  window_size <- ceiling(ci * length(x_sorted))

  if (window_size < 2) {
    if (verbose) {
      insight::format_alert(
        "`ci` is too small or x does not contain enough data points, returning NAs."
      )
    }
    return(data.frame(
      CI = ci,
      CI_low = NA,
      CI_high = NA
    ))
  }

  nCIs <- length(x_sorted) - window_size

  if (nCIs < 1) {
    if (verbose) {
      insight::format_alert(
        "`ci` is too large or x does not contain enough data points, returning NAs."
      )
    }
    return(data.frame(
      CI = ci,
      CI_low = NA,
      CI_high = NA
    ))
  }

  ci.width <- sapply(1:nCIs, function(.x) x_sorted[.x + window_size] - x_sorted[.x])

  # find minimum of width differences, check for multiple minima
  min_i <- which(ci.width == min(ci.width))
  n_candies <- length(min_i)

  if (n_candies > 1) {
    if (any(diff(sort(min_i)) != 1)) {
      if (verbose) {
        insight::format_alert(
          "Identical densities found along different segments of the distribution, choosing rightmost."
        )
      }
      min_i <- max(min_i)
    } else {
      min_i <- floor(mean(min_i))
    }
  }

  data.frame(
    CI = ci,
    CI_low = x_sorted[min_i],
    CI_high = x_sorted[min_i + window_size]
  )
}


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
  # validation check for very low CI levels
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
      x.l <- .safe(.spi_lower(
        bw = frac * bw,
        n.sims = n.sims,
        k = k,
        l = l,
        dens = dens,
        x = x
      ))
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
      x.u <- .safe(.spi_upper(
        bw = frac * bw,
        n.sims = n.sims,
        ui = ui,
        u = u,
        dens = dens,
        x = x
      ))
      if (frac <= 0.1) {
        insight::format_alert("Could not find a solution for the SPI upper bound.")
        x.u <- NA
      }
    }
  }

  # output
  data.frame(CI = ci, CI_low = x.l, CI_high = x.u)
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

  x.l
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
