#' Density Estimation
#'
#' This function is a wrapper over different methods of density estimation. By
#' default, it uses the base R `density` with by default uses a different smoothing
#' bandwidth (`"SJ"`) from the legacy default implemented the base R `density`
#' function (`"nrd0"`). However, Deng and Wickham suggest that `method = "KernSmooth"`
#' is the fastest and the most accurate.
#'
#' @inheritParams hdi
#' @inheritParams stats::density
#' @param bw See the eponymous argument in `density`. Here, the default has been
#' changed for `"SJ"`, which is recommended.
#' @param ci The confidence interval threshold. Only used when `method = "kernel"`.
#' This feature is experimental, use with caution.
#' @param method Density estimation method. Can be `"kernel"` (default), `"logspline"`
#' or `"KernSmooth"`.
#' @param precision Number of points of density data. See the `n` parameter in `density`.
#' @param extend Extend the range of the x axis by a factor of `extend_scale`.
#' @param extend_scale Ratio of range by which to extend the x axis. A value of `0.1`
#' means that the x axis will be extended by `1/10` of the range of the data.
#' @param select Character vector of column names. If `NULL` (the default), all
#' numeric variables will be selected. Other arguments from
#' `datawizard::extract_column_names()` (such as `exclude`) can also be used.
#' @param by Optional character vector. If not `NULL` and input is a data frame,
#' density estimation is performed for each group (subsets) indicated by `by`.
#' See examples.
#' @param at Deprecated in favour of `by`.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/bayestestR.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examplesIf require("logspline") && require("KernSmooth") && require("mclust") && require("emmeans") && require("rstanarm") && require("brms")
#' library(bayestestR)
#'
#' set.seed(1)
#' x <- rnorm(250, mean = 1)
#'
#' # Basic usage
#' density_kernel <- estimate_density(x) # default method is "kernel"
#'
#' hist(x, prob = TRUE)
#' lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2)
#' lines(density_kernel$x, density_kernel$CI_low, col = "gray", lty = 2)
#' lines(density_kernel$x, density_kernel$CI_high, col = "gray", lty = 2)
#' legend("topright",
#'   legend = c("Estimate", "95% CI"),
#'   col = c("black", "gray"), lwd = 2, lty = c(1, 2)
#' )
#'
#' # Other Methods
#' density_logspline <- estimate_density(x, method = "logspline")
#' density_KernSmooth <- estimate_density(x, method = "KernSmooth")
#' density_mixture <- estimate_density(x, method = "mixture")
#'
#' hist(x, prob = TRUE)
#' lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2)
#' lines(density_logspline$x, density_logspline$y, col = "red", lwd = 2)
#' lines(density_KernSmooth$x, density_KernSmooth$y, col = "blue", lwd = 2)
#' lines(density_mixture$x, density_mixture$y, col = "green", lwd = 2)
#'
#' # Extension
#' density_extended <- estimate_density(x, extend = TRUE)
#' density_default <- estimate_density(x, extend = FALSE)
#'
#' hist(x, prob = TRUE)
#' lines(density_extended$x, density_extended$y, col = "red", lwd = 3)
#' lines(density_default$x, density_default$y, col = "black", lwd = 3)
#'
#' # Multiple columns
#' head(estimate_density(iris))
#' head(estimate_density(iris, select = "Sepal.Width"))
#'
#' # Grouped data
#' head(estimate_density(iris, by = "Species"))
#' head(estimate_density(iris$Petal.Width, by = iris$Species))
#' \donttest{
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- suppressWarnings(
#'   stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' )
#' head(estimate_density(model))
#'
#' library(emmeans)
#' head(estimate_density(emtrends(model, ~1, "wt", data = mtcars)))
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' estimate_density(model)
#' }
#'
#' @references Deng, H., & Wickham, H. (2011). Density estimation in R. Electronic publication.
#'
#' @export
estimate_density <- function(x, ...) {
  UseMethod("estimate_density")
}


#' @export
estimate_density.default <- function(x, ...) {
  insight::format_error(
    paste0("`estimate_density()` is not yet implemented for objects of class `", class(x)[1], "`.")
  )
}


#' @keywords internal
.estimate_density <- function(x,
                              method = "kernel",
                              precision = 2^10,
                              extend = FALSE,
                              extend_scale = 0.1,
                              bw = "SJ",
                              ci = NULL,
                              ...) {
  method <- match.arg(
    tolower(method),
    c("kernel", "logspline", "kernsmooth", "smooth", "mixture", "mclust")
  )

  # Remove NA
  x <- x[!is.na(x)]

  if (length(x) < 2) {
    return(stats::setNames(
      data.frame(matrix(ncol = 3, nrow = 0)),
      c("Parameter", "x", "y")
    ))
  }

  # Range
  x_range <- range(x)
  if (extend) {
    extension_scale <- diff(x_range) * extend_scale
    x_range[1] <- x_range[1] - extension_scale
    x_range[2] <- x_range[2] + extension_scale
  }

  # Replace inf values if needed
  x_range[is.infinite(x_range)] <- 5.565423e+156

  # Kernel
  if (method == "kernel") {
    kde <- .estimate_density_kernel(x, x_range, precision, bw, ci, ...)
    # Logspline
  } else if (method == "logspline") {
    kde <- .estimate_density_logspline(x, x_range, precision, ...)
    # KernSmooth
  } else if (method %in% c("kernsmooth", "smooth")) {
    kde <- .estimate_density_KernSmooth(x, x_range, precision, ...)
    # Mixture
  } else if (method %in% c("mixture", "mclust")) {
    kde <- .estimate_density_mixture(x, x_range, precision, ...)
  } else {
    insight::format_error("method should be one of 'kernel', 'logspline', 'KernSmooth' or 'mixture'.")
  }
  kde
}


# Methods -----------------------------------------------------------------

#' @export
estimate_density.numeric <- function(x,
                                     method = "kernel",
                                     precision = 2^10,
                                     extend = FALSE,
                                     extend_scale = 0.1,
                                     bw = "SJ",
                                     ci = NULL,
                                     by = NULL,
                                     at = NULL,
                                     ...) {
  # TODO remove deprecation warning
  # Sanity
  if (!is.null(at)) {
    insight::format_warning(
      "The `at` argument is deprecated and might be removed in a future update. Please replace by `by`."
    )
    by <- at
  }

  if (!is.null(by)) {
    if (length(by) == 1) {
      insight::format_error(paste0(
        "`by` must be either the name of a group column if a data frame is entered as input,",
        " or in this case (where a single vector was passed) a vector of same length."
      ))
    }
    out <- estimate_density(
      data.frame(V1 = x, Group = by, stringsAsFactors = FALSE),
      method = method,
      precision = precision,
      extend = extend,
      extend_scale = extend_scale,
      bw = bw,
      ci = ci,
      by = "Group",
      ...
    )
    out$Parameter <- NULL
    return(out)
  }
  out <- .estimate_density(
    x,
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ci = ci,
    ...
  )
  class(out) <- .set_density_class(out)
  out
}


#' @rdname estimate_density
#' @inheritParams p_direction
#' @export
estimate_density.data.frame <- function(x,
                                        method = "kernel",
                                        precision = 2^10,
                                        extend = FALSE,
                                        extend_scale = 0.1,
                                        bw = "SJ",
                                        ci = NULL,
                                        select = NULL,
                                        by = NULL,
                                        at = NULL,
                                        rvar_col = NULL,
                                        ...) {
  x_rvar <- .possibly_extract_rvar_col(x, rvar_col)
  if (length(x_rvar) > 0L) {
    cl <- match.call()
    cl[[1]] <- bayestestR::estimate_density
    cl$x <- x_rvar
    cl$rvar_col <- NULL
    out <- eval.parent(cl)

    obj_name <- insight::safe_deparse_symbol(substitute(x))
    attr(out, "object_name") <- sprintf('%s[["%s"]]', obj_name, rvar_col)

    out <- .append_datagrid(out, x, long = TRUE)
    class(out) <- .set_density_class(out)
    return(out)
  }


  # Sanity
  if (!is.null(at)) {
    insight::format_warning(paste0(
      "The `at` argument is deprecated and might be removed in a future update.",
      " Please replace by `by`."
    ))
    by <- at
  }

  if (is.null(by)) {
    # No grouping -------------------
    out <- .estimate_density_df(
      x = x,
      method = method,
      precision = precision,
      extend = extend,
      extend_scale = extend_scale,
      bw = bw,
      ci = ci,
      select = select,
      ...
    )
  } else {
    # Deal with by- grouping --------

    groups <- insight::get_datagrid(x[, by, drop = FALSE], by = by) # Get combinations
    out <- data.frame()
    for (row in seq_len(nrow(groups))) {
      subdata <- datawizard::data_match(x, groups[row, , drop = FALSE])
      subdata[names(groups)] <- NULL
      subdata <- .estimate_density_df(
        subdata,
        method = method,
        precision = precision,
        extend = extend,
        extend_scale = extend_scale,
        bw = bw,
        ci = ci,
        select = select,
        ...
      )
      out <- rbind(out, merge(subdata, groups[row, , drop = FALSE]))
    }
  }

  class(out) <- .set_density_df_class(out)
  out
}


#' @export
estimate_density.draws <- function(x,
                                   method = "kernel",
                                   precision = 2^10,
                                   extend = FALSE,
                                   extend_scale = 0.1,
                                   bw = "SJ",
                                   ci = NULL,
                                   select = NULL,
                                   by = NULL,
                                   at = NULL,
                                   ...) {
  if (!is.null(at)) {
    insight::format_warning(paste0(
      "The `at` argument is deprecated and might be removed in a future update.",
      " Please replace by `by`."
    ))
    by <- at
  }

  estimate_density(
    .posterior_draws_to_df(x),
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    select = select,
    by = by
  )
}

#' @export
estimate_density.rvar <- estimate_density.draws


#' @export
estimate_density.grouped_df <- function(x,
                                        method = "kernel",
                                        precision = 2^10,
                                        extend = FALSE,
                                        extend_scale = 0.1,
                                        bw = "SJ",
                                        ci = NULL,
                                        select = NULL,
                                        ...) {
  groups <- .group_vars(x)
  ungrouped_x <- as.data.frame(x)

  xlist <- split(ungrouped_x, ungrouped_x[groups])

  out <- lapply(names(xlist), function(group) {
    dens <- estimate_density(
      xlist[[group]],
      method = method,
      precision = precision,
      extend = extend,
      extend_scale = extend_scale,
      bw = bw,
      ci = ci,
      select = select,
      ...
    )
    dens$Group <- group
    dens
  })
  do.call(rbind, out)
}

# to avoid class conflicts - e.g., numeric variables imported with the
# haven package are of class "haven_labelled" and "double", which causes
# problems with the generic or numeric method.

#' @export
estimate_density.double <- estimate_density.numeric

#' @export
estimate_density.emmGrid <- function(x,
                                     method = "kernel",
                                     precision = 2^10,
                                     extend = FALSE,
                                     extend_scale = 0.1,
                                     bw = "SJ",
                                     ...) {
  xdf <- insight::get_parameters(x)

  out <- estimate_density(xdf,
    method = method, precision = precision,
    extend = extend, extend_scale = extend_scale,
    bw = bw, ...
  )

  out <- .append_datagrid(out, x, long = TRUE)
  class(out) <- .set_density_class(out)
  out
}

#' @export
estimate_density.emm_list <- estimate_density.emmGrid

#' @export
estimate_density.slopes <- function(x,
                                    method = "kernel",
                                    precision = 2^10,
                                    extend = FALSE,
                                    extend_scale = 0.1,
                                    bw = "SJ",
                                    ...) {
  xdf <- .get_marginaleffects_draws(x)

  out <- estimate_density(xdf,
    method = method, precision = precision,
    extend = extend, extend_scale = extend_scale,
    bw = bw, ...
  )

  out <- .append_datagrid(out, x, long = TRUE)
  class(out) <- .set_density_class(out)
  out
}

#' @export
estimate_density.predictions <- estimate_density.slopes

#' @export
estimate_density.comparisons <- estimate_density.slopes


#' @export
estimate_density.stanreg <- function(x,
                                     method = "kernel",
                                     precision = 2^10,
                                     extend = FALSE,
                                     extend_scale = 0.1,
                                     bw = "SJ",
                                     effects = c("fixed", "random", "all"),
                                     component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"),
                                     parameters = NULL,
                                     ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- estimate_density(
    insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- .set_density_class(out)
  out
}

#' @export
estimate_density.stanfit <- estimate_density.stanreg

#' @export
estimate_density.blavaan <- estimate_density.stanreg


#' @export
estimate_density.brmsfit <- function(x,
                                     method = "kernel",
                                     precision = 2^10,
                                     extend = FALSE,
                                     extend_scale = 0.1,
                                     bw = "SJ",
                                     effects = c("fixed", "random", "all"),
                                     component = c("conditional", "zi", "zero_inflated", "all"),
                                     parameters = NULL,
                                     ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- estimate_density(
    insight::get_parameters(x, effects = effects, component = component, parameters = parameters),
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- .set_density_class(out)
  out
}


#' @export
estimate_density.MCMCglmm <- function(x,
                                      method = "kernel",
                                      precision = 2^10,
                                      extend = FALSE,
                                      extend_scale = 0.1,
                                      bw = "SJ",
                                      parameters = NULL,
                                      ...) {
  nF <- x$Fixed$nfl
  out <- estimate_density(
    as.data.frame(x$Sol[, 1:nF, drop = FALSE]),
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- .set_density_class(out)
  out
}


#' @export
estimate_density.mcmc <- function(x,
                                  method = "kernel",
                                  precision = 2^10,
                                  extend = FALSE,
                                  extend_scale = 0.1,
                                  bw = "SJ",
                                  parameters = NULL,
                                  ...) {
  out <- estimate_density(
    insight::get_parameters(x, parameters = parameters),
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(out) <- .set_density_class(out)
  out
}

#' @export
estimate_density.bayesQR <- estimate_density.mcmc

#' @export
estimate_density.blrm <- estimate_density.mcmc

#' @export
estimate_density.bcplm <- estimate_density.mcmc

#' @export
estimate_density.BGGM <- estimate_density.mcmc

#' @export
estimate_density.mcmc.list <- estimate_density.mcmc


#' @export
estimate_density.bamlss <- function(x,
                                    method = "kernel",
                                    precision = 2^10,
                                    extend = FALSE,
                                    extend_scale = 0.1,
                                    bw = "SJ",
                                    component = c("all", "conditional", "location"),
                                    parameters = NULL,
                                    ...) {
  component <- match.arg(component)
  out <- estimate_density(
    insight::get_parameters(x, component = component, parameters = parameters),
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))

  class(out) <- .set_density_class(out)
  out
}


#' Coerce to a Data Frame
#'
#' @inheritParams base::as.data.frame
#' @method as.data.frame density
#' @export
as.data.frame.density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}


# helper ------------------

.estimate_density_df <- function(x,
                                 method = "kernel",
                                 precision = 2^10,
                                 extend = FALSE,
                                 extend_scale = 0.1,
                                 bw = "SJ",
                                 ci = NULL,
                                 select = NULL,
                                 ...) {
  # TODO: replace by exposed select argument
  if (is.null(select)) {
    x <- .select_nums(x)
  } else {
    x <- datawizard::data_select(x, select, ...)
  }

  out <- sapply(
    x,
    estimate_density,
    method = method,
    precision = precision,
    extend = extend,
    extend_scale = extend_scale,
    bw = bw,
    ci = ci,
    simplify = FALSE
  )

  for (i in names(out)) {
    if (nrow(out[[i]]) == 0) {
      insight::format_warning(paste0("`", i, "`, or one of its groups specified in `by`, is empty and has no density information."))
    } else {
      out[[i]]$Parameter <- i
    }
  }
  out <- do.call(rbind, out)

  row.names(out) <- NULL
  out[, c("Parameter", "x", "y")]
}


#' Density Probability at a Given Value
#'
#' Compute the density value at a given point of a distribution (i.e.,
#' the value of the `y` axis of a value `x` of a distribution).
#'
#' @param posterior Vector representing a posterior distribution.
#' @param x The value of which to get the approximate probability.
#' @inheritParams estimate_density
#'
#' @examples
#' library(bayestestR)
#' posterior <- distribution_normal(n = 10)
#' density_at(posterior, 0)
#' density_at(posterior, c(0, 1))
#' @export
density_at <- function(posterior, x, precision = 2^10, method = "kernel", ...) {
  posterior_density <- estimate_density(posterior, precision = precision, method = method, ...)
  stats::approx(posterior_density$x, posterior_density$y, xout = x)$y
}


# Different functions -----------------------------------------------------

.estimate_density_kernel <- function(x, x_range, precision, bw, ci = 0.95, ...) {
  # unsupported arguments raise warnings
  dots <- list(...)
  dots[c("effects", "component", "parameters")] <- NULL

  # Get the kernel density estimation (KDE)
  my_args <- c(dots, list(
    x = x,
    n = precision,
    bw = bw,
    from = x_range[1],
    to = x_range[2]
  ))
  fun <- get("density", asNamespace("stats"))
  kde <- suppressWarnings(do.call("fun", my_args))
  my_df <- as.data.frame(kde)

  # Get CI (https://bookdown.org/egarpor/NP-UC3M/app-kde-ci.html)
  if (!is.null(ci)) {
    h <- kde$bw # Selected bandwidth
    # R(K) for a normal
    Rk <- 1 / (2 * sqrt(pi))
    # Estimate the SD
    sd_kde <- sqrt(my_df$y * Rk / (length(x) * h))
    # CI with estimated variance
    z_alpha <- stats::qnorm(ci)
    my_df$CI_low <- my_df$y - z_alpha * sd_kde
    my_df$CI_high <- my_df$y + z_alpha * sd_kde
  }
  my_df
}


.estimate_density_logspline <- function(x, x_range, precision, ...) {
  insight::check_if_installed("logspline")
  x_axis <- seq(x_range[1], x_range[2], length.out = precision)
  y <- logspline::dlogspline(x_axis, logspline::logspline(x, ...), ...)
  data.frame(x = x_axis, y = y)
}


.estimate_density_KernSmooth <- function(x, x_range, precision, ...) {
  insight::check_if_installed("KernSmooth")
  as.data.frame(KernSmooth::bkde(x, range.x = x_range, gridsize = precision, truncate = TRUE, ...))
}


.estimate_density_mixture <- function(x, x_range, precision, ...) {
  insight::check_if_installed("mclust")
  x_axis <- seq(x_range[1], x_range[2], length.out = precision)
  y <- stats::predict(mclust::densityMclust(x, verbose = FALSE, ...), newdata = x_axis, ...)
  data.frame(x = x_axis, y = y)
}


.set_density_df_class <- function(out) {
  setdiff(
    unique(c("estimate_density_df", "see_estimate_density_df", class(out))),
    c("estimate_density", "see_estimate_density")
  )
}

.set_density_class <- function(out) {
  if (is.null(out)) {
    return(NULL)
  }
  setdiff(
    unique(c("estimate_density", "see_estimate_density", class(out))),
    c("estimate_density_df", "see_estimate_density_df")
  )
}
