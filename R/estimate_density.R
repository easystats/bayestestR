#' Density Estimation
#'
#' This function is a wrapper over different methods of density estimation. By default, it uses the base R \link{density} with by default uses a different smoothing bandwidth (\code{"SJ"}) from the legacy default implemented the base R \link{density} function (\code{"nrd0"}). However, Deng \& Wickham suggest that \code{method = "KernSmooth"} is the fastest and the most accurate.
#'
#' @inheritParams hdi
#' @inheritParams stats::density
#' @param method Method of density estimation. Can be \code{"kernel"} (default), \code{"logspline"} or \code{"KernSmooth"}.
#' @param precision Number of points of density data. See the \code{n} parameter in \link[=density]{density}.
#' @param extend Extend the range of the x axis by a factor of \code{extend_scale}.
#' @param extend_scale Ratio of range by which to extend the x axis. A value of \code{0.1} means that the x axis will be extended by \code{1/10} of the range of the data.
#'
#' @examples
#' library(bayestestR)
#'
#' x <- rnorm(250, 1)
#'
#' # Methods
#' density_kernel <- estimate_density(x, method = "kernel")
#' density_logspline <- estimate_density(x, method = "logspline")
#' density_KernSmooth <- estimate_density(x, method = "KernSmooth")
#'
#' hist(x, prob = TRUE)
#' lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2)
#' lines(density_logspline$x, density_logspline$y, col = "red", lwd = 2)
#' lines(density_KernSmooth$x, density_KernSmooth$y, col = "blue", lwd = 2)
#'
#' # Extension
#' density_extended <- estimate_density(x, extend = TRUE)
#' density_default <- estimate_density(x, extend = FALSE)
#'
#' hist(x, prob = TRUE)
#' lines(density_extended$x, density_extended$y, col = "red", lwd = 3)
#' lines(density_default$x, density_default$y, col = "black", lwd = 3)
#'
#' df <- data.frame(replicate(4, rnorm(100)))
#' head(estimate_density(df))
#'
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#' head(estimate_density(model))
#'
#' library(emmeans)
#' head(estimate_density(emtrends(model, ~1, "wt")))
#' \dontrun{
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' estimate_density(model)
#' }
#'
#' @references Deng, H., & Wickham, H. (2011). Density estimation in R. Electronic publication.
#'
#' @importFrom stats density
#' @importFrom utils install.packages
#' @export
estimate_density <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", ...) {
  UseMethod("estimate_density")
}



#' @keywords internal
.estimate_density <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", ...) {
  method <- match.arg(method, c("kernel", "logspline", "KernSmooth", "smooth"))

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
    return(as.data.frame(density(x, n = precision, bw = bw, from = x_range[1], to = x_range[2], ...)))

    # Logspline
  } else if (method == "logspline") {
    if (!requireNamespace("logspline")) {
      if (interactive()) {
        readline("Package \"logspline\" needed for this function. Press ENTER to install or ESCAPE to abort.")
        install.packages("logspline")
      } else {
        stop("Package \"logspline\" needed for this function. Press run 'install.packages(\"logspline\")'.")
      }
    }

    x_axis <- seq(x_range[1], x_range[2], length.out = precision)
    y <- logspline::dlogspline(x_axis, logspline::logspline(x, ...), ...)
    return(data.frame(x = x_axis, y = y))

    # KernSmooth
  } else if (method %in% c("KernSmooth", "smooth")) {
    if (!requireNamespace("KernSmooth")) {
      if (interactive()) {
        readline("Package \"KernSmooth\" needed for this function. Press ENTER to install or ESCAPE to abort.")
        install.packages("KernSmooth")
      } else {
        stop("Package \"KernSmooth\" needed for this function. Press run 'install.packages(\"KernSmooth\")'.")
      }
    }
    return(as.data.frame(KernSmooth::bkde(x, range.x = x_range, gridsize = precision, truncate = TRUE, ...)))
  } else {
    stop("method should be one of 'kernel', 'logspline' or 'KernSmooth'")
  }
}





#' @export
estimate_density.numeric <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", ...) {
  out <- .estimate_density(x, method = method, precision = precision, extend = extend, extend_scale = extend_scale, bw = bw, ...)
  class(out) <- c("estimate_density", "see_estimate_density", class(out))
  out
}






#' @export
estimate_density.data.frame <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", ...) {
  x <- .select_nums(x)
  out <- sapply(x, estimate_density, method = method, precision = precision, extend = extend, extend_scale = extend_scale, bw = bw, simplify = FALSE)
  for (i in names(out)) {
    out[[i]]$Parameter <- i
  }
  out <- do.call(rbind, out)

  row.names(out) <- NULL
  out[, c("Parameter", "x", "y")]
}

#' @export
estimate_density.emmGrid <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }
  x <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(x, names = FALSE)))

  estimate_density(x,
    method = method, precision = precision,
    extend = extend, extend_scale = extend_scale,
    bw = bw, ...
  )
}




#' @importFrom insight get_parameters
#' @export
estimate_density.stanreg <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)

  out <- estimate_density(insight::get_parameters(x, effects = effects, parameters = parameters), method = method, precision = precision, extend = extend, extend_scale = extend_scale, bw = bw, ...)

  out
}



#' @importFrom insight get_parameters
#' @export
estimate_density.brmsfit <- function(x, method = "kernel", precision = 2^10, extend = FALSE, extend_scale = 0.1, bw = "SJ", effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- estimate_density(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), method = method, precision = precision, extend = extend, extend_scale = extend_scale, bw = bw, ...)

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






#' Density Probability at a Given Value
#'
#' Compute the density value at a given point of a distribution (i.e., the value of the \code{y} axis of a value \code{x} of a distribution).
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
#' @importFrom stats approx density
#' @export
density_at <- function(posterior, x, precision = 2^10, ...) {
  density <- estimate_density(posterior, precision = precision, ...)
  stats::approx(density$x, density$y, xout = x)$y
}

