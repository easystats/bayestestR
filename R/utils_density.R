#' Density Estimation
#'
#'
#' @inheritParams stats::density
#' @param method Method of density estimation.
#' @param precision Number of points of density data. See the \code{n} parameter in \link[=density]{density}.
#'
#' @examples
#' x <- rnorm(250, 1)
#'
#' density_kernel <- estimate_density(x, method="kernel")
#' density_logspline <- estimate_density(x, method="logspline")
#' density_KernSmooth <- estimate_density(x, method="KernSmooth")
#'
#' hist(x, prob=TRUE)
#' lines(density_kernel$x, density_kernel$y, col="black", lwd=2)
#' lines(density_logspline$x, density_logspline$y, col="red", lwd=2)
#' lines(density_KernSmooth$x, density_KernSmooth$y, col="blue", lwd=2)
#'
#' @importFrom stats density
#' @importFrom utils install.packages
#' @export
estimate_density <- function(x, method="kernel", precision = 2^10, bw = "SJ", ...){

  method <- match.arg(method, c("kernel", "logspline", "KernSmooth"))

  x_range <- range(x)

  # Kernel
  if(method == "kernel"){
    return(as.data.frame(density(x, n = precision, bw = bw, from = x_range[1], to = x_range[2], ...)))

  # Logspline
  } else if(method == "logspline"){
    if (!requireNamespace("logspline")) {
      if(interactive()){
        readline("Package \"logspline\" needed for this function. Press ENTER to install or ESCAPE to abort.")
        install.packages("logspline")
      } else{
        stop("Package \"logspline\" needed for this function. Press run 'install.packages(\"logspline\")'.")
      }
    }

    x_axis <- seq(x_range[1], x_range[2], length.out = precision)
    y <- logspline::dlogspline(x_axis, logspline::logspline(x, ...), ...)
    return(data.frame(x=x_axis, y=y))

    # KernSmooth
  } else if(method == "KernSmooth"){
    if (!requireNamespace("KernSmooth")) {
      if(interactive()){
        readline("Package \"KernSmooth\" needed for this function. Press ENTER to install or ESCAPE to abort.")
        install.packages("KernSmooth")
      } else{
        stop("Package \"KernSmooth\" needed for this function. Press run 'install.packages(\"KernSmooth\")'.")
      }
    }
    x <- as.data.frame(KernSmooth::bkde(x, range.x = x_range, gridsize = precision, truncate = TRUE, ...))
  } else{
    stop("method should be one of 'kernel', 'logspline' or 'KernSmooth'")
  }
}




#' Coerce to a Data Frame
#'
#' Functions to check if an object is a data frame, or coerce it if possible.
#'
#' @param x Any R object.
#' @param ... Additional arguments to be passed to or from methods.
#'
#'
#' @method as.data.frame density
#' @export
as.data.frame.density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}






#' Probability of a Given Point
#'
#' Compute the density of a given point of a distribution.
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








