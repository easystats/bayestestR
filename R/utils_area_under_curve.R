#' Area under the Curve (AUC)
#'
#' Based on the DescTools \code{AUC} function. It calculates the area under the curve with a naive algorithm and with a more elaborated spline approach. The curve must be given by vectors of xy-coordinates.
#'
#' @param x Vector of x values.
#' @param y Vector of y values.
#' @param method Can be "trapezoid" (default), "step" or "spline".
#'
#' @details If method is set to "trapezoid" then the curve is formed by connecting all points by a direct line (composite trapezoid rule). If "step" is chosen then a stepwise connection of two points is used. For calculating the area under a spline interpolation the splinefun function is used in combination with integrate. The AUC function will handle unsorted x values (by sorting x) and ties for the x values (by ignoring duplicates).
#'
#' @examples
#' library(bayestestR)
#' posterior <- rnorm_perfect(1000)
#'
#' dens <- as.data.frame(density(posterior))
#' dens <- dens[dens$x > 0, ]
#' x <- dens$x
#' y <- dens$y
#'
#' area_under_curve(x, y, method="trapezoid")
#' area_under_curve(x, y, method="step")
#' area_under_curve(x, y, method="spline")
#'
#' @importFrom stats integrate splinefun
#' @seealso DescTools
#' @export
area_under_curve <- function(x, y, method=c("trapezoid", "step", "spline")) {

  # Stolen from DescTools: https://github.com/cran/DescTools/blob/master/R/StatsAndCIs.r

  if (length(x) != length(y))
    stop("length x must equal length y")

  idx <- order(x)
  x <- x[idx]
  y <- y[idx]

  switch( match.arg( arg=method, choices=c("trapezoid","step","spline") )
          , "trapezoid" = { a <- sum((apply( cbind(y[-length(y)], y[-1]), 1, mean))*(x[-1] - x[-length(x)])) }
          , "step" = { a <- sum( y[-length(y)] * (x[-1] - x[-length(x)])) }
          , "spline" = { a <- integrate(splinefun(x, y, method="natural"), lower=min(x), upper=max(x))$value }
  )
  return(a)
}

#' @rdname area_under_curve
#' @export
auc <- area_under_curve