#' Curvewise Intervals (CWI)
#'
#' Compute the **Curvewise interval (CWI)** (also called the "simultaneous interval" or "joint interval") of posterior distributions using \code{ggdist::curve_interval()}.
#' Whereas the more typical "pointwise intervals" contain xx% of the posterior for a single parameter,
#' joint/curvewise intervals contain xx% of the posterior distribution for **all** parameters.
#'
#' Applied model predictions, pointwise intervals contain xx% of the predicted response values **conditional** on specific predictor values.
#' In contrast, curvewise intervals contain xx% of the predicted response values across all predictor values.
#' Put another way, curvewise intervals contain xx% of the full **prediction lines** from the model.
#'
#' For more details, see the [*ggdist* documentation on curvewise intervals](https://mjskay.github.io/ggdist/articles/lineribbon.html#curve-boxplots-aka-lineribbons-with-joint-intervals-or-curvewise-intervals-).
#'
#' @inheritParams hdi
#' @inherit ci return
#' @inherit hdi details
#' @inherit hdi seealso
#' @family ci
#'
#' @examples
#' \donttest{
#' library(bayestestR)
#'
#' if (require("ggplot2") && require("rstanarm") && require("ggdist")) {
#'   # Generate data =============================================
#'   k <- 11 # number of curves (iterations)
#'   n <- 201 # number of rows
#'   data <- data.frame(x = seq(-15, 15, length.out = n))
#'
#'   # Simulate iterations as new columns
#'   for (i in 1:k) {
#'     data[paste0("iter_", i)] <- dnorm(data$x, seq(-5, 5, length.out = k)[i], 3)
#'   }
#'
#'   # Note: first, we need to transpose the data to have iters as rows
#'   iters <- datawizard::data_transpose(data[paste0("iter_", 1:k)])
#'
#'   # Compute Median
#'   data$Median <- point_estimate(iters)[["Median"]]
#'
#'   # Compute Credible Intervals ================================
#'
#'   # Compute ETI (default type of CI)
#'   data[c("ETI_low", "ETI_high")] <- eti(iters, ci = 0.5)[c("CI_low", "CI_high")]
#'
#'   # Compute CWI
#'   # ggdist::curve_interval(reshape_iterations(data), iter_value .width = c(.5))
#'
#'   # Visualization =============================================
#'   ggplot(data, aes(x = x, y = Median)) +
#'     geom_ribbon(aes(ymin = ETI_low, ymax = ETI_high), fill = "red", alpha = 0.3) +
#'     geom_line(size = 1) +
#'     geom_line(
#'       data = reshape_iterations(data),
#'       aes(y = iter_value, group = iter_group),
#'       alpha = 0.3
#'     )
#' }
#' }
#' @export
cwi <- function(x, ...) {
  UseMethod("cwi")
}


#' @rdname cwi
#' @export
cwi.data.frame <- function(x, ci = 0.95, ...) {
  insight::check_if_installed("ggdist")

  print("Comming soon!") # @DominiqueMakowski GitBlame says this was 2 years ago - when is "soon"? :-)
}
