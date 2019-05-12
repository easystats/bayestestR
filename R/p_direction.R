#' Probability of Direction (pd)
#'
#' Compute the \strong{Probability of Direction} (\strong{\emph{p}d}, also known as the Maximum Probability of Effect - \emph{MPE}). It varies between 50\% and 100\% and can be interpreted as the probability (expressed in percentage) that a parameter (described by its posterior distribution) is strictly positive or negative (whichever is the most probable). It is mathematically defined as the proportion of the posterior distribution that is of the median's sign. Altough differently expressed, this index is fairly similar (\emph{i.e.}, is strongly correlated) to the frequentist \strong{\emph{p}-value}.
#'
#' @param x Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param method Can be \code{"direct"} or one of methods of \link[=estimate_density]{density estimation}, such as \code{"kernel"}, \code{"logspline"} or \code{"KernSmooth"}. If \code{"direct"} (default), the computation is based on the raw ratio of samples superior and inferior to 0. Else, the result is based on the \link[=auc]{Area under the Curve (AUC)} of the estimated \link[=estimate_density]{density} function.
#' @inheritParams hdi
#'
#' @details \strong{Relationship with the p-value}: In most cases, it seems that the \emph{p}d corresponds to the frequentist one-sided \emph{p}-value through the formula \eqn{p_{two sided} = 2*(1-\frac{p_{d}}{100})} and to the two-sided \emph{p}-value (\emph{the most commonly reported one} through the formula \eqn{p_{one sided} = 2*(1-\frac{p_{d}}{100})}. Thus, a \code{pd} of \code{95\%}, \code{97.5\%}, \code{99.5\%} and \code{99.95\%} corresponds approximately to a\emph{p}-value of respectively \code{.1}, \code{.05}, \code{.01} and \code{.001}. See the \href{https://easystats.github.io/bayestestR/articles/guidelines.html}{\emph{reporting guidelines}}.
#'
#' @examples
#' library(bayestestR)
#'
#' # Simulate a posterior distribution of mean 1 and SD 1
#' # ----------------------------------------------------
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' p_direction(posterior)
#' p_direction(posterior, method = "kernel")
#'
#' # Simulate a dataframe of posterior distributions
#' # -----------------------------------------------
#' df <- data.frame(replicate(4, rnorm(100)))
#' p_direction(df)
#' p_direction(df, method = "kernel")
#'
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' p_direction(model)
#' p_direction(model, method = "kernel")
#'
#' # brms models
#' # -----------------------------------------------
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' p_direction(model)
#' p_direction(model, method = "kernel")
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' p_direction(bf)
#' p_direction(bf, method = "kernel")
#' }
#'
#' \dontrun{
#' # Comparison of methods
#' # -----------------------------------------------
#' library(logspline)
#' library(KernSmooth)
#'
#' data <- data.frame()
#' for(i in rnorm(8, 0, 4)){
#'   for(j in rnorm(8, 0.5, 4)){
#'     x <- rnorm(1000, i, abs(j))
#'     data <- rbind(data,
#'       data.frame("direct" = pd(x),
#'                  "kernel" = pd(x, method="kernel"),
#'                  "logspline" = pd(x, method="logspline"),
#'                  "KernSmooth" = pd(x, method="KernSmooth")
#'                  ))
#'   }
#' }
#' pairs(~., data=data)
#' }
#'
#'
#' @export
p_direction <- function(x, ...) {
  UseMethod("p_direction")
}

#' @rdname p_direction
#' @export
pd <- p_direction




#' @importFrom stats density
#' @rdname p_direction
#' @export
p_direction.numeric <- function(x, method = "direct", ...) {
  if (method == "direct") {
    p_direction <- 100 * max(
      c(
        length(x[x > 0]) / length(x), # pd positive
        length(x[x < 0]) / length(x) # pd negative
      )
    )
  } else {
    dens <- estimate_density(x, method=method, precision = 2^10, extend = TRUE, ...)
    if (length(x[x > 0]) > length(x[x < 0])) {
      dens <- dens[dens$x > 0, ]
    } else {
      dens <- dens[dens$x < 0, ]
    }
    p_direction <- area_under_curve(dens$x, dens$y, method = "spline") * 100
    if (p_direction >= 100) p_direction <- 100
  }

  attr(p_direction, "method") <- method
  class(p_direction) <- c("p_direction", class(p_direction))
  p_direction
}





#' @rdname p_direction
#' @export
p_direction.data.frame <- function(x, method = "direct", ...) {
  if(ncol(x) == 1){
    pd <- p_direction(x[, 1], method = method, ...)
  } else{
    pd <- sapply(.select_nums(x), p_direction, method = method, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "pd" = pd,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  class(out) <- c("p_direction", class(out))
  out

}



#' @importFrom insight get_parameters
#' @keywords internal
.p_direction_models <- function(x, effects, component, parameters, method = "direct", ...) {

  out <- p_direction(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), method = method, ...)
  out$Parameter <- .get_parameter_names(x, effects = effects, component = component, parameters = parameters)

  out
}


#' @rdname p_direction
#' @export
p_direction.stanreg <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, method = "direct", ...) {
  effects <- match.arg(effects)

  out <- .p_direction_models(
    x = x,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    method = method,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @rdname p_direction
#' @export
p_direction.brmsfit <- function(x, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, method = "direct", ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .p_direction_models(
    x = x,
    effects = effects,
    component = component,
    parameters = parameters,
    method = method,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' @rdname p_direction
#' @export
p_direction.BFBayesFactor <- function(x, method = "direct", ...) {
  out <- p_direction(insight::get_parameters(x), method = method, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}


#' Numeric Vectors
#'
#' @inheritParams base::as.numeric
#' @method as.numeric p_direction
#' @export
as.numeric.p_direction <- function(x, ...){
  if("data.frame" %in% class(x)){
    return(as.numeric(as.vector(x$pd)))
  } else{
    return(as.vector(x))
  }
}


#' @method as.double p_direction
#' @export
as.double.p_direction <- as.numeric.p_direction
