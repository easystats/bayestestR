#' Probability of Direction (pd)
#'
#' Compute the \strong{Probability of Direction} (\strong{\emph{pd}}, also known
#' as the Maximum Probability of Effect - \emph{MPE}). It varies between 50\%
#' and 100\% (\emph{i.e.}, \code{0.5} and \code{1}) and can be interpreted as
#' the probability (expressed in percentage) that a parameter (described by its
#' posterior distribution) is strictly positive or negative (whichever is the
#' most probable). It is mathematically defined as the proportion of the
#' posterior distribution that is of the median's sign. Although differently
#' expressed, this index is fairly similar (\emph{i.e.}, is strongly correlated)
#' to the frequentist \strong{p-value}.
#' \cr\cr
#' Note that in some (rare) cases, especially when used with model averaged
#' posteriors (see \code{\link{weighted_posteriors}} or
#' \code{brms::posterior_average}), \code{pd} can be smaller than \code{0.5},
#' reflecting high credibility of \code{0}.
#'
#' @param x Vector representing a posterior distribution. Can also be a Bayesian model (\code{stanreg}, \code{brmsfit} or \code{BayesFactor}).
#' @param method Can be \code{"direct"} or one of methods of \link[=estimate_density]{density estimation}, such as \code{"kernel"}, \code{"logspline"} or \code{"KernSmooth"}. If \code{"direct"} (default), the computation is based on the raw ratio of samples superior and inferior to 0. Else, the result is based on the \link[=auc]{Area under the Curve (AUC)} of the estimated \link[=estimate_density]{density} function.
#' @param null The value considered as a "null" effect. Traditionally 0, but could also be 1 in the case of ratios.
#' @inheritParams hdi
#'
#' @details
#' \subsection{What is the \emph{pd}?}{
#' The Probability of Direction (pd) is an index of effect existence, ranging from 50\% to 100\%, representing the certainty with which an effect goes in a particular direction (\emph{i.e.}, is positive or negative). Beyond its simplicity of interpretation, understanding and computation, this index also presents other interesting properties:
#' \itemize{
#'   \item It is independent from the model: It is solely based on the posterior distributions and does not require any additional information from the data or the model.
#'   \item It is robust to the scale of both the response variable and the predictors.
#'   \item It is strongly correlated with the frequentist p-value, and can thus be used to draw parallels and give some reference to readers non-familiar with Bayesian statistics.
#' }
#' }
#' \subsection{Relationship with the p-value}{
#' In most cases, it seems that the \emph{pd} has a direct correspondence with the frequentist one-sided \emph{p}-value through the formula \ifelse{html}{\out{p<sub>one&nbsp;sided</sub>&nbsp;=&nbsp;1&nbsp;-&nbsp;<sup>p(<em>d</em>)</sup>/<sub>100</sub>}}{\eqn{p_{one sided}=1-\frac{p_{d}}{100}}} and to the two-sided p-value (the most commonly reported one) through the formula \ifelse{html}{\out{p<sub>two&nbsp;sided</sub>&nbsp;=&nbsp;2&nbsp;*&nbsp;(1&nbsp;-&nbsp;<sup>p(<em>d</em>)</sup>/<sub>100</sub>)}}{\eqn{p_{two sided}=2*(1-\frac{p_{d}}{100})}}. Thus, a two-sided p-value of respectively \code{.1}, \code{.05}, \code{.01} and \code{.001} would correspond approximately to a \emph{pd} of 95\%, 97.5\%, 99.5\% and 99.95\%. See also \code{\link{pd_to_p}}.
#' }
#' \subsection{Methods of computation}{
#'  The most simple and direct way to compute the \emph{pd} is to 1) look at the median's sign, 2) select the portion of the posterior of the same sign and 3) compute the percentage that this portion represents. This "simple" method is the most straightforward, but its precision is directly tied to the number of posterior draws. The second approach relies on \link[=estimate_density]{density estimation}. It starts by estimating the density function (for which many methods are available), and then computing the \link[=area_under_curve]{area under the curve} (AUC) of the density curve on the other side of 0.
#' }
#' \subsection{Strengths and Limitations}{
#' \strong{Strengths:} Straightforward computation and interpretation. Objective property of the posterior distribution. 1:1 correspondence with the frequentist p-value.
#' \cr \cr
#' \strong{Limitations:} Limited information favoring the null hypothesis.
#' }
#'
#' @return
#' Values between 0.5 and 1 corresponding to the probability of direction (pd).
#' \cr\cr
#' Note that in some (rare) cases, especially when used with model averaged
#' posteriors (see \code{\link{weighted_posteriors}} or
#' \code{brms::posterior_average}), \code{pd} can be smaller than \code{0.5},
#' reflecting high credibility of \code{0}. To detect such cases, the
#' \code{method = "direct"} must be used.
#'
#' @seealso \code{\link{pd_to_p}} to convert between Probability of Direction (pd) and p-value.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @references Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of Effect Existence and Significance in the Bayesian Framework. Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
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
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' if (require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt + cyl,
#'     data = mtcars,
#'     chains = 2, refresh = 0
#'   )
#'   p_direction(model)
#'   p_direction(model, method = "kernel")
#' }
#'
#' # emmeans
#' # -----------------------------------------------
#' if (require("emmeans")) {
#'   p_direction(emtrends(model, ~1, "wt"))
#' }
#'
#' # brms models
#' # -----------------------------------------------
#' if (require("brms")) {
#'   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#'   p_direction(model)
#'   p_direction(model, method = "kernel")
#' }
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' if (require("BayesFactor")) {
#'   bf <- ttestBF(x = rnorm(100, 1, 1))
#'   p_direction(bf)
#'   p_direction(bf, method = "kernel")
#' }
#' }
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
p_direction.numeric <- function(x, method = "direct", null = 0, ...) {
  if (method == "direct") {
    pdir  <- max(
      c(
        length(x[x > null]) / length(x), # pd positive
        length(x[x < null]) / length(x) # pd negative
      )
    )
  } else {
    dens <- estimate_density(x, method = method, precision = 2^10, extend = TRUE, ...)
    if (length(x[x > null]) > length(x[x < null])) {
      dens <- dens[dens$x > null, ]
    } else {
      dens <- dens[dens$x < null, ]
    }
    pdir <- area_under_curve(dens$x, dens$y, method = "spline")
    if (pdir >= 1) pdir <- 1 # Enforce bounds
  }

  attr(pdir, "method") <- method
  attr(pdir, "data") <- x

  class(pdir) <- unique(c("p_direction", "see_p_direction", class(pdir)))

  pdir
}





#' @rdname p_direction
#' @export
p_direction.data.frame <- function(x, method = "direct", null = 0, ...) {
  obj_name <- .safe_deparse(substitute(x))
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    pd <- p_direction(x[, 1], method = method, null = null, ...)
  } else {
    pd <- sapply(x, p_direction, method = method, null = null, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "pd" = pd,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  attr(out, "object_name") <- obj_name
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))

  out
}


#' @rdname p_direction
#' @export
p_direction.MCMCglmm <- function(x, method = "direct", null = 0, ...) {
  nF <- x$Fixed$nfl
  out <- p_direction(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), method = method, null = null, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @export
p_direction.mcmc <- function(x, method = "direct", null = 0, ...) {
  p_direction(as.data.frame(x), method = method, null = null, ...)
}


#' @export
p_direction.bcplm <- function(x, method = "direct", null = 0, ...) {
  p_direction(insight::get_parameters(x), method = method, null = null, ...)
}

#' @export
p_direction.mcmc.list <- p_direction.bcplm

#' @export
p_direction.blrm <- p_direction.bcplm

#' @export
p_direction.bayesQR <- p_direction.bcplm


#' @export
p_direction.bamlss <- function(x, method = "direct", null = 0, component = c("all", "conditional", "location"), ...) {
  component <- match.arg(component)
  out <- p_direction(insight::get_parameters(x, component = component), method = method, null = null, ...)
  out <- .add_clean_parameters_attribute(out, x)
  out
}


#' @rdname p_direction
#' @export
p_direction.emmGrid <- function(x, method = "direct", null = 0, ...) {
  xdf <- insight::get_parameters(x)

  out <- p_direction(xdf, method = method, null = null, ...)
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}

#' @export
p_direction.emm_list <- p_direction.emmGrid

#' @importFrom insight get_parameters
#' @keywords internal
.p_direction_models <- function(x, effects, component, parameters, method = "direct", null = 0, ...) {
  p_direction(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), method = method, null = null, ...)
}


#' @export
p_direction.sim.merMod <- function(x, effects = c("fixed", "random", "all"), parameters = NULL, method = "direct", null = 0, ...) {
  effects <- match.arg(effects)

  out <- .p_direction_models(
    x = x,
    effects = effects,
    component = "conditional",
    parameters = parameters,
    method = method,
    null = null,
    ...
  )
  attr(out, "data") <- insight::get_parameters(x, effects = effects, parameters = parameters)
  out
}


#' @export
p_direction.sim <- function(x, parameters = NULL, method = "direct", null = 0, ...) {
  out <- .p_direction_models(
    x = x,
    effects = "fixed",
    component = "conditional",
    parameters = parameters,
    method = method,
    null = null,
    ...
  )
  attr(out, "data") <- insight::get_parameters(x, parameters = parameters)
  out
}



#' @rdname p_direction
#' @export
p_direction.stanreg <- function(x, effects = c("fixed", "random", "all"), component = c("location", "all", "conditional", "smooth_terms", "sigma", "distributional", "auxiliary"), parameters = NULL, method = "direct", null = 0, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    p_direction(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), method = method, null = null, ...),
    cleaned_parameters,
    inherits(x, "stanmvreg")
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_direction.stanfit <- p_direction.stanreg

#' @export
p_direction.blavaan <- p_direction.stanreg


#' @rdname p_direction
#' @export
p_direction.brmsfit <- function(x, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, method = "direct", null = 0, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  cleaned_parameters <- insight::clean_parameters(x)

  out <- .prepare_output(
    p_direction(insight::get_parameters(x, effects = effects, component = component, parameters = parameters), method = method, null = null, ...),
    cleaned_parameters
  )

  attr(out, "clean_parameters") <- cleaned_parameters
  class(out) <- unique(c("p_direction", "see_p_direction", class(out)))
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}


#' @rdname p_direction
#' @export
p_direction.BFBayesFactor <- function(x, method = "direct", null = 0, ...) {
  out <- p_direction(insight::get_parameters(x), method = method, null = null, ...)
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

#' @export
p_direction.get_predicted <- function(x, ...) {
  if ("iterations" %in% names(attributes(x))) {
    out <- p_direction(as.data.frame(t(attributes(x)$iterations)), ...)
  } else {
    stop("No iterations present in the output.")
  }
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  out
}

# Methods -----------------------------------------------------------------


#' Convert to Numeric
#'
#' @inheritParams base::as.numeric
#' @method as.numeric p_direction
#' @export
as.numeric.p_direction <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$pd)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_direction
#' @export
as.double.p_direction <- as.numeric.p_direction
