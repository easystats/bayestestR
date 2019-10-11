#' Practical Significance (ps)
#'
#' Compute the probability of \strong{Practical Significance} (\strong{\emph{ps}}), which can be conceptualized as a unidirectional equivalence test. It returns the probability that effect is above a given threshold corresponding to a negligible effect in the median's direction. Mathematically, it is defined as the proportion of the posterior distribution of the median sign above the threshold.
#'
#' @inheritParams rope
#' @param threshold The threshold value that separates significant from negligible effect. If \code{"default"}, the range is set to \code{0.1} if input is a vector, and based on \code{\link[=rope_range]{rope_range()}} if a Bayesian model is provided.
#'
#' @return Values between 0.5 and 1 corresponding to the probability of practical significance (ps).
#'
#' @examples
#' library(bayestestR)
#'
#' # Simulate a posterior distribution of mean 1 and SD 1
#' # ----------------------------------------------------
#' posterior <- rnorm(1000, mean = 1, sd = 1)
#' p_significance(posterior)
#'
#' # Simulate a dataframe of posterior distributions
#' # -----------------------------------------------
#' df <- data.frame(replicate(4, rnorm(100)))
#' p_significance(df)
#'
#' # rstanarm models
#' # -----------------------------------------------
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars,
#'                             chains = 2, refresh = 0)
#' p_significance(model)
#'
#' @export
p_significance <- function(x, ...) {
  UseMethod("p_significance")
}




#' @rdname p_significance
#' @export
p_significance.numeric <- function(x, threshold = "default", ...) {

  threshold <- .select_threshold_ps(x = x, threshold = threshold)

  psig <- max(
    c(
      length(x[x > abs(threshold)]) / length(x), # ps positive
      length(x[x < -abs(threshold)]) / length(x) # ps negative
    )
  )

  attr(psig, "threshold") <- threshold
  attr(psig, "data") <- x

  class(psig) <- unique(c("p_significance", "see_p_significance", class(psig)))

  psig
}


#' @rdname p_significance
#' @export
p_significance.data.frame <- function(x, threshold = "default", ...) {

  threshold <- .select_threshold_ps(x = x, threshold = threshold)
  x <- .select_nums(x)

  if (ncol(x) == 1) {
    ps <- p_significance(x[, 1], threshold = threshold, ...)
  } else {
    ps <- sapply(x, p_significance, threshold = threshold, simplify = TRUE, ...)
  }

  out <- data.frame(
    "Parameter" = names(x),
    "ps" = as.numeric(ps),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  class(out) <- unique(c("p_significance", "see_p_significance", class(out)))

  out
}




#' @rdname p_significance
#' @export
p_significance.MCMCglmm <- function(x, threshold = "default", ...) {
  nF <- x$Fixed$nfl
  p_significance(as.data.frame(x$Sol[, 1:nF, drop = FALSE]), threshold = threshold, ...)
}


#' @rdname p_significance
#' @export
p_significance.emmGrid <- function(x, threshold = "default", ...) {
  if (!requireNamespace("emmeans")) {
    stop("Package 'emmeans' required for this function to work. Please install it by running `install.packages('emmeans')`.")
  }
  xdf <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(x, names = FALSE)))
  out <- p_significance(xdf, threshold = threshold, ...)

  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  out
}







#' @rdname p_significance
#' @export
p_significance.stanreg <- function(x, threshold = "default", effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  threshold <- .select_threshold_ps(model = x, threshold = threshold)

  data <- p_significance(
    insight::get_parameters(x, effects = effects, parameters = parameters),
    threshold = threshold)

  out <- .prepare_output(data, insight::clean_parameters(x))

  attr(out, "threshold") <- threshold
  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- class(data)

  out
}





#' @rdname as.numeric.p_direction
#' @export
as.numeric.p_significance <- function(x, ...) {
  if ("data.frame" %in% class(x)) {
    return(as.numeric(as.vector(x$ps)))
  } else {
    return(as.vector(x))
  }
}


#' @method as.double p_significance
#' @export
as.double.p_significance <- as.numeric.p_significance



#' @keywords internal
.select_threshold_ps <- function(x = NULL, model = NULL, threshold = "default"){
  if (all(threshold == "default")) {
    if(!is.null(model)){
      threshold <- rope_range(model)[2]
    } else{
      threshold <- 0.1
    }
  } else if (!all(is.numeric(threshold))) {
    stop("`threshold` should be 'default' or a numeric value (e.g., 0.1).")
  }
  threshold
}

