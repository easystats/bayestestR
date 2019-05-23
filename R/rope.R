#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion (in percentage) of the HDI (default to the 90\% HDI) of a posterior distribution that lies within a region of practical equivalence.
#'
#' @param x Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param range ROPE's lower and higher bounds. Should be a vector of length two (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the range is set to \code{c(-0.1, 0.1)} if input is a vector, and based on \code{\link[=rope_range]{rope_range()}} if a Bayesian model is provided.
#' @param ci The Credible Interval (CI) probability, corresponding to the proportion of HDI, to use for the percentage in ROPE.
#'
#' @inheritParams hdi
#'
#' @details Statistically, the probability of a posterior distribution of being
#'   different from 0 does not make much sense (the probability of it being
#'   different from a single point being 100%). Therefore, the idea
#'   underlining ROPE is to let the user define an area around the null value
#'   enclosing values that are \emph{equivalent to the null} value for practical
#'   purposes (\cite{Kruschke 2010, 2011, 2014}).
#'   \cr \cr
#'   Kruschke (2018) suggests that such null value could be set, by default,
#'   to the -0.1 to 0.1 range of a standardized parameter (negligible effect
#'   size according to Cohen, 1988). This could be generalized: For instance,
#'   for linear models, the ROPE could be set as \code{0 +/- .1 * sd(y)}.
#'   This ROPE range can be automatically computed for models using the
#'   \link{rope_range} function.
#'   \cr \cr
#'   Kruschke (2010, 2011, 2014) suggests using the proportion of  the 95\%
#'   (or 89\%, considered more stable) \link[=hdi]{HDI} that falls within the
#'   ROPE as an index for "null-hypothesis" testing (as understood under the
#'   Bayesian framework, see \code{\link[=equivalence_test]{equivalence_test()}}).
#'   \cr \cr
#'   \strong{Multicollinearity: Non-independent covariates}
#'   \cr \cr
#'   When parameters show strong correlations, i.e. when covariates are not
#'   independent, the joint parameter distributions may shift towards or
#'   away from the ROPE. Collinearity invalidates ROPE and hypothesis
#'   testing based on univariate marginals, as the probabilities are conditional
#'   on independence. Most problematic are parameters that only have partial
#'   overlap with the ROPE region. In case of collinearity, the (joint) distributions
#'   of these parameters may either get an increased or decreased ROPE, which
#'   means that inferences based on \code{rope()} are inappropriate
#'   (\cite{Kruschke 2014, 340f}).
#'   \cr \cr
#'   \code{rope()} performs a simple check for pairwise correlations between
#'   parameters, but as there can be collinearity between more than two variables,
#'   a first step to check the assumptions of this hypothesis testing is to look
#'   at different pair plots. An even more sophisticated check is the projection
#'   predictive variable selection (\cite{Piironen and Vehtari 2017}).
#'
#' @references \itemize{
#' \item Cohen, J. (1988). Statistical power analysis for the behavioural sciences.
#' \item Kruschke, J. K. (2010). What to believe: Bayesian methods for data analysis. Trends in cognitive sciences, 14(7), 293-300. \doi{10.1016/j.tics.2010.05.001}.
#' \item Kruschke, J. K. (2011). Bayesian assessment of null values via parameter estimation and model comparison. Perspectives on Psychological Science, 6(3), 299-312. \doi{10.1177/1745691611406925}.
#' \item Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan. Academic Press. \doi{10.1177/2515245918771304}.
#' \item Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. \doi{10.1177/2515245918771304}.
#' \item Piironen, J., & Vehtari, A. (2017). Comparison of Bayesian predictive methods for model selection. Statistics and Computing, 27(3), 711–735. \doi{10.1007/s11222-016-9649-y}
#' }
#'
#' @examples
#' library(bayestestR)
#'
#' rope(x = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' rope(x = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#' rope(x = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
#' rope(x = rnorm(1000, 1, 1), ci = c(.90, .95))
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' rope(model)
#' rope(model, ci = c(.90, .95))
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' rope(model)
#' rope(model, ci = c(.90, .95))
#'
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' rope(bf)
#' rope(bf, ci = c(.90, .95))
#' }
#'
#' @importFrom insight get_parameters is_multivariate
#' @export
rope <- function(x, ...) {
  UseMethod("rope")
}


#' @method as.double rope
#' @export
as.double.rope <- function(x, ...) {
  x$ROPE_Percentage
}



#' @rdname rope
#' @export
rope.default <- function(x, ...) {
  NULL
}



#' @rdname rope
#' @export
rope.numeric <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  if (all(range == "default")) {
    range <- c(-0.1, 0.1)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  rope_values <- lapply(ci, function(i) {
    .rope(x, range = range, ci = i, verbose = verbose)
  })

  # "do.call(rbind)" does not bind attribute values together
  # so we need to capture the information about HDI separately


  out <- do.call(rbind, rope_values)
  if (nrow(out) > 1) {
    out$ROPE_Percentage <- as.numeric(out$ROPE_Percentage)
  }

  # Attributes
  hdi_area <- cbind(CI = ci * 100, data.frame(do.call(rbind, lapply(rope_values, attr, "HDI_area"))))
  names(hdi_area) <- c("CI", "CI_low", "CI_high")

  attr(out, "HDI_area") <- hdi_area
  attr(out, "data") <- x

  class(out) <- unique(c("rope", "see_rope", class(out)))

  out
}




#' @rdname rope
#' @export
rope.data.frame <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  out <- .prepare_rope_df(x, range, ci, verbose)
  HDI_area_attributes <- .compact_list(out$HDI_area)
  dat <- data.frame(
    Parameter = rep(names(HDI_area_attributes), each = length(ci)),
    out$tmp,
    stringsAsFactors = FALSE
  )
  row.names(dat) <- NULL

  attr(dat, "HDI_area") <- HDI_area_attributes
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)

  class(dat) <- c("rope", "see_rope", "data.frame")
  dat
}



#' @rdname rope
#' @export
rope.BFBayesFactor <- function(x, range = "default", ci = .89, verbose = TRUE, ...) {
  out <- rope(insight::get_parameters(x), range = range, ci = ci, verbose = verbose, ...)
  out
}



.rope <- function(x, range = c(-0.1, 0.1), ci = .89, verbose = TRUE) {
  HDI_area <- .hdi_area <- hdi(x, ci, verbose)

  if (anyNA(HDI_area)) {
    rope_percentage <- NA
  } else {
    HDI_area <- x[x >= HDI_area$CI_low & x <= HDI_area$CI_high]
    area_within <- HDI_area[HDI_area >= min(range) & HDI_area <= max(range)]
    rope_percentage <- length(area_within) / length(HDI_area) * 100
  }


  rope <- data.frame(
    "CI" = ci * 100,
    "ROPE_low" = range[1],
    "ROPE_high" = range[2],
    "ROPE_Percentage" = rope_percentage
  )

  attr(rope, "HDI_area") <- c(.hdi_area$CI_low, .hdi_area$CI_high)
  class(rope) <- unique(c("rope", "see_rope", class(rope)))
  rope
}



#' @rdname rope
#' @export
rope.stanreg <- function(x, range = "default", ci = .89, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)

  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  # check for possible collinearity that might bias ROPE
  if (verbose) .check_multicollinearity(x, "rope")

  list <- lapply(c("fixed", "random"), function(.x) {
    parms <- insight::get_parameters(x, effects = .x, parameters = parameters)

    getropedata <- .prepare_rope_df(parms, range, ci, verbose)
    tmp <- getropedata$tmp
    HDI_area <- getropedata$HDI_area

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_stanreg(
        tmp,
        group = .x,
        cols = c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Group"),
        parms = names(parms)
      )

      if (!.is_empty_object(HDI_area)) {
        attr(tmp, "HDI_area") <- HDI_area
      }
    } else {
      tmp <- NULL
    }

    tmp
  })

  dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))

  dat <- switch(
    effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- .remove_column(dat, "Group")
  }

  HDI_area_attributes <- lapply(.compact_list(list), attr, "HDI_area")

  if (effects != "all") {
    HDI_area_attributes <- HDI_area_attributes[[1]]
  } else {
    names(HDI_area_attributes) <- c("fixed", "random")
  }

  attr(dat, "HDI_area") <- HDI_area_attributes
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)

  dat
}



#' @rdname rope
#' @export
rope.brmsfit <- function(x, range = "default", ci = .89, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (insight::is_multivariate(x)) {
    stop("Multivariate response models are not yet supported.")
  }

  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  if (all(range == "default")) {
    range <- rope_range(x)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  # check for possible collinearity that might bias ROPE
  if (verbose) .check_multicollinearity(x, "rope")

  .get_rope <- function(.x, .y) {
    parms <- insight::get_parameters(x, effects = .x, component = .y, parameters = parameters)

    getropedata <- .prepare_rope_df(parms, range, ci, verbose)
    tmp <- getropedata$tmp
    HDI_area <- getropedata$HDI_area

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_brms(
        tmp,
        group = .x,
        component = .y,
        cols = c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Component", "Group"),
        parms = names(parms)
      )

      if (!.is_empty_object(HDI_area)) {
        attr(tmp, "HDI_area") <- HDI_area
      }
    } else {
      tmp <- NULL
    }

    tmp
  }

  list <- mapply(.get_rope, eff, com, SIMPLIFY = FALSE)
  dat <- do.call(rbind, args = c(.compact_list(list), make.row.names = FALSE))

  dat <- switch(
    effects,
    fixed = .select_rows(dat, "Group", "fixed"),
    random = .select_rows(dat, "Group", "random"),
    dat
  )

  dat <- switch(
    component,
    conditional = .select_rows(dat, "Component", "conditional"),
    zi = ,
    zero_inflated = .select_rows(dat, "Component", "zero_inflated"),
    dat
  )

  if (all(dat$Group == dat$Group[1])) {
    dat <- .remove_column(dat, "Group")
  }

  if (all(dat$Component == dat$Component[1])) {
    dat <- .remove_column(dat, "Component")
  }

  HDI_area_attributes <- lapply(.compact_list(list), attr, "HDI_area")

  if (effects != "all") {
    HDI_area_attributes <- HDI_area_attributes[[1]]
  } else {
    names(HDI_area_attributes) <- c("fixed", "random")
  }

  attr(dat, "HDI_area") <- HDI_area_attributes
  attr(dat, "object_name") <- deparse(substitute(x), width.cutoff = 500)

  dat
}



#' @keywords internal
.prepare_rope_df <- function(parms, range, ci, verbose) {
  tmp <- sapply(
    parms,
    rope,
    range = range,
    ci = ci,
    verbose = verbose,
    simplify = FALSE
  )

  HDI_area <- lapply(tmp, function(.x) {
    attr(.x, "HDI_area")
  })

  # HDI_area <- lapply(HDI_area, function(.x) {
  #   dat <- cbind(CI = ci, data.frame(do.call(rbind, .x)))
  #   colnames(dat) <- c("CI", "HDI_low", "HDI_high")
  #   dat
  # })

  list(
    tmp = do.call(rbind, tmp),
    HDI_area = HDI_area
  )
}
