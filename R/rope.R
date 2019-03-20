#' Region of Practical Equivalence (ROPE)
#'
#' Compute the proportion (in percentage) of the HDI (default to the 90\% HDI) of a posterior distribution that lies within a region of practical equivalence.
#'
#' @param posterior Vector representing a posterior distribution. Can also be a \code{stanreg} or \code{brmsfit} model.
#' @param range ROPE's lower and higher bounds. Should be a vector of length two (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the range is set to \code{c(0.1, 0.1)} if input is a vector, and \code{x +- 0.1*SD(response)} if a Bayesian model is provided.
#' @param ci The Credible Interval (CI) probability, corresponding to the proportion of HDI, to use.
#'
#' @inheritParams hdi
#'
#' @details Statistically, the probability of a posterior distribution of being
#'   different from 0 does not make much sense (the probability of it being
#'   different from a single point being infinite). Therefore, the idea
#'   underlining ROPE is to let the user define an area around the null value
#'   enclosing values that are equivalent to the null value for practical
#'   purposes (\cite{Kruschke 2010, 2011, 2014}).
#'   \cr \cr
#'   Kruschke (2018) suggests that such null value could be set, by default,
#'   to the -0.1 to 0.1 range of a standardized parameter (negligible effect
#'   size according to Cohen, 1988). This could be generalized: For instance,
#'   for linear models, the ROPE could be set as \code{0 +/- .1 * sd(y)}.
#'   \cr \cr
#'   Kruschke (2010, 2011, 2014) suggests using the proportion of  the 95\%
#'   (or 90\%, considered more stable) \link[=hdi]{HDI} that falls within the
#'   ROPE as an index for "null-hypothesis" testing (as understood under the
#'   Bayesian framework, see \link[=equivalence_test]{equivalence_test}).
#'
#' @examples
#' library(bayestestR)
#'
#' rope(posterior = rnorm(1000, 0, 0.01), range = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 0, 1), range = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 1, 0.01), range = c(-0.1, 0.1))
#' rope(posterior = rnorm(1000, 1, 1), ci = c(.90, .95))
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
#' }
#'
#' @importFrom insight get_parameters is_multivariate print_color
#' @export
rope <- function(posterior, ...) {
  UseMethod("rope")
}


#' @method as.double rope
#' @export
as.double.rope <- function(x, ...) {
  x$ROPE_Percentage
}


#' @export
print.rope <- function(x, digits = 2, ...) {
  insight::print_color(sprintf(
    "# Proportion%s of samples inside the ROPE [%.*f, %.*f]:\n\n",
    ifelse(all(x$CI[1] == x$CI), "", "s"),
    digits,
    x$ROPE_low[1],
    digits,
    x$ROPE_high[1]
  ), "blue")


  # I think this is something nobody will understand and we'll probably forget
  # why we did this, so I'll comment a bit...

  # These are the base columns we want to print
  cols <- c("Parameter", "ROPE_Percentage", "Component", "Group")

  # In case we have ropes for different CIs, we also want this information
  # So we first check if values in the CI column differ, and if so, we also
  # keep this column for printing
  if (!all(x$CI[1] == x$CI))
    cols <- c("CI", cols)

  # Either way, we need to know the different CI-values, so we can
  # split the data frame for printing later...
  ci <- unique(x$CI)

  # now we check which of the requested columns are actually in our data frame "x"
  # "x" may differ, depending on if "rope()" was called with a model-object,
  # or with a simple vector. So we can't hard-code this
  x <- subset(x, select = intersect(cols, colnames(x)))

  # This is just cosmetics, to have nicer column names and values
  x$ROPE_Percentage <- sprintf("%.*f %%", digits, x$ROPE_Percentage)
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  # In case we have multiple CI values, we create a subset for each CI value.
  # Else, parameter-rows would be mixed up with both CIs, which is a bit
  # more difficult to read...

  if (length(ci) == 1) {
    # print complete data frame, because we have no different CI values here
    print_data_frame(x, digits = digits)
  } else {
    for (i in ci) {
      xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
      insight::print_color(sprintf("ROPE for the %s%% HDI:\n\n", i), "cyan")
      print_data_frame(xsub, digits = digits)
      cat("\n")
    }
  }
}


#' @rdname rope
#' @export
rope.numeric <- function(posterior, range = "default", ci = .90, verbose = TRUE, ...) {
  if (all(range == "default")) {
    range <- c(-0.1, 0.1)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  rope_values <- lapply(ci, function(i) {
    .rope(posterior, range = range, ci = i, verbose = verbose)
  })

  # "do.call(rbind)" does not bind attribute values together
  # so we need to capture the information about HDI separately

  hdi_area <- lapply(rope_values, attr, "HDI_area")

  out <- do.call(rbind, rope_values)
  if (nrow(out) > 1) {
    out$ROPE_Percentage <- as.numeric(out$ROPE_Percentage)
  }

  attr(out, "HDI_area") <- hdi_area
  out
}


.rope <- function(posterior, range = c(-0.1, 0.1), ci = .90, verbose = TRUE) {
  HDI_area <- .hdi_area <- hdi(posterior, ci, verbose)

  if (anyNA(HDI_area)) {
    rope_percentage <- NA
  } else {
    HDI_area <- posterior[posterior >= HDI_area$CI_low & posterior <= HDI_area$CI_high]
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
  class(rope) <- c("rope", class(rope))
  rope
}


#' @rdname rope
#' @export
rope.stanreg <- function(posterior, range = "default", ci = .90, effects = c("fixed", "random", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)

  if (all(range == "default")) {
    range <- rope_range(posterior)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  list <- lapply(c("fixed", "random"), function(x) {
    parms <- insight::get_parameters(posterior, effects = x, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      rope,
      range = range,
      ci = ci,
      verbose = verbose,
      simplify = FALSE)
    )

    HDI_area <- attr(tmp, "HDI_area")

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_stanreg(
        tmp,
        x,
        cols = c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    attr(tmp, "HDI_area") <- HDI_area
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

  attr(dat, "HDI_area") <- do.call(rbind, lapply(list, attr, "HDI_area"))
  dat
}


#' @rdname rope
#' @export
rope.brmsfit <- function(posterior, range = "default", ci = .90, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if (insight::is_multivariate(posterior)) {
    stop("Multivariate response models are not yet supported.")
  }

  eff <- c("fixed", "fixed", "random", "random")
  com <- c("conditional", "zi", "conditional", "zi")

  if (all(range == "default")) {
    range <- rope_range(posterior)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  .get_rope <- function(x, y) {
    parms <- insight::get_parameters(posterior, effects = x, component = y, parameters = parameters)
    tmp <- do.call(rbind, sapply(
      parms,
      rope,
      range = range,
      ci = ci,
      verbose = verbose,
      simplify = FALSE)
    )

    HDI_area <- attr(tmp, "HDI_area")

    if (!.is_empty_object(tmp)) {
      tmp <- .clean_up_tmp_brms(
        tmp,
        x,
        y,
        cols = c("CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Component", "Group"),
        parms = names(parms)
      )
    } else {
      tmp <- NULL
    }

    attr(tmp, "HDI_area") <- HDI_area
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

  attr(dat, "HDI_area") <- do.call(rbind, lapply(list, attr, "HDI_area"))
  dat
}
