#' Methods for Bayes factors
#'
#' @param x,object Bayes factor object
#' @param ... Additional arguments (currently not used).
#'
#' @return
#' - `as.numeric()` / `as.double()` / `as.vector()`: a numeric vector of (log)
#'   Bayes factors.
#' - `as.logical()`: a logical data frame with a column for each
#'   order-restricted hypothesis.
#' - `as.matrix()`: a square matrix of (log) Bayes factors, with rows as
#'   denominators and columns as numerators.
#' - `update()`: an updated `bayesfactor_models` object.
#'
#' @section Interpreting Bayes Factors:
#' A Bayes factor greater than 1 can be interpreted as evidence against the
#' null, at which one convention is that a Bayes factor greater than 3 can be
#' considered as "substantial" evidence against the null (and vice versa, a
#' Bayes factor smaller than 1/3 indicates substantial evidence in favor of the
#' null-model). See also `effectsize::interpret_bf()`.
#'
#' @section Transitivity of Bayes factors:
#' For multiple inputs (models or hypotheses), the function will return multiple
#' Bayes factors between each model and _the same_ reference model (the
#' `denominator` or un-restricted model). However, we can take advantage of the
#' transitivity of Bayes factors - where if we have two Bayes factors for Model
#' _A_ and model _B_ against the _same reference model C_, we can obtain a Bayes
#' factor for comparing model _A_ to model _B_ by dividing them:
#' \cr\cr
#' \deqn{BF_{AB} = \frac{BF_{AC}}{BF_{BC}} = \frac{\frac{ML_{A}}{ML_{C}}}{\frac{ML_{B}}{ML_{C}}} = \frac{ML_{A}}{ML_{B}}}
#' \cr\cr
#' (Where _ML_ is the _marginal likelihood_.)
#' \cr\cr
#' A full matrix comparing all models can be obtained with `as.matrix()`.
#'
#' @section Prior and posterior considerations:
#' In order to correctly and precisely estimate Bayes factors, a rule of thumb
#' are the 4 P's: **P**roper **P**riors and **P**lentiful
#' **P**osteriors.
#' \cr\cr
#' For the computation of Bayes factors, the model priors must be proper priors
#' (at the very least they should be *not flat*, and it is preferable that they
#' be *informative*) (Note that by default, `brms::brm()` uses flat priors for
#' fixed-effects); Wide priors result in smaller marginal likelihoods, and thus
#' models with wider priors are trivially less likely than models with narrower
#' priors - where, at the extreme, that a model with completely flat priors is
#' infinitely less favorable than a point null model (this is called *the
#' Jeffreys-Lindley-Bartlett paradox*). Thus, you should only ever try (or want)
#' to compute a Bayes factor when you have an informed prior.
#' \cr\cr
#' Additionally, for models using MCMC estimation the number of posterior
#' samples needed for testing is substantially larger than for estimation (the
#' default of 4000 samples may not be enough in many cases). A conservative rule
#' of thumb is to obtain 10 times more samples than would be required for
#' estimation (_Gronau, Singmann, & Wagenmakers, 2017_). If less than 40,000
#' samples are detected, a warning is issued.
#'
#' @rdname bayesfactor_methods
#' @name bayesfactor_methods
NULL

## as.matrix -------------------------

#' @param log Return log(BF) (default), or BF values.
#'
#' @rdname bayesfactor_methods
#' @export
as.matrix.bayestestRBF <- function(x, log = TRUE, ...) {
  if (inherits(x, "bayesfactor_restricted")) {
    log_BFs <- c(0, x$log_BF)
    models <- c("(Un-restricted)", x$Hypothesis)
    bf_fun <- "bayesfactor_restricted()"
  } else if (inherits(x, "bayesfactor_models")) {
    log_BFs <- x$log_BF
    models <- x$Model
    bf_fun <- "bayesfactor_models()"
  } else {
    insight::format_error("Cannot extract a Bayes factor matrix from this object.")
  }

  out <- -outer(log_BFs, log_BFs, FUN = "-")
  rownames(out) <- colnames(out) <- models

  if (!log) {
    out <- exp(out)
  }

  class(out) <- c("bayesfactor_matrix", class(out))
  attr(out, "log_BF") <- log
  attr(out, "bf_fun") <- bf_fun
  out
}

#' @export
print.bayesfactor_matrix <- function(x, log = FALSE, ...) {
  orig_x <- x
  orig_log <- attr(x, "log_BF")

  # Format values
  x <- unclass(x)
  if (log) {
    if (!orig_log) x <- log(x)
    sgn <- sign(x) < 0
    x <- insight::format_value(abs(x), digits = 2, ...)

    if (any(sgn)) {
      x[sgn] <- paste0("-", x[sgn])
    }

    diag(x) <- "0"
  } else {
    if (orig_log) x <- exp(x)
    x <- insight::format_bf(x, name = NULL, exact = TRUE, ...)

    diag(x) <- "1"
  }

  df <- as.data.frame(x)

  # Model names
  models <- colnames(df)
  models[models == "1"] <- "(Intercept only)"
  models <- paste0("[", seq_along(models), "] ", models)

  rownames(df) <- colnames(df) <- NULL
  df <- cbind(modl = models, df)
  colnames(df) <- c(
    "Denominator\\Numerator",
    paste0(" [", seq_along(models), "] ")
  )

  # caption and footer
  caption <- switch(attr(orig_x, "bf_fun"),
                    "bayesfactor_restricted()" = "# Bayes Factors for Restricted Models",
                    "# Bayes Factors for Model Comparison"
  )
  footer <- if (log) c("\nBayes Factors are on the log-scale.\n", "red")

  out <- insight::export_table(
    df,
    caption = c(caption, "blue"),
    footer = footer
  )
  # Fix spacing
  out <- sub("Denominator", " Denominator", out, fixed = TRUE)

  cat(out)

  invisible(orig_x)
}


## update -------------------------

#' @param subset Vector of model indices to keep or remove.
#' @param reference Index of model to reference to, or `"top"` to
#'   reference to the best model, or `"bottom"` to reference to the worst
#'   model.
#'
#' @rdname bayesfactor_methods
#' @export
update.bayesfactor_models <- function(object, subset = NULL, reference = NULL, ...) {
  if (!is.null(reference)) {
    if (reference == "top") {
      reference <- which.max(object$log_BF)
    } else if (reference == "bottom") {
      reference <- which.min(object$log_BF)
    }
    object$log_BF <- object$log_BF - object$log_BF[reference]
    attr(object, "denominator") <- reference
  }

  denominator <- attr(object, "denominator")

  if (!is.null(subset)) {
    if (all(subset < 0)) {
      subset <- seq_len(nrow(object))[subset]
    }
    object_subset <- object[subset, ]

    if (denominator %in% subset) {
      attr(object_subset, "denominator") <- which(denominator == subset)
    } else {
      object_subset <- rbind(object[denominator, ], object_subset)
      attr(object_subset, "denominator") <- 1
    }
    object <- object_subset
  }
  object
}


## as.numeric -------------------------------------------------------

#' @rdname bayesfactor_methods
#' @export
as.numeric.bayestestRBF <- function(x, log = FALSE, ...) {
  out <- x[["log_BF"]]
  if (!log) out <- exp(out)
  return(out)
}

#' @export
as.double.bayestestRBF <- as.numeric.bayestestRBF

#' @export
as.vector.bayestestRBF <- as.numeric.bayestestRBF

## as.logical -----------------------------------------------------------------

#' @param which Should the logical matrix be of the posterior or prior distribution(s)?
#'
#' @rdname bayesfactor_methods
#' @export
as.logical.bayesfactor_restricted <- function(x, which = c("posterior", "prior"), ...) {
  which <- match.arg(which)
  as.matrix(attr(x, "bool_results")[[which]])
}




# Utils -----------------------------
# We need this to avoid argument conflicts with the non-generic as.vector
# For as.vector.bayestestRBF and as.vector.p_direction

#' @export
as.vector <- function(x, ...) {
  UseMethod("as.vector")
}

#' @export
as.vector.default <- function(x, ...) {
  base::as.vector(x)
}

