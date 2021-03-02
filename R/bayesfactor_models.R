#' Bayes Factors (BF) for model comparison
#'
#' @description This function computes or extracts Bayes factors from fitted models.
#' \cr \cr
#' The \code{bf_*} function is an alias of the main function.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @param ... Fitted models (see details), all fit on the same data, or a single
#'   \code{BFBayesFactor} object (see 'Details'). Ignored in \code{as.matrix()},
#'   \code{update()}.
#' @param denominator Either an integer indicating which of the models to use as
#'   the denominator, or a model to be used as a denominator. Ignored for
#'   \code{BFBayesFactor}.
#' @param object,x A \code{\link{bayesfactor_models}} object.
#' @param subset Vector of model indices to keep or remove.
#' @param reference Index of model to rereference to, or \code{"top"} to
#'   reference to the best model, or \code{"bottom"} to reference to the worst
#'   model.
#' @inheritParams hdi
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/bayestestR.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details
#' If the passed models are supported by \pkg{insight} the DV of all models will be tested for equality
#' (else this is assumed to be true), and the models' terms will be extracted (allowing for follow-up
#' analysis with \code{bayesfactor_inclusion}).
#'
#' \itemize{
#'   \item For \code{brmsfit} or \code{stanreg} models, Bayes factors are computed using the \CRANpkg{bridgesampling} package.
#'   \itemize{
#'     \item \code{brmsfit} models must have been fitted with \code{save_pars = save_pars(all = TRUE)}.
#'     \item \code{stanreg} models must have been fitted with a defined \code{diagnostic_file}.
#'   }
#'   \item For \code{BFBayesFactor}, \code{bayesfactor_models()} is mostly a wraparound \code{BayesFactor::extractBF()}.
#'   \item For all other model types (supported by \CRANpkg{insight}), BIC approximations are used to compute Bayes factors.
#' }
#' In order to correctly and precisely estimate Bayes factors, a rule of thumb
#' are the 4 P's: \strong{P}roper \strong{P}riors and \strong{P}lentiful
#' \strong{P}osteriors. How many? The number of posterior samples needed for
#' testing is substantially larger than for estimation (the default of 4000
#' samples may not be enough in many cases). A conservative rule of thumb is to
#' obtain 10 times more samples than would be required for estimation
#' (\cite{Gronau, Singmann, & Wagenmakers, 2017}). If less than 40,000 samples
#' are detected, \code{bayesfactor_models()} gives a warning.
#' \cr \cr
#' See also \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.
#'
#' @inheritSection bayesfactor_parameters Interpreting Bayes Factors
#'
#' @return A data frame containing the models' formulas (reconstructed fixed and random effects) and their BFs, that prints nicely.
#'
#' @examples
#' # With lm objects:
#' # ----------------
#' lm1 <- lm(Sepal.Length ~ 1, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm4 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' bayesfactor_models(lm1, lm2, lm3, lm4, denominator = 1)
#' bayesfactor_models(lm2, lm3, lm4, denominator = lm1) # same result
#' BFM <- bayesfactor_models(lm1, lm2, lm3, lm4, denominator = lm1) # same result
#'
#' update(BFM, reference = "bottom")
#' as.matrix(BFM)
#' \dontrun{
#' # With lmerMod objects:
#' # ---------------------
#' if (require("lme4")) {
#'   lmer1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   lmer2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
#'   lmer3 <- lmer(
#'     Sepal.Length ~ Petal.Length + (Petal.Length | Species) + (1 | Petal.Width),
#'     data = iris
#'   )
#'   bayesfactor_models(lmer1, lmer2, lmer3, denominator = 1)
#'   bayesfactor_models(lmer1, lmer2, lmer3, denominator = lmer1)
#' }
#'
#' # rstanarm models
#' # ---------------------
#' # (note that a unique diagnostic_file MUST be specified in order to work)
#' if (require("rstanarm")) {
#'   stan_m0 <- stan_glm(Sepal.Length ~ 1,
#'     data = iris,
#'     family = gaussian(),
#'     diagnostic_file = file.path(tempdir(), "df0.csv")
#'   )
#'   stan_m1 <- stan_glm(Sepal.Length ~ Species,
#'     data = iris,
#'     family = gaussian(),
#'     diagnostic_file = file.path(tempdir(), "df1.csv")
#'   )
#'   stan_m2 <- stan_glm(Sepal.Length ~ Species + Petal.Length,
#'     data = iris,
#'     family = gaussian(),
#'     diagnostic_file = file.path(tempdir(), "df2.csv")
#'   )
#'   bayesfactor_models(stan_m1, stan_m2, denominator = stan_m0)
#' }
#'
#'
#' # brms models
#' # --------------------
#' # (note the save_pars MUST be set to save_pars(all = TRUE) in order to work)
#' if (require("brms")) {
#'   brm1 <- brm(Sepal.Length ~ 1, data = iris, save_all_pars = TRUE)
#'   brm2 <- brm(Sepal.Length ~ Species, data = iris, save_all_pars = TRUE)
#'   brm3 <- brm(
#'     Sepal.Length ~ Species + Petal.Length,
#'     data = iris,
#'     save_pars = save_pars(all = TRUE)
#'   )
#'
#'   bayesfactor_models(brm1, brm2, brm3, denominator = 1)
#' }
#'
#'
#' # BayesFactor
#' # ---------------------------
#' if (require("BayesFactor")) {
#'   data(puzzles)
#'   BF <- anovaBF(RT ~ shape * color + ID,
#'     data = puzzles,
#'     whichRandom = "ID", progress = FALSE
#'   )
#'   BF
#'   bayesfactor_models(BF) # basically the same
#' }
#' }
#' @references
#' \itemize{
#'   \item Gronau, Q. F., Singmann, H., & Wagenmakers, E. J. (2017). Bridgesampling: An R package for estimating normalizing constants. arXiv preprint arXiv:1710.08162.
#'   \item Kass, R. E., and Raftery, A. E. (1995). Bayes Factors. Journal of the American Statistical Association, 90(430), 773-795.
#'   \item Robert, C. P. (2016). The expected demise of the Bayes factor. Journal of Mathematical Psychology, 72, 33–37.
#'   \item Wagenmakers, E. J. (2007). A practical solution to the pervasive problems of p values. Psychonomic bulletin & review, 14(5), 779-804.
#'   \item Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#' }
#'
#' @importFrom insight get_response is_model_supported
#' @export
bayesfactor_models <- function(..., denominator = 1, verbose = TRUE) {
  UseMethod("bayesfactor_models")
}

#' @rdname bayesfactor_models
#' @export
bf_models <- bayesfactor_models

#' @export
bayesfactor_models.default <- function(..., denominator = 1, verbose = TRUE) {
  # Organize the models and their names
  mods <- list(...)
  mnames <- sapply(match.call(expand.dots = FALSE)$`...`, .safe_deparse)

  # In the case of a list as direct input
  if (length(mods) == 1 && inherits(mods[[1]], "list")) {
    mods <- mods[[1]]
    mnames <- names(mods)
  }

  if (!is.numeric(denominator)) {
    model_name <- .safe_deparse(match.call()[["denominator"]])
    denominator_model <- which(mnames == model_name)

    if (length(denominator_model) == 0) {
      mods <- c(mods, list(denominator))
      mnames <- c(mnames, model_name)
      denominator <- length(mods)
    } else {
      denominator <- denominator_model
    }
  }

  # Get formula / model names
  mforms <- mnames

  # supported models
  supported_models <- sapply(mods, insight::is_model_supported)
  if (all(supported_models)) {
    temp_forms <- sapply(mods, .find_full_formula)

    has_terms <- sapply(temp_forms, nchar) > 0

    mforms[has_terms] <- temp_forms[has_terms]
    supported_models[!has_terms] <- FALSE
  }

  if (!all(supported_models)) {
    if (verbose) {
      warning(sprintf(
        "Unable to extract terms from the following models: \n%s",
        paste0(mnames[!supported_models], collapse = ", ")
      ), call. = FALSE)
    }
  }

  # Get BF
  names(mods) <- mforms
  mBIC <- .BIC_list(mods)
  mBFs <- exp((mBIC - mBIC[denominator]) / (-2))

  res <- data.frame(
    Model = mforms,
    BF = mBFs,
    stringsAsFactors = FALSE
  )


  .bf_models_output(res,
    denominator = denominator,
    bf_method = "BIC approximation",
    unsupported_models = !all(supported_models),
    model_names = mnames
  )
}


#' @importFrom insight get_response find_algorithm
.bayesfactor_models_stan <- function(..., denominator = 1, verbose = TRUE) {
  if (!requireNamespace("bridgesampling")) {
    stop("Package 'bridgesampling' required for this function to work. Please install it by running `install.packages('bridgesampling')`.")
  }

  # Organize the models
  mods <- list(...)

  # In the case of a list as direct input
  if (length(mods) == 1 && inherits(mods[[1]], "list")) {
    mods <- mods[[1]]
    was_list <- TRUE
  } else {
    was_list <- FALSE
  }


  # Warn
  n_samps <- sapply(mods, function(x) {
    alg <- insight::find_algorithm(x)
    (alg$iterations - alg$warmup) * alg$chains
  })
  if (any(n_samps < 4e4)) {
    warning(
      "Bayes factors might not be precise.\n",
      "For precise Bayes factors, it is recommended sampling at least 40,000 posterior samples.",
      call. = FALSE
    )
  }

  if (!is.numeric(denominator)) {
    model_name <- .safe_deparse(match.call()[["denominator"]])
    if (was_list) {
      arg_names <- names(mods)
    } else {
      arg_names <- sapply(match.call(expand.dots = FALSE)$`...`, .safe_deparse)
    }

    denominator_model <- which(arg_names == model_name)

    if (length(denominator_model) == 0) {
      mods <- c(mods, list(denominator))
      denominator <- length(mods)
    } else {
      denominator <- denominator_model
    }
  }

  # Test that all is good:
  resps <- lapply(mods, insight::get_response)
  from_same_data_as_den <- sapply(resps[-denominator],
    identical,
    y = resps[[denominator]]
  )
  if (!all(from_same_data_as_den)) {
    stop("Models were not computed from the same data.")
  }

  # Get BF
  if (verbose) {
    message("Computation of Bayes factors: estimating marginal likelihood, please wait...")
  }
  mML <- lapply(mods, function(x) {
    bridgesampling::bridge_sampler(x, silent = TRUE)
  })
  mBFs <- sapply(mML, function(x) {
    bf <- bridgesampling::bf(x, mML[[denominator]], log = TRUE)
    bf[["bf"]]
  })

  # Get formula
  mforms <- sapply(mods, .find_full_formula)

  res <- data.frame(
    Model = mforms,
    BF = exp(mBFs),
    stringsAsFactors = FALSE
  )


  .bf_models_output(res,
    denominator = denominator,
    bf_method = "marginal likelihoods (bridgesampling)"
  )
}




#' @export
bayesfactor_models.stanreg <- function(..., denominator = 1, verbose = TRUE) {
  if (!requireNamespace("rstanarm")) {
    stop("Package 'rstanarm' required for this function to work. Please install it by running `install.packages('rstanarm')`.")
  }
  .bayesfactor_models_stan(..., denominator = denominator)
}

#' @export
bayesfactor_models.brmsfit <- function(..., denominator = 1, verbose = TRUE) {
  if (!requireNamespace("brms")) {
    stop("Package 'brms' required for this function to work. Please install it by running `install.packages('brms')`.")
  }
  if (!("brms" %in% .packages())) {
    stop("This function requires package 'brms' to be loaded. Please run `library(brms)`.")
  }
  .bayesfactor_models_stan(..., denominator = denominator)
}


#' @export
bayesfactor_models.BFBayesFactor <- function(..., verbose = TRUE) {
  models <- c(...)
  if (!requireNamespace("BayesFactor")) {
    stop("Package 'BayesFactor' required for this function to work. Please install it by running `install.packages('BayesFactor')`.")
  }
  mBFs <- c(0, BayesFactor::extractBF(models, TRUE, TRUE))
  mforms <- sapply(c(models@denominator, models@numerator), function(x) x@shortName)

  if (!"BFlinearModel" %in% class(models@denominator)) {
    mforms <- .clean_non_linBF_mods(mforms)
  } else {
    mforms[mforms == "Intercept only"] <- "1"
  }

  res <- data.frame(
    Model = unname(mforms),
    BF = exp(mBFs),
    stringsAsFactors = FALSE
  )

  .bf_models_output(res,
    denominator = 1,
    bf_method = "JZS (BayesFactor)",
    unsupported_models = !"BFlinearModel" %in% class(models@denominator)
  )
}


# Methods -----------------------------------------------------------------

#' @rdname bayesfactor_models
#' @export
update.bayesfactor_models <- function(object, subset = NULL, reference = NULL, ...) {
  if (!is.null(reference)) {
    if (reference == "top") {
      reference <- which.max(object$BF)
    } else if (reference == "bottom") {
      reference <- which.min(object$BF)
    }
    object$BF <- object$BF / object$BF[reference]
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


#' @rdname bayesfactor_models
#' @export
as.matrix.bayesfactor_models <- function(x, ...) {
  x$BF <- log(x$BF)
  out <- -outer(x$BF, x$BF, FUN = "-")
  rownames(out) <- colnames(out) <- x$Model

  out <- exp(out)

  class(out) <- c("bayesfactor_models_matrix", class(out))
  out
}

# Helpers -----------------------------------------------------------------
#' @keywords internal
.bf_models_output <- function(res, denominator = 1, bf_method = "method", unsupported_models = FALSE, model_names = NULL) {
  attr(res, "denominator") <- denominator
  attr(res, "BF_method") <- bf_method
  attr(res, "unsupported_models") <- unsupported_models
  attr(res, "model_names") <- model_names
  class(res) <- c("bayesfactor_models", "see_bayesfactor_models", class(res))

  res
}


#' @keywords internal
#' @importFrom insight find_formula
.find_full_formula <- function(mod) {
  formulas <- insight::find_formula(mod)

  conditional <- random <- NULL
  if (!is.null(formulas$conditional)) {
    conditional <- as.character(formulas$conditional)[3]
  }

  if (!is.null(formulas$random)) {
    if (!is.list(formulas$random)) {
      formulas$random <- list(formulas$random)
    }
    random <- sapply(formulas$random, function(x) {
      paste0("(", as.character(x)[2], ")")
    })
  }
  paste(c(conditional, random), collapse = " + ")
}

#' @keywords internal
#' @importFrom stats BIC
.BIC_list <- function(x) {
  sapply(x, function(m) {
    tryCatch(
      {
        bic <- stats::BIC(m, x[[1]])
        bic$BIC[1]
      },
      warning = function(w) {
        stop(conditionMessage(w), call. = FALSE)
      }
    )
  })
}

#' @keywords internal
.clean_non_linBF_mods <- function(m_names) {
  tryCatch(
    {
      m_txt <- character(length = length(m_names))

      ## Detect types ##
      is_null <- grepl("^Null", m_names)
      is_rho <- grepl("rho", m_names)
      is_mu <- grepl("mu", m_names)
      is_d <- grepl("d", m_names)
      is_p <- grepl("p", m_names)
      is_range <- grepl("<", m_names)

      ## Range Alts ##
      m_txt[!is_null & is_range] <-
        sub("^[^\\s]*\\s[^\\s]*\\s", "", m_names[!is_null & is_range])

      ## Null models + Not nulls ##
      if (any(is_d & is_p)) {
        is_null <- !grepl("^Non", m_names)
        temp <- m_names[is_null][1]
        mi <- gregexpr("\\(.*\\)", temp)
        aa <- unlist(regmatches(temp, m = mi))

        m_txt[is_null] <- sub("a=", "a = ", aa)
        m_txt[!is_null & !is_range] <- sub("a=", "a != ", aa)
      } else if (any(is_rho)) {
        m_txt[is_null] <- "rho = 0"
        m_txt[!is_null & !is_range] <- "rho != 0"
        m_txt <- sub("<rho<", " < rho < ", m_txt)
      } else if (any(is_d | is_mu)) {
        m_txt[is_null] <- "d = 0"
        m_txt[!is_null & !is_range] <- "d != 0"
        m_txt <- sub("<d<", " < d < ", m_txt)
      } else if (any(is_p)) {
        temp <- m_names[is_null][1]
        mi <- gregexpr("[0-9|\\.]+", temp)
        pp <- unlist(regmatches(temp, m = mi))

        m_txt[is_null] <- paste0("p = ", pp)
        m_txt[!is_null & !is_range] <- paste0("p != ", pp)
        m_txt <- sub("<p<", " < p < ", m_txt)
      } else {
        stop("!")
      }

      ## wrap with () for readability ##
      is_wrapped <- grepl("\\(", m_txt)
      m_txt[!is_wrapped] <- paste0("(", m_txt[!is_wrapped], ")")

      return(m_txt)
    },
    error = function(e) {
      return(m_names)
    }
  )
}
