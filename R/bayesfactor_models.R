#' Extract Bayes Factors from fitted models
#'
#' @description This function computes Bayes factors from fitted models or extracts
#'   the Bayes factor from objects of class \code{BFBayesFactor}.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @param ... Fitted models (any models supported by \CRANpkg{insight}), all fit on the same data, or a single \code{BFBayesFactor} object (see 'Details').
#' @param denominator Either an integer indicating which of the models to use as the denominator,
#' or a model to use as a denominator. Ignored for \code{BFBayesFactor}.
#'
#' @details
#' \itemize{
#'   \item For \code{brmsfit} or \code{stanreg} models, Bayes factors are computed using the \code{bridgesampling} package.
#'   \itemize{
#'     \item \code{brmsfit} models must have been fitted with \code{save_all_pars = TRUE}.
#'     \item \code{stanreg} models must have been fitted with a defined \code{diagnostic_file}.
#'   }
#'   \item For \code{BFBayesFactor}, \code{bayesfactor_models} is mostly a wraparoud \code{BayesFactor::extractBF}.
#'   \item For all other model types (supported by \CRANpkg{insight}), BIC approximations are used to compute Bayes factors.
#' }
#' In order to correctly and precisely estimate Bayes Factors, a rule of thumb are
#' the 4 P's: \strong{P}roper \strong{P}riors and \strong{P}lentiful \strong{P}osterior
#' (i.e. probably at leat 40,000 samples instead of the default of 4,000).
#'
#' @return A data frame containing the models' formulas (reconstructed fixed and random effects) and their BFs (log) of the supplied models, that prints nicely.
#'
#' @examples
#' # With lm objects:
#' lm1 <- lm(Sepal.Length ~ 1, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm4 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' bayesfactor_models(lm1, lm2, lm3, lm4, denominator = 1)
#' bayesfactor_models(lm2, lm3, lm4, denominator = lm1) # same result
#' bayesfactor_models(lm1, lm2, lm3, lm4, denominator = lm1) # same result
#'
#' # With lmerMod objects:
#' library(lme4)
#' lmer1 <- lmer(Sepal.Length ~ Petal.Length + (1|Species), data = iris)
#' lmer2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length|Species), data = iris)
#' lmer3 <- lmer(
#'   Sepal.Length ~ Petal.Length + (Petal.Length|Species) + (1|Petal.Width),
#'   data = iris
#' )
#' bayesfactor_models(lmer1, lmer2, lmer3, denominator = 1)
#' bayesfactor_models(lmer1, lmer2, lmer3, denominator = lmer1)
#'
#' \dontrun{
#' # With BFBayesFactor objects:
#' library(BayesFactor)
#' data(puzzles)
#' BF <- anovaBF(RT ~ shape * color + ID, data = puzzles,
#'               whichRandom = "ID", progress = FALSE)
#' BF
#' bayesfactor_models(BF) # basically the same
#'
#' # with brmfit objects:
#' # (note the save_all_pars MUST be set to TRUE in order to work)
#' library(brms)
#' brm1 <- brm(Sepal.Length ~ 1, data = iris, save_all_pars = TRUE)
#' brm2 <- brm(Sepal.Length ~ Species, data = iris, save_all_pars = TRUE)
#' brm3 <- brm(
#'   Sepal.Length ~ Species + Petal.Length, data = iris,
#'   save_all_pars = TRUE
#' )
#'
#' bayesfactor_models(brm1, brm2, brm3, denominator = 1)
#'}
#'
#' @references
#' \itemize{
#'   \item Kass, R. E., & Raftery, A. E. (1995). Bayes Factors. Journal of the American Statistical Association, 90(430), 773-795.
#'   \item Robert, C. P. (2016). The expected demise of the Bayes factor. Journal of Mathematical Psychology, 72, 33–37.
#'   \item Wagenmakers, E. J. (2007). A practical solution to the pervasive problems of p values. Psychonomic bulletin & review, 14(5), 779-804.
#'   \item Gronau, Q. F., Wagenmakers, E. J., Heck, D. W., & Matzke, D. (2019). A simple method for comparing complex models: Bayesian model comparison for hierarchical multinomial processing tree models using Warp-III bridge sampling. Psychometrika, 84(1), 261-284.
#' }
#'
#' @seealso update.BFGrid
#'
#' @importFrom insight get_response
#' @export
bayesfactor_models <- function(..., denominator = 1) {
  UseMethod("bayesfactor_models")
}

#' @importFrom stats BIC
#' @export
bayesfactor_models.default <- function(..., denominator = 1){
  # Organize the models
  mods <- list(...)

  if (!is.numeric(denominator)) {
    model_name <- deparse(match.call()[["denominator"]])
    arg_names <- sapply(match.call(expand.dots = F)$`...`, deparse)
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
  if (!all(sapply(resps[-denominator], function(x) identical(x, resps[[denominator]])))) {
    stop("Models were not computed from the same data.")
  }

  # Get BF
  mBIC <- sapply(mods, BIC)
  mBFs <- (mBIC - mBIC[denominator]) / (-2)

  # Get formula
  mforms <- sapply(mods, .find_full_formula)

  res <- data.frame(Model  = mforms,
                    log.BF = mBFs,
                    stringsAsFactors = FALSE)

  attr(res, 'denominator') <- denominator
  attr(res, 'BF_method') <- 'BIC approximation'
  class(res) <- c('bayesfactor_models', class(res))

  res
}


#' @importFrom insight get_response find_algorithm
.bayesfactor_models_stan <- function(..., denominator = 1){
  if (!requireNamespace("bridgesampling")) {
    stop("Package \"bridgesampling\" needed for this function to work. Please install it.")
  }

  if (!requireNamespace("brms")) {
    stop("Package \"brms\" needed for this function to work. Please install it.")
  }

  # Orgenize the models
  mods <- list(...)

  # Warn
  n_samps <- sapply(mods, function(x) {
    alg <- insight::find_algorithm(x)
    (alg$chains - alg$warmup) * alg$iterations
  })
  if (any(n_samps < 4e4)) {
    warning("Bayes factors might not be precise.\n",
            "For precise Bayes factors, it is recommended sampling at least 40,000 posterior samples.")
  }

  if (!is.numeric(denominator)) {
    model_name <- deparse(match.call()[["denominator"]])
    arg_names <- sapply(match.call(expand.dots = F)$`...`, deparse)
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
  if (!all(sapply(resps[-denominator], function(x) identical(x, resps[[denominator]])))) {
    stop("Models were not computed from the same data.")
  }

  # Get BF
  mML <- lapply(mods, function(x)
    bridgesampling::bridge_sampler(x, silent = TRUE))
  mBFs <- sapply(mML, function(x)
    bridgesampling::bf(x, mML[[denominator]], log = TRUE)[['bf']])

  # Get formula
  mforms <- sapply(mods, .find_full_formula)

  res <- data.frame(Model  = mforms,
                    log.BF = mBFs,
                    stringsAsFactors = FALSE)

  attr(res,'denominator') <- denominator
  attr(res,'BF_method') <- 'marginal likelihoods (bridgesampling)'
  class(res) <- c('bayesfactor_models', class(res))

  res
}

#' @export
bayesfactor_models.stanreg <- function(..., denominator = 1){
  if (!requireNamespace("rstanarm")) {
    stop("Package \"rstanarm\" needed for this function to work. Please install it.")
  }
  .bayesfactor_models_stan(..., denominator = denominator)
}

#' @export
bayesfactor_models.brmsfit <- function(..., denominator = 1){
  if (!require("brms")) {
    stop("Package \"brms\" needed for this function to work. Please install it.")
  }
  .bayesfactor_models_stan(..., denominator = denominator)
}


#' @export
bayesfactor_models.BFBayesFactor <- function(...) {
  models <- c(...)
  if (!requireNamespace("BayesFactor")) {
    stop("Package \"BayesFactor\" needed for this function to work. Please install it.")
  }
  mBFs <- c(0, BayesFactor::extractBF(models, TRUE, TRUE))
  mforms <- sapply(c(models@denominator, models@numerator), function(x) x@shortName)
  mforms[mforms == "Intercept only"] <- "1"

  res <- data.frame(Model  = unname(mforms),
                    log.BF = mBFs,
                    stringsAsFactors = FALSE)

  attr(res,'denominator') <- 1
  attr(res,'BF_method') <- 'JZS (BayesFactor)'
  class(res) <- c('bayesfactor_models', class(res))

  res
}

#' Update bayesfactor_models
#'
#'
#' @param object A \link{bayesfactor_models} object.
#' @param subset Vector of model indices to keep or remove.
#' @param reference Index of model to rereference to, or \code{"top"} to reference to the best model, or \code{"bottom"} to reference to the worst model.
#' @param ... Currently not used.
#' @export
update.bayesfactor_models <- function(object, subset = NULL, reference = NULL, ...){
  if (!is.null(reference)) {
    if (reference == "top") {
      reference <- which.max(object$log.BF)
    } else if (reference == "bottom") {
      reference <- which.min(object$log.BF)
    }
    object$log.BF <- object$log.BF - object$log.BF[reference]
    attr(object,"denominator") <- reference
  }

  denominator <- attr(object,"denominator")

  if (!is.null(subset)) {
    object_subset <- object[subset,]

    if (denominator %in% subset) {
      attr(object_subset,"denominator") <- which(denominator == subset)
    } else {
      object_subset <- rbind(object[denominator,],object_subset)
      attr(object_subset,"denominator") <- 1
    }
    object <- object_subset
  }
  object
}

#' @keywords internal
#' @importFrom insight find_formula
.find_full_formula <- function(mod){
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
