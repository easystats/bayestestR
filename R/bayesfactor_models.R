#' Bayes Factors (BF) for model comparison
#'
#' @description This function computes or extracts Bayes factors from fitted models.
#'
#' @author Mattan S. Ben-Shachar
#'
#' @param ... Fitted models (any models supported by \pkg{insight}), all fit on the same data, or a single \code{BFBayesFactor} object (see 'Details').
#' @param denominator Either an integer indicating which of the models to use as the denominator,
#' or a model to use as a denominator. Ignored for \code{BFBayesFactor}.
#' @inheritParams hdi
#'
#' @details
#' \itemize{
#'   \item For \code{brmsfit} or \code{stanreg} models, Bayes factors are computed using the \CRANpkg{bridgesampling} package.
#'   \itemize{
#'     \item \code{brmsfit} models must have been fitted with \code{save_all_pars = TRUE}.
#'     \item \code{stanreg} models must have been fitted with a defined \code{diagnostic_file}.
#'   }
#'   \item For \code{BFBayesFactor}, \code{bayesfactor_models()} is mostly a wraparoud \code{BayesFactor::extractBF()}.
#'   \item For all other model types (supported by \CRANpkg{insight}), BIC approximations are used to compute Bayes factors.
#' }
#' In order to correctly and precisely estimate Bayes factors, a rule of thumb are
#' the 4 P's: \strong{P}roper \strong{P}riors and \strong{P}lentiful \strong{P}osterior
#' (i.e. probably at leat 40,000 samples instead of the default of 4,000).
#' \cr \cr
#' A Bayes factor greater than 1 can be interpereted as evidence against the compared-to
#' model (the denominator). One convention is that a Bayes factor greater than 3 can be considered
#' as "substantial" evidence against the denominator model (and vice versa, a Bayes factor
#' smaller than 1/3 indicates substantial evidence in favor of the denominator model)
#' (\cite{Wetzels et al. 2011}).
#' \cr \cr
#' See also \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.
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
#' bayesfactor_models(lm1, lm2, lm3, lm4, denominator = lm1) # same result
#'
#' # With lmerMod objects:
#' # ---------------------
#' library(lme4)
#' lmer1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' lmer2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
#' lmer3 <- lmer(
#'   Sepal.Length ~ Petal.Length + (Petal.Length | Species) + (1 | Petal.Width),
#'   data = iris
#' )
#' bayesfactor_models(lmer1, lmer2, lmer3, denominator = 1)
#' bayesfactor_models(lmer1, lmer2, lmer3, denominator = lmer1)
#' \dontrun{
#' # rstanarm models
#' # ---------------------
#' # (note that a unique diagnostic_file MUST be specified in order to work)
#' library(rstanarm)
#' stan_m0 <- stan_glm(Sepal.Length ~ 1,
#'   data = iris,
#'   family = gaussian(),
#'   diagnostic_file = file.path(tempdir(), "df0.csv")
#' )
#' stan_m1 <- stan_glm(Sepal.Length ~ Species,
#'   data = iris,
#'   family = gaussian(),
#'   diagnostic_file = file.path(tempdir(), "df1.csv")
#' )
#' stan_m2 <- stan_glm(Sepal.Length ~ Species + Petal.Length,
#'   data = iris,
#'   family = gaussian(),
#'   diagnostic_file = file.path(tempdir(), "df2.csv")
#' )
#' bayesfactor_models(stan_m1, stan_m2, denominator = stan_m0)
#'
#'
#' # brms models
#' # --------------------
#' # (note the save_all_pars MUST be set to TRUE in order to work)
#' library(brms)
#' brm1 <- brm(Sepal.Length ~ 1, data = iris, save_all_pars = TRUE)
#' brm2 <- brm(Sepal.Length ~ Species, data = iris, save_all_pars = TRUE)
#' brm3 <- brm(
#'   Sepal.Length ~ Species + Petal.Length,
#'   data = iris,
#'   save_all_pars = TRUE
#' )
#'
#' bayesfactor_models(brm1, brm2, brm3, denominator = 1)
#'
#'
#' # BayesFactor
#' # ---------------------------
#' library(BayesFactor)
#' data(puzzles)
#' BF <- anovaBF(RT ~ shape * color + ID,
#'   data = puzzles,
#'   whichRandom = "ID", progress = FALSE
#' )
#' BF
#' bayesfactor_models(BF) # basically the same
#' }
#'
#' @references
#' \itemize{
#'   \item Gronau, Q. F., Wagenmakers, E. J., Heck, D. W., and Matzke, D. (2019). A simple method for comparing complex models: Bayesian model comparison for hierarchical multinomial processing tree models using Warp-III bridge sampling. Psychometrika, 84(1), 261-284.
#'   \item Kass, R. E., and Raftery, A. E. (1995). Bayes Factors. Journal of the American Statistical Association, 90(430), 773-795.
#'   \item Robert, C. P. (2016). The expected demise of the Bayes factor. Journal of Mathematical Psychology, 72, 33–37.
#'   \item Wagenmakers, E. J. (2007). A practical solution to the pervasive problems of p values. Psychonomic bulletin & review, 14(5), 779-804.
#'   \item Wetzels, R., Matzke, D., Lee, M. D., Rouder, J. N., Iverson, G. J., and Wagenmakers, E.-J. (2011). Statistical Evidence in Experimental Psychology: An Empirical Comparison Using 855 t Tests. Perspectives on Psychological Science, 6(3), 291–298. \doi{10.1177/1745691611406923}
#' }
#'
#' @importFrom insight get_response is_model
#' @export
bayesfactor_models <- function(..., denominator = 1, verbose = TRUE) {
  UseMethod("bayesfactor_models")
}

#' @importFrom stats BIC
#' @export
bayesfactor_models.default <- function(..., denominator = 1, verbose = TRUE) {
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

  # supported models
  supported_models <- sapply(mods, insight::is_model)
  if (!all(supported_models)) {
    object_names <- match.call(expand.dots = FALSE)$`...`
    stop(sprintf("Can't calculate Bayes factor.\nFollowing objects are no (supported) model objects: %s", paste0(object_names[!supported_models], collapse = ", ")), call. = FALSE)
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

  res <- data.frame(
    Model = mforms,
    BF = exp(mBFs),
    stringsAsFactors = FALSE
  )

  attr(res, "denominator") <- denominator
  attr(res, "BF_method") <- "BIC approximation"
  class(res) <- c("bayesfactor_models", "see_bayesfactor_models", class(res))

  res
}


#' @importFrom insight get_response find_algorithm
.bayesfactor_models_stan <- function(..., denominator = 1, verbose = TRUE) {
  if (!requireNamespace("bridgesampling")) {
    stop("Package \"bridgesampling\" needed for this function to work. Please install it.")
  }

  # Orgenize the models
  mods <- list(...)

  # Warn
  n_samps <- sapply(mods, function(x) {
    alg <- insight::find_algorithm(x)
    (alg$iterations - alg$warmup) * alg$chains
  })
  if (any(n_samps < 4e4)) {
    warning(
      "Bayes factors might not be precise.\n",
      "For precise Bayes factors, it is recommended sampling at least 40,000 posterior samples."
    )
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
  if (verbose) {
    message("Computation of Bayes factors: estimating marginal likelihood, please wait...")
  }
  mML <- lapply(mods, function(x)
    bridgesampling::bridge_sampler(x, silent = TRUE))
  mBFs <- sapply(mML, function(x)
    bridgesampling::bf(x, mML[[denominator]], log = TRUE)[["bf"]])

  # Get formula
  mforms <- sapply(mods, .find_full_formula)

  res <- data.frame(
    Model = mforms,
    BF = exp(mBFs),
    stringsAsFactors = FALSE
  )

  attr(res, "denominator") <- denominator
  attr(res, "BF_method") <- "marginal likelihoods (bridgesampling)"
  class(res) <- c("bayesfactor_models", "see_bayesfactor_models", class(res))

  res
}

#' @export
bayesfactor_models.stanreg <- function(..., denominator = 1, verbose = TRUE) {
  if (!requireNamespace("rstanarm")) {
    stop("Package \"rstanarm\" needed for this function to work. Please install it.")
  }
  .bayesfactor_models_stan(..., denominator = denominator)
}

#' @export
bayesfactor_models.brmsfit <- function(..., denominator = 1, verbose = TRUE) {
  if (!requireNamespace("brms")) {
    stop("Package \"brms\" needed for this function to work. Please install it.")
  }
  if (!("brms" %in% .packages())) {
    stop("This function requires package \"brms\" to be loaded. Please run \"library(brms)\".")
  }
  .bayesfactor_models_stan(..., denominator = denominator)
}


#' @export
bayesfactor_models.BFBayesFactor <- function(..., verbose = TRUE) {
  models <- c(...)
  if (!requireNamespace("BayesFactor")) {
    stop("Package \"BayesFactor\" needed for this function to work. Please install it.")
  }
  mBFs <- c(0, BayesFactor::extractBF(models, TRUE, TRUE))
  mforms <- sapply(c(models@denominator, models@numerator), function(x) x@shortName)
  mforms[mforms == "Intercept only"] <- "1"

  res <- data.frame(
    Model = unname(mforms),
    BF = exp(mBFs),
    stringsAsFactors = FALSE
  )

  attr(res, "denominator") <- 1
  attr(res, "BF_method") <- "JZS (BayesFactor)"
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
