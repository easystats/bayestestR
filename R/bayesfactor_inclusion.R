#' Inclusion Bayes Factors for testing predictors across Bayesian models
#'
#'
#' @author Mattan S. Ben-Shachar
#' @param models An object of class \code{\link{bayesfactor_models}} or \code{BFBayesFactor}.
#' @param match_models See details.
#' @param prior_odds Optional vector of prior odds for the models. See \code{BayesFactor::priorOdds<-}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return a data frame containing the prior and posterior probabilities, and BF for each effect.
#'
#' @details Inclusion Bayes factors answer the question: Are the observed data more
#' probable under models with a particular effect, than they are under models without
#' that particular effect? In other words, on average - are models with effect \eqn{X}
#' more likely to have produced the observed data than models without effect \eqn{X}?
#' \cr \cr
#' For more info, see \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.
#'
#' \subsection{Match Models}{
#' If \code{match_models=FALSE} (default), Inclusion BFs are computed by comparing all models
#' with a predictor against all models without that predictor. If \code{TRUE},
#' comparison is restricted to models that (1) do not include any interactions
#' with the predictor of interest; (2) for interaction predictors, averaging is done
#' only across models that containe the main effect from which the interaction
#' predictor is comprised.
#' }
#'
#' @note Random effects in the \code{lme} style will be displayed as interactions:
#' i.e., \code{(X|G)} will become \code{1:G} and \code{X:G}.
#'
#' @examples
#' library(bayestestR)
#'
#' # Using bayesfactor_models:
#' # ------------------------------
#' mo0 <- lm(Sepal.Length ~ 1, data = iris)
#' mo1 <- lm(Sepal.Length ~ Species, data = iris)
#' mo2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' mo3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#'
#' BFmodels <- bayesfactor_models(mo1, mo2, mo3, denominator = mo0)
#' bayesfactor_inclusion(BFmodels)
#' \dontrun{
#' # BayesFactor
#' # -------------------------------
#' library(BayesFactor)
#'
#' BF <- generalTestBF(len ~ supp * dose, ToothGrowth, progress = FALSE)
#'
#' bayesfactor_inclusion(BF)
#'
#' # compare only matched models:
#' bayesfactor_inclusion(BF, match_models = TRUE)
#' }
#' @references
#' \itemize{
#'   \item Hinne, M., Gronau, Q. F., van den Bergh, D., and Wagenmakers, E. (2019, March 25). A conceptual introduction to Bayesian Model Averaging. \doi{10.31234/osf.io/wgb64}
#'   \item Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20(1), 80-101.
#'   \item Mathot. S. (2017). Bayes like a Baws: Interpreting Bayesian Repeated Measures in JASP [Blog post]. Retrieved from https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp
#' }
#'
#'
#' @export
bayesfactor_inclusion <- function(models, match_models = FALSE, prior_odds = NULL, ...) {
  UseMethod("bayesfactor_inclusion")
}


#' @export
bayesfactor_inclusion.bayesfactor_models <- function(models, match_models = FALSE, prior_odds = NULL, ...) {
  if (utils::packageVersion("base") < "3.6.0") {
    stop("'bayesfactor_inclusion' requires R 3.6.0 or later to work.\nYou might find the 'installr' package useful.")
  }


  # Build Models Table #
  df.model <- .get_model_table(models, priorOdds = prior_odds)
  effnames <- colnames(df.model)[-(1:3)]

  # Build Interaction Matrix #
  if (isTRUE(match_models)) {
    effects.matrix <- as.matrix(df.model[, -c(1:3)])

    df.interaction <- data.frame(effnames, stringsAsFactors = FALSE)

    for (eff in effnames) {
      df.interaction[, eff] <- sapply(effnames, function(x) .includes_interaction(x, eff))
    }
    rownames(df.interaction) <- effnames
    df.interaction <- as.matrix(df.interaction[, -1])
  }

  # Build Effect Table #
  df.effect <- data.frame(
    effnames,
    Pinc = rep(NA, length(effnames)),
    PincD = rep(NA, length(effnames)),
    BF_inclusion = rep(NA, length(effnames)),
    stringsAsFactors = FALSE
  )

  for (eff in effnames) {
    if (isTRUE(match_models)) {
      idx1 <- df.interaction[eff, ]
      idx2 <- df.interaction[, eff]

      has_not_high_order_interactions <- !apply(effects.matrix[, idx1, drop = FALSE], 1, any)

      ind_include <- has_not_high_order_interactions & effects.matrix[, eff]

      ind_exclude <- apply(effects.matrix[, idx2, drop = FALSE], 1, all) &
        has_not_high_order_interactions &
        !effects.matrix[, eff]

      df.model_temp <- df.model[ind_include | ind_exclude, , drop = FALSE]
    } else {
      df.model_temp <- df.model
    }

    # models with effect
    mwith <- which(df.model_temp[[eff]])
    mwithprior <- sum(df.model_temp[mwith, "priorProbs"])
    mwithpost <- sum(df.model_temp[mwith, "postProbs"])

    # models without effect
    mwithoutprior <- sum(df.model_temp[-mwith, "priorProbs"])
    mwithoutpost <- sum(df.model_temp[-mwith, "postProbs"])

    # Save results
    df.effect$Pinc[effnames == eff] <- mwithprior
    df.effect$PincD[effnames == eff] <- mwithpost
    df.effect$BF_inclusion[effnames == eff] <- (mwithpost / mwithoutpost) / (mwithprior / mwithoutprior)
  }

  df.effect$BF_inclusion <- df.effect$BF_inclusion
  df.effect <- df.effect[, -1, drop = FALSE]
  colnames(df.effect) <- c("p_prior", "p_posterior", "BF")
  rownames(df.effect) <- effnames


  class(df.effect) <- c("bayesfactor_inclusion", class(df.effect))
  attr(df.effect, "matched") <- match_models
  attr(df.effect, "priorOdds") <- prior_odds

  return(df.effect)
}


#' @export
bayesfactor_inclusion.BFBayesFactor <- function(models, match_models = FALSE, prior_odds = NULL, ...) {
  models <- bayesfactor_models.BFBayesFactor(models)
  bayesfactor_inclusion.bayesfactor_models(models, match_models = match_models, prior_odds = prior_odds)
}


#' @keywords internal
#' @importFrom stats as.formula terms terms.formula
.get_model_table <- function(BFGrid, priorOdds = NULL, ...) {
  denominator <- attr(BFGrid, "denominator")
  BFGrid <- rbind(BFGrid[denominator, ], BFGrid[-denominator, ])
  attr(BFGrid, "denominator") <- 1

  # Prior and post odds
  Modelnames <- BFGrid$Model
  if (!is.null(priorOdds)) {
    priorOdds <- c(1, priorOdds)
  } else {
    priorOdds <- rep(1, length(Modelnames))
  }

  posterior_odds <- priorOdds * BFGrid$BF

  priorProbs <- priorOdds / sum(priorOdds)
  postProbs <- posterior_odds / sum(posterior_odds)

  df.model <- data.frame(
    Modelnames,
    priorProbs,
    postProbs,
    stringsAsFactors = FALSE
  )

  # add effects table
  make_terms <- function(formula) {
    formula.f <- stats::as.formula(paste0("~", formula))
    all.terms <- attr(stats::terms(formula.f), "term.labels")

    fix_trms <- all.terms[!grepl("\\|", all.terms)] # no random

    random_parts <- paste0(all.terms[grepl("\\|", all.terms)]) # only random
    if (length(random_parts) == 0) {
      return(fix_trms)
    }

    random_units <- sub("^.+\\|\\s+", "", random_parts)
    tmp_random <- lapply(
      sub("\\|.+$", "", random_parts),
      function(x) stats::as.formula(paste0("~", x))
    )

    rand_trms <- vector("list", length(random_parts))

    for (i in seq_along(random_parts)) {
      tmp_trms <- attr(stats::terms.formula(tmp_random[[i]]), "term.labels")

      if (!any(unlist(strsplit(as.character(tmp_random[[i]])[[2]], " \\+ ")) == "0")) {
        tmp_trms <- c("1", tmp_trms)
      }

      rand_trms[[i]] <- paste0(tmp_trms, ":", random_units[[i]])
    }

    c(fix_trms, unlist(rand_trms))
  }

  for (m in seq_len(nrow(df.model))) {
    tmp_terms <- make_terms(df.model$Modelnames[m])
    if (length(tmp_terms) > 0) {
      df.model[m, tmp_terms] <- TRUE
    }
  }

  df.model[is.na(df.model)] <- FALSE

  df.model
}


#' @keywords internal
.includes_interaction <- function(eff, effnames) {
  eff_b <- strsplit(eff, "\\:")
  effnames_b <- strsplit(effnames, "\\:")

  is_int <- sapply(effnames_b, function(x) length(x) > 1)

  temp <- logical(length(effnames))

  for (rr in seq_along(effnames)) {
    if (is_int[rr]) {
      temp[rr] <- all(eff_b[[1]] %in% effnames_b[[rr]]) &
        !all(effnames_b[[rr]] %in% eff_b[[1]])
    }
  }

  temp
}
