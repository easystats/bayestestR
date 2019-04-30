#' Get inclusion BFs for effects across Bayesian models
#'
#'
#' @author Mattan S. Ben-Shachar
#' @param models an object of class \code{BFGrid} or \code{BFBayesFactor}.
#' @param match_models If \code{FALSE} (default), Inclustion BFs are computed by
#' comparing all models with an effect against all models without the effect. If \code{TRUE},
#' Inclusion BFs are computed by comparing all models with an effect against models without
#' the effect AND without any higher-order interactions with the effect.
#' @param prior_odds optional vector of prior odds for the models. See \code{BayesFactor::priorOdds}
#'
#' @return a data frame containing the prior and posterior probabilities, and BF (log) for each effect.
#'
#' @note Random effects in the \code{lme} style will be displayed as interactions:
#' i.e., \code{(X|G)} will become \code{1:G} and \code{X:G}.
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#' library(BayesFactor)
#'
#' BF <- generalTestBF(len ~ supp * dose, ToothGrowth, progress = FALSE)
#'
#' bayesfactor_inclusion(BF)
#'
#' # compare only matched models:
#' bayesfactor_inclusion(BF, match_models = TRUE)
#' }
#'
#' @references Hinne, M., Gronau, Q. F., van den Bergh, D., & Wagenmakers,
#' E. (2019, March 25). A conceptual introduction to Bayesian Model
#' Averaging. https://doi.org/10.31234/osf.io/wgb64
#'
#'#' Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian
#' adaptive sampling for variable selection and model averaging. Journal
#' of Computational and Graphical Statistics, 20(1), 80-101.
#'
#' Mathot. S. (2017). Bayes like a Baws:
#' Interpreting Bayesian Repeated Measures in JASP [Blog post].
#' Retrieved from https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp
#'
#'
#' @export
bayesfactor_inclusion <- function(models, match_models = FALSE, prior_odds = NULL) {
  UseMethod("bayesfactor_inclusion")
}

bayesfactor_inclusion.BFGrid <- function(models, match_models = FALSE, prior_odds = NULL) {
  # Build Models Table #
  df.model <- get_model_table(models, priorOdds = prior_odds)
  effnames <- colnames(df.model)[-(1:3)]

  # Build Interaction Matrix #
  if (match_models) {
    df.interaction <- data.frame(effnames)

    for (eff in effnames) {
      df.interaction[,eff] <- sapply(effnames, function(x) includes_interaction(x,eff))
    }
    rownames(df.interaction) <- effnames
    df.interaction <- df.interaction[,-1]
  }

  # Build Effect Table #
  df.effect <- data.frame(effnames,
                          Pinc  = rep(NA,length(effnames)),
                          PincD = rep(NA,length(effnames)),
                          BFinc = rep(NA,length(effnames)))

  for (eff in effnames) {
    df.model_temp <- df.model

    if (match_models) {
      # remove models with higher interactions
      inter_term <- effnames[unlist(df.interaction[effnames==eff,,drop = TRUE])]

      hashigherinter <- which(rowSums(df.model[,inter_term, drop = FALSE])>0)

      if (length(hashigherinter)>0) {
        df.model_temp <- df.model_temp[-hashigherinter,,drop = FALSE]
      }
    }

    # models with effect
    mwith <- which(df.model_temp[[eff]])
    mwithprior <- sum(df.model_temp[mwith,'priorProbs'])
    mwithpost <- sum(df.model_temp[mwith,'postProbs'])

    # models without effect
    mwithoutprior <- sum(df.model_temp[-mwith,'priorProbs'])
    mwithoutpost <- sum(df.model_temp[-mwith,'postProbs'])

    # Save results
    df.effect$Pinc[effnames==eff]  <- mwithprior
    df.effect$PincD[effnames==eff] <- mwithpost
    df.effect$BFinc[effnames==eff] <- (mwithpost/mwithoutpost)/(mwithprior/mwithoutprior)
  }

  df.effect$BFinc <- log(df.effect$BFinc)
  df.effect <- df.effect[,-1,drop = FALSE]
  colnames(df.effect) <- c("P.Inc.prior","P.Inc.posterior","log.BF.Inc")
  rownames(df.effect) <- effnames


  class(df.effect) <- c('BFinc',class(df.effect))
  attr(df.effect,'matched') <- match_models
  attr(df.effect,'priorOdds') <- prior_odds

  return(df.effect)
}

bayesfactor_inclusion.BFBayesFactor <- function(models, match_models = FALSE, prior_odds = NULL) {
  models <- bayesfactor_models.BFBayesFactor(models)
  bayesfactor_inclusion.BFGrid(models, match_models = match_models, prior_odds = prior_odds)
}

#' @importFrom stats as.formula terms setNames
get_model_table <- function(BFGrid, priorOdds = NULL){
  denominator <- attr(BFGrid,'denominator')
  BFGrid <- rbind(BFGrid[denominator,],BFGrid[-denominator,])
  attr(BFGrid,'denominator') <- 1

  # Prior and post odds
  Modelnames <- BFGrid$Model
  if (!is.null(priorOdds)) {
    priorOdds <- c(1, priorOdds)
  } else {
    priorOdds <- rep(1, length(Modelnames))
  }

  posterior_odds <- priorOdds * exp(BFGrid$log.BF)

  priorProbs <- priorOdds / sum(priorOdds)
  postProbs <- posterior_odds / sum(posterior_odds)

  df.model <- data.frame(Modelnames,
                         priorProbs,
                         postProbs,
                         stringsAsFactors = FALSE)

  # add effects table
  make_terms <- function(formula) {
    formula.f <- stats::as.formula(paste0('~', formula))
    all.terms <- attr(stats::terms(formula.f), "term.labels")

    fix_trms <- all.terms[!grepl("\\|", all.terms)] # no random

    random_parts <- paste0(all.terms[grepl("\\|", all.terms)]) # only random
    if (length(random_parts) == 0) {
      return(stats::setNames(fix_trms, fix_trms))
    }

    random_units <- sub("^.+\\|\\s+", "", random_parts)
    tmp_random <- lapply(
      sub("\\|.+$", "", random_parts),
      function(x) stats::as.formula(paste0("~", x))
    )

    rand_print <- rand_trms <- vector("list", length(random_parts))

    for (i in seq_along(random_parts)) {
      tmp_trms <- attr(terms.formula(tmp_random[[i]]), "term.labels")

      if (!any(unlist(strsplit(as.character(tmp_random[[i]])[[2]], ' \\+ ')) == "0"))
        tmp_trms <- c("1", tmp_trms)

      rand_trms[[i]] <- paste0(tmp_trms, ':', random_units[[i]])
      rand_print[[i]] <- paste0("(", tmp_trms, "|", random_units[[i]], ")")
    }

    stats::setNames(
      c(fix_trms,unlist(rand_trms)),
      c(fix_trms,unlist(rand_print))
    )
  }

  print_terms <- character()
  for (m in seq_len(nrow(df.model))) {
    tmp_terms <- make_terms(df.model$Modelnames[m])
    # Somehow save randomeffs for printing?
    df.model[m, tmp_terms] <- TRUE
  }

  df.model[is.na(df.model)] <- FALSE

  df.model
}


includes_interaction <- function(eff,effnames){
  eff_b <- strsplit(eff,"\\:")
  effnames_b <- strsplit(effnames,'\\:')

  is_int <- sapply(effnames_b,function(x) length(x)>1)

  temp <- logical(length(effnames))
  for (rr in seq_along(effnames)) {
    if (is_int[rr]) {
      temp[rr] <- all(eff_b[[1]] %in% effnames_b[[rr]]) &
        !all(effnames_b[[rr]] %in% eff_b[[1]])
    }
  }
  return(temp)
}
