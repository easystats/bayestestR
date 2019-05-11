#' Describe Posterior Distributions
#'
#' Compute indices relevant to describe and characterise the posterior distributions.
#'
#' @param posteriors A vector, dataframe or model of posterior draws.
#' @param ci Credible Interval (CI) level. Default to 0.90 (90\%).
#' @param ci_method The type of index used for Credible Interval. Can be \link{hdi} (default) or "quantile" (see \link{ci}).
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean" or "MAP".
#' @param test The \href{https://easystats.github.io/bayestestR/articles/indicesEstimationComparison.html}{indices of effect existence} to compute. Can be a character or a list with "p_direction", "rope", "p_map" or "bayesfactor".
#' @param rope_range \href{https://easystats.github.io/bayestestR/#rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
#' @param dispersion Indices of dispersion related to the estimate(s) (\code{SD} and \code{MAD} for \code{mean} and \code{median}, respectively).
#' @param ... Additional arguments to be passed to or from methods.
#'
#'
#' @examples
#' describe_posterior(rnorm(1000))
#' @importFrom stats mad median sd setNames
#'
#' @export
describe_posterior <- function(posteriors, estimate = "median", ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, dispersion = TRUE, ...) {
  UseMethod("describe_posterior")
}






#' @rdname describe_posterior
#' @export
describe_posterior.numeric <- function(posteriors, estimate = "median", ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, dispersion = TRUE, ...) {
  x <-
    describe_posterior(
      as.data.frame(posteriors),
      ci = ci,
      ci_method = ci_method,
      estimate = estimate,
      test = test,
      rope_range = rope_range,
      rope_full = rope_full,
      dispersion = dispersion
    )
  x[names(x) != "Parameter"]
}


#' @rdname describe_posterior
#' @export
describe_posterior.double <- describe_posterior.numeric





#' @rdname describe_posterior
#' @method describe_posterior data.frame
#' @export
describe_posterior.data.frame <- function(posteriors, estimate = "median", ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, dispersion = TRUE, ...) {
  # Point estimates
  out <- data.frame("Parameter" = colnames(posteriors))

  if(!is.null(estimate)){

    estimate_list <- tolower(c(estimate))
    if ("all" %in% estimate_list){
      estimate_list <- c("median", "mean", "map")
    }

    if ("median" %in% estimate_list) {
      out$Median <- sapply(posteriors, stats::median)
      if (dispersion) {
        out$MAD <- sapply(posteriors, stats::mad)
      }
    }
    if ("mean" %in% estimate_list) {
      out$Mean <- sapply(posteriors, mean)
      if (dispersion) {
        out$SD <- sapply(posteriors, stats::sd)
      }
    }
    if ("map" %in% estimate_list) {
      out$MAP <- unlist(sapply(posteriors, map_estimate, ...))
    }

  }



  # CI
  if (!is.null(ci)) {
    if (length(ci) > 1) {
      if (ci_method == "hdi") {
        hdi <- sapply(posteriors, hdi, ci = ci, simplify = FALSE)
      } else {
        hdi <- sapply(posteriors, ci, ci = ci, simplify = FALSE)
      }
      for (i in names(hdi)) {
        current_hdi <- hdi[[i]]

        hdi_low <- as.data.frame(t(stats::setNames(current_hdi$CI_low, as.numeric(current_hdi$CI))), stringsAsFactors = FALSE)
        names(hdi_low) <- paste0("CI_low_", names(hdi_low))

        hdi_high <- as.data.frame(t(stats::setNames(current_hdi$CI_high, as.numeric(current_hdi$CI))), stringsAsFactors = FALSE)
        names(hdi_high) <- paste0("CI_high_", names(hdi_high))

        hdi[[i]] <- cbind(hdi_low, hdi_high)
      }
      hdi <- .flatten_list(hdi)
      hdi <- hdi[names(hdi) != "name"]
    } else {
      if (ci_method == "hdi") {
        hdi <- as.data.frame(t(sapply(posteriors, hdi, ci = ci, ...)), stringsAsFactors = FALSE)
      } else {
        hdi <- as.data.frame(t(sapply(posteriors, ci, ci = ci, ...)), stringsAsFactors = FALSE)
      }
      hdi <- hdi[c("CI_low", "CI_high")]
    }
    hdi <- sapply(hdi, as.numeric)
    if (is.null(ncol(hdi))) hdi <- t(hdi) # Catch When nrow == 1
    out <- cbind(out, hdi)
  }


  # Effect Existence
  if (!is.null(test)) {
    test_list <- tolower(c(test))

    if ("all" %in% test_list){
      test_list <- c("pd", "rope", "p_map", "bayesfactor")
    }
    if ("pd" %in% test_list | "p_direction" %in% test_list | "pdir" %in% test_list | "mpe" %in% test_list) {
      out$pd <- sapply(posteriors, p_direction, ...)
    }
    if ("rope" %in% test_list | "equivalence" %in% test_list | "equi" %in% test_list) {
      if (length(ci) == 1 | rope_full) {
        if (rope_full) {
          results_rope <- as.data.frame(t(sapply(posteriors, equivalence_test, range = rope_range, ci = 1)), stringsAsFactors = FALSE)
        } else {
          results_rope <- as.data.frame(t(sapply(posteriors, equivalence_test, range = rope_range, ci = ci)), stringsAsFactors = FALSE)
        }
        results_rope <- results_rope[c("ROPE_Percentage", "ROPE_Equivalence")]
        results_rope$ROPE_Percentage <- as.numeric(results_rope$ROPE_Percentage)
        results_rope$ROPE_Equivalence <- as.character(results_rope$ROPE_Equivalence)
      } else {
        results_rope <- sapply(posteriors, equivalence_test, range = rope_range, ci = ci, simplify = FALSE)
        for (i in names(results_rope)) {
          current_rope <- results_rope[[i]]

          rope_percentage <- as.data.frame(t(stats::setNames(current_rope$ROPE_Percentage, as.numeric(current_rope$CI))), stringsAsFactors = FALSE)
          names(rope_percentage) <- paste0("CI_", names(rope_percentage), "_ROPE_Percentage")
          rope_percentage <- sapply(rope_percentage, as.numeric)

          rope_equivalence <- as.data.frame(t(stats::setNames(current_rope$ROPE_Equivalence, as.numeric(current_rope$CI))), stringsAsFactors = FALSE)
          names(rope_equivalence) <- paste0("CI_", names(rope_equivalence), "_ROPE_Equivalence")
          rope_equivalence <- sapply(rope_equivalence, as.character)

          results_rope[[i]] <- cbind(rope_percentage, rope_equivalence)
        }
        results_rope <- .flatten_list(results_rope)
        results_rope <- results_rope[names(results_rope) != "name"]
      }
      out <- cbind(out, results_rope)
    }
    if ("p_map" %in% test_list | "pmap" %in% test_list) {
      out$p_MAP <- sapply(posteriors, p_map)
    }
    if ("bayes factor" %in% test_list | "bayesfactor" %in% test_list | "bf" %in% test_list) {
      out$BF <- sapply(posteriors, bayesfactor_savagedickey, ...)
    }
    # TODO: add p_ROPE, but first must enhance its implementation
  }

  rownames(out) <- NULL
  return(out)
}


#' @inheritParams insight::get_parameters
#' @rdname describe_posterior
#' @export
describe_posterior.stanreg <- function(posteriors, estimate = "median", ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, dispersion = TRUE, effects = c("fixed", "random", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  x <- insight::get_parameters(posteriors, effects = effects, parameters = parameters)

  describe_posterior(x, ci = ci, ci_method = ci_method, estimate = estimate, test = test, rope_range = rope_range, rope_full = rope_full, dispersion = dispersion, ...)
}

#' @inheritParams insight::get_parameters
#' @rdname describe_posterior
#' @export
describe_posterior.stanreg <- function(posteriors, estimate = "median", ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, dispersion = TRUE, effects = c("fixed", "random", "all"), component = c("conditional", "zi", "zero_inflated", "all"), parameters = NULL, ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  x <- insight::get_parameters(posteriors, effects = effects, component = component, parameters = parameters)

  describe_posterior(x, ci = ci, ci_method = ci_method, estimate = estimate, test = test, rope_range = rope_range, rope_full = rope_full, dispersion = dispersion, ...)
}