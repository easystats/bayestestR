#' Describe Posterior Distributions
#'
#' Compute indices relevant to describe and characterize the posterior distributions.
#'
#' @param posteriors A vector, data frame or model of posterior draws.
#'   **bayestestR** supports a wide range of models (see `methods("describe_posterior")`)
#'   and not all of those are documented in the 'Usage' section, because methods
#'   for other classes mostly resemble the arguments of the `.numeric` method.
#' @param ci_method The type of index used for Credible Interval. Can be
#'   `"ETI"` (default, see [bayestestR::eti()]), `"HDI"`
#'   (see [bayestestR::hdi()]), `"BCI"` (see
#'   [bayestestR::bci()]), `"SPI"` (see [bayestestR::spi()]), or
#'   `"SI"` (see [bayestestR::si()]).
#' @param test The indices of effect existence to compute. Character (vector) or
#'   list with one or more of these options: `"p_direction"` (or `"pd"`),
#'   `"rope"`, `"p_map"`, `"equivalence_test"` (or `"equitest"`),
#'   `"bayesfactor"` (or `"bf"`) or `"all"` to compute all tests.
#'   For each "test", the corresponding \pkg{bayestestR} function is called
#'   (e.g. [bayestestR::rope()] or [bayestestR::p_direction()]) and its results
#'   included in the summary output.
#' @param rope_range ROPE's lower and higher bounds. Should be a list of two
#'   values (e.g., `c(-0.1, 0.1)`) or `"default"`. If `"default"`,
#'   the bounds are set to `x +- 0.1*SD(response)`.
#' @param rope_ci The Credible Interval (CI) probability, corresponding to the
#'   proportion of HDI, to use for the percentage in ROPE.
#' @param keep_iterations If `TRUE`, will keep all iterations (draws) of
#'   bootstrapped or Bayesian models. They will be added as additional columns
#'   named `iter_1, iter_2, ...`. You can reshape them to a long format by
#'   running [bayestestR::reshape_iterations()].
#' @param bf_prior Distribution representing a prior for the computation of
#'   Bayes factors / SI. Used if the input is a posterior, otherwise (in the
#'   case of models) ignored.
#' @param priors Add the prior used for each parameter.
#'
#' @inheritParams point_estimate
#' @inheritParams ci
#' @inheritParams si
#'
#' @details
#' One or more components of point estimates (like posterior mean or median),
#' intervals and tests can be omitted from the summary output by setting the
#' related argument to `NULL`. For example, `test = NULL` and `centrality =
#' NULL` would only return the HDI (or CI).
#'
#' @references
#' - Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D. (2019).
#'   *Indices of Effect Existence and Significance in the Bayesian Framework*.
#'   Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}
#' - [Region of Practical Equivalence (ROPE)](https://easystats.github.io/bayestestR/articles/region_of_practical_equivalence.html)
#' - [Bayes factors](https://easystats.github.io/bayestestR/articles/bayes_factors.html)
#'
#' @examples
#' library(bayestestR)
#'
#' if (require("logspline")) {
#'   x <- rnorm(1000)
#'   describe_posterior(x, verbose = FALSE)
#'   describe_posterior(x,
#'     centrality = "all",
#'     dispersion = TRUE,
#'     test = "all",
#'     verbose = FALSE
#'   )
#'   describe_posterior(x, ci = c(0.80, 0.90), verbose = FALSE)
#'
#'   df <- data.frame(replicate(4, rnorm(100)))
#'   describe_posterior(df, verbose = FALSE)
#'   describe_posterior(
#'     df,
#'     centrality = "all",
#'     dispersion = TRUE,
#'     test = "all",
#'     verbose = FALSE
#'   )
#'   describe_posterior(df, ci = c(0.80, 0.90), verbose = FALSE)
#'
#'   df <- data.frame(replicate(4, rnorm(20)))
#'   head(reshape_iterations(
#'     describe_posterior(df, keep_iterations = TRUE, verbose = FALSE)
#'   ))
#' }
#' \dontrun{
#' # rstanarm models
#' # -----------------------------------------------
#' if (require("rstanarm") && require("emmeans")) {
#'   model <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200, refresh = 0)
#'   describe_posterior(model)
#'   describe_posterior(model, centrality = "all", dispersion = TRUE, test = "all")
#'   describe_posterior(model, ci = c(0.80, 0.90))
#'
#'   # emmeans estimates
#'   # -----------------------------------------------
#'   describe_posterior(emtrends(model, ~1, "wt"))
#' }
#'
#' # BayesFactor objects
#' # -----------------------------------------------
#' if (require("BayesFactor")) {
#'   bf <- ttestBF(x = rnorm(100, 1, 1))
#'   describe_posterior(bf)
#'   describe_posterior(bf, centrality = "all", dispersion = TRUE, test = "all")
#'   describe_posterior(bf, ci = c(0.80, 0.90))
#' }
#' }
#' @export
describe_posterior <- function(posteriors, ...) {
  UseMethod("describe_posterior")
}


#' @export
describe_posterior.default <- function(posteriors, ...) {
  insight::format_error(
    paste0("`describe_posterior()` is not yet implemented for objects of class `", class(posteriors)[1], "`.")
  )
}


#' @keywords internal
.describe_posterior <- function(x,
                                centrality = "median",
                                dispersion = FALSE,
                                ci = 0.95,
                                ci_method = "eti",
                                test = c("p_direction", "rope"),
                                rope_range = "default",
                                rope_ci = 0.95,
                                keep_iterations = FALSE,
                                bf_prior = NULL,
                                BF = 1,
                                ...) {
  if (is.null(x)) {
    insight::format_warning("Could not extract posterior samples.")
    return(NULL)
  }

  # we need this information from the original object
  if (all(rope_range == "default")) {
    rope_range <- rope_range(x, ...)
  }

  if (!is.data.frame(x) && !is.numeric(x)) {
    is_stanmvreg <- inherits(x, "stanmvreg")
    cleaned_parameters <- insight::clean_parameters(x)
    # rename to use `x` in bayes factor later
    x_df <- insight::get_parameters(x, ...)
  } else {
    cleaned_parameters <- NULL
    x_df <- x
  }

  # Arguments fixes
  if (!is.null(centrality) && length(centrality) == 1 && (centrality == "none" || isFALSE(centrality))) {
    centrality <- NULL
  }
  if (!is.null(ci) && length(ci) == 1 && (is.na(ci) || isFALSE(ci))) {
    ci <- NULL
  }
  if (!is.null(test) && length(test) == 1 && (test == "none" || isFALSE(test))) {
    test <- NULL
  }


  # Point-estimates

  if (!is.null(centrality)) {
    estimates <- .prepare_output(
      point_estimate(x_df, centrality = centrality, dispersion = dispersion, ...),
      cleaned_parameters,
      is_stanmvreg
    )
    if (!"Parameter" %in% names(estimates)) {
      estimates <- cbind(
        data.frame(Parameter = "Posterior", stringsAsFactors = FALSE),
        estimates
      )
    }
  } else {
    estimates <- data.frame(Parameter = NA)
  }


  # Uncertainty

  if (!is.null(ci)) {
    ci_method <- match.arg(tolower(ci_method), c("hdi", "spi", "quantile", "ci", "eti", "si", "bci", "bcai"))
    # not sure why "si" requires the model object
    if (ci_method == "si") {
      uncertainty <- ci(x, BF = BF, method = ci_method, prior = bf_prior, ...)
    } else {
      uncertainty <- ci(x_df, ci = ci, method = ci_method, ...)
    }
    uncertainty <- .prepare_output(
      uncertainty,
      cleaned_parameters,
      is_stanmvreg
    )

    if (!"Parameter" %in% names(uncertainty)) {
      uncertainty <- cbind(
        data.frame(Parameter = "Posterior", stringsAsFactors = FALSE),
        uncertainty
      )
    }
  } else {
    uncertainty <- data.frame(Parameter = NA)
  }


  # Effect Existence

  if (!is.null(test)) {
    test <- .check_test_values(test)
    if ("all" %in% test) {
      test <- c("pd", "p_map", "p_rope", "p_significance", "rope", "equivalence", "bf")
    }

    ## TODO no BF for arm::sim
    if (inherits(x, c("sim", "sim.merMod", "mcmc", "stanfit"))) {
      test <- setdiff(test, "bf")
    }

    ## TODO enable once "rope()" works for multi-response models

    # no ROPE for multi-response models
    if (insight::is_multivariate(x)) {
      test <- setdiff(test, c("rope", "p_rope"))
      insight::format_warning(
        "Multivariate response models are not yet supported for tests `rope` and `p_rope`."
      )
    }

    # MAP-based p-value

    if (any(c("p_map", "p_pointnull") %in% test)) {
      test_pmap <- .prepare_output(
        p_map(x_df, ...),
        cleaned_parameters,
        is_stanmvreg
      )
      if (!is.data.frame(test_pmap)) {
        test_pmap <- data.frame(
          Parameter = "Posterior",
          p_map = test_pmap,
          stringsAsFactors = FALSE
        )
      }
    } else {
      test_pmap <- data.frame(Parameter = NA)
    }


    # Probability of direction

    if (any(c("pd", "p_direction", "pdir", "mpe") %in% test)) {
      test_pd <- .prepare_output(
        p_direction(x_df, ...),
        cleaned_parameters,
        is_stanmvreg
      )
      if (!is.data.frame(test_pd)) {
        test_pd <- data.frame(
          Parameter = "Posterior",
          pd = test_pd,
          stringsAsFactors = FALSE
        )
      }
    } else {
      test_pd <- data.frame(Parameter = NA)
    }

    # Probability of rope

    if ("p_rope" %in% test) {
      test_prope <- .prepare_output(
        p_rope(x_df, range = rope_range, ...),
        cleaned_parameters,
        is_stanmvreg
      )
      if (!"Parameter" %in% names(test_prope)) {
        test_prope <- cbind(
          data.frame(Parameter = "Posterior", stringsAsFactors = FALSE),
          test_prope
        )
      }
    } else {
      test_prope <- data.frame(Parameter = NA)
    }

    # Probability of significance

    if (any(c("ps", "p_sig", "p_significance") %in% test)) {
      test_psig <- .prepare_output(
        p_significance(x_df, threshold = rope_range, ...),
        cleaned_parameters,
        is_stanmvreg
      )
      if (!is.data.frame(test_psig)) {
        test_psig <- data.frame(
          Parameter = "Posterior",
          ps = test_psig,
          stringsAsFactors = FALSE
        )
      }
    } else {
      test_psig <- data.frame(Parameter = NA)
    }


    # ROPE

    if ("rope" %in% test) {
      test_rope <- .prepare_output(
        rope(x_df, range = rope_range, ci = rope_ci, ...),
        cleaned_parameters,
        is_stanmvreg
      )
      if (!"Parameter" %in% names(test_rope)) {
        test_rope <- cbind(
          data.frame(Parameter = "Posterior", stringsAsFactors = FALSE),
          test_rope
        )
      }
      names(test_rope)[names(test_rope) == "CI"] <- "ROPE_CI"
    } else {
      test_rope <- data.frame(Parameter = NA)
    }


    # Equivalence test

    if (any(c("equivalence", "equivalence_test", "equitest") %in% test)) {
      dot_args <- list(...)
      dot_args$verbose <- !"rope" %in% test
      test_equi <- .prepare_output(
        equivalence_test(x_df,
          range = rope_range,
          ci = rope_ci,
          dot_args
        ),
        cleaned_parameters,
        is_stanmvreg
      )
      test_equi$Cleaned_Parameter <- NULL

      if (!"Parameter" %in% names(test_equi)) {
        test_equi <- cbind(
          data.frame(Parameter = "Posterior", stringsAsFactors = FALSE),
          test_equi
        )
      }
      names(test_equi)[names(test_equi) == "CI"] <- "ROPE_CI"

      test_rope <- merge(test_rope, test_equi, all = TRUE)
      test_rope <- test_rope[!names(test_rope) %in% c("HDI_low", "HDI_high")]
    }


    # Bayes Factors

    if (any(c("bf", "bayesfactor", "bayes_factor") %in% test)) {
      test_bf <- tryCatch(
        .prepare_output(
          bayesfactor_parameters(x, prior = bf_prior, ...),
          cleaned_parameters,
          is_stanmvreg
        ),
        error = function(e) data.frame("Parameter" = NA)
      )
      if (!"Parameter" %in% names(test_bf)) {
        test_bf <- cbind(
          data.frame(Parameter = "Posterior", stringsAsFactors = FALSE),
          test_bf
        )
      }
    } else {
      test_bf <- data.frame("Parameter" = NA)
    }
  } else {
    test_pd <- data.frame(
      "Parameter" = NA,
      "Effects" = NA,
      "Component" = NA,
      "Response" = NA
    )

    test_rope <- data.frame(
      "Parameter" = NA,
      "Effects" = NA,
      "Component" = NA,
      "Response" = NA
    )

    test_prope <- data.frame(
      "Parameter" = NA,
      "Effects" = NA,
      "Component" = NA,
      "Response" = NA
    )

    test_psig <- data.frame(
      "Parameter" = NA,
      "Effects" = NA,
      "Component" = NA,
      "Response" = NA
    )

    test_bf <- data.frame(
      "Parameter" = NA,
      "Effects" = NA,
      "Component" = NA,
      "Response" = NA
    )

    test_pmap <- data.frame(
      "Parameter" = NA,
      "Effects" = NA,
      "Component" = NA,
      "Response" = NA
    )
  }


  # for data frames or numeric, and even for some models, we don't
  # have the "Effects" or "Component" column for all data frames.
  # To make "merge()" work, we add those columns to all data frames,
  # filled with NA, and remove the columns later if necessary

  estimates <- .add_effects_component_column(estimates)
  uncertainty <- .add_effects_component_column(uncertainty)
  test_pmap <- .add_effects_component_column(test_pmap)
  test_pd <- .add_effects_component_column(test_pd)
  test_prope <- .add_effects_component_column(test_prope)
  test_psig <- .add_effects_component_column(test_psig)
  test_rope <- .add_effects_component_column(test_rope)
  test_bf <- .add_effects_component_column(test_bf)


  # at least one "valid" data frame needs a row id, to restore
  # row-order after merging

  if (!all(is.na(estimates$Parameter))) {
    estimates$.rowid <- seq_len(nrow(estimates))
  } else if (!all(is.na(test_pmap$Parameter))) {
    test_pmap$.rowid <- seq_len(nrow(test_pmap))
  } else if (!all(is.na(test_pd$Parameter))) {
    test_pd$.rowid <- seq_len(nrow(test_pd))
  } else if (!all(is.na(test_prope$Parameter))) {
    test_prope$.rowid <- seq_len(nrow(test_prope))
  } else if (!all(is.na(test_psig$Parameter))) {
    test_psig$.rowid <- seq_len(nrow(test_psig))
  } else if (!all(is.na(test_rope$Parameter))) {
    test_rope$.rowid <- seq_len(nrow(test_rope))
  } else if (!all(is.na(test_bf$Parameter))) {
    test_bf$.rowid <- seq_len(nrow(test_bf))
  } else {
    estimates$.rowid <- seq_len(nrow(estimates))
  }


  # remove duplicated columns
  if (all(c("rope", "p_rope") %in% test)) {
    test_prope$ROPE_low <- NULL
    test_prope$ROPE_high <- NULL
  }

  # merge all data frames
  merge_by <- c("Parameter", "Effects", "Component", "Response")
  # merge_by <- intersect(merge_by, colnames(estimates))
  out <- merge(estimates, uncertainty, by = merge_by, all = TRUE)
  out <- merge(out, test_pmap, by = merge_by, all = TRUE)
  out <- merge(out, test_pd, by = merge_by, all = TRUE)
  out <- merge(out, test_prope, by = merge_by, all = TRUE)
  out <- merge(out, test_psig, by = merge_by, all = TRUE)
  out <- merge(out, test_rope, by = merge_by, all = TRUE)
  out <- merge(out, test_bf, by = merge_by, all = TRUE)
  out <- out[!is.na(out$Parameter), ]

  # check which columns can be removed at the end. In any case, we don't
  # need .rowid in the returned data frame, and when the Effects or Component
  # column consist only of missing values, we remove those columns as well

  remove_columns <- ".rowid"
  if (insight::n_unique(out$Effects, na.rm = TRUE) < 2) remove_columns <- c(remove_columns, "Effects")
  if (insight::n_unique(out$Component, na.rm = TRUE) < 2) remove_columns <- c(remove_columns, "Component")
  if (insight::n_unique(out$Response, na.rm = TRUE) < 2) remove_columns <- c(remove_columns, "Response")

  # Restore columns order
  out <- datawizard::data_remove(out[order(out$.rowid), ], remove_columns, verbose = FALSE)

  # Add iterations
  if (keep_iterations) {
    row_order <- out$Parameter
    iter <- as.data.frame(t(as.data.frame(x_df, ...)))
    names(iter) <- paste0("iter_", seq_len(ncol(iter)))
    iter$Parameter <- row.names(iter)
    out <- merge(out, iter, all.x = TRUE, by = "Parameter")
    out <- out[match(row_order, out$Parameter), ]
    row.names(out) <- NULL
  }



  # Prepare output
  attr(out, "ci_method") <- ci_method
  out
}


#' @keywords internal
.add_effects_component_column <- function(x) {
  if (!"Effects" %in% names(x)) x <- cbind(x, data.frame("Effects" = NA))
  if (!"Component" %in% names(x)) x <- cbind(x, data.frame("Component" = NA))
  if (!"Response" %in% names(x)) x <- cbind(x, data.frame("Response" = NA))
  x
}




# Models based on simple data frame of posteriors ---------------------


#' @rdname describe_posterior
#' @export
describe_posterior.numeric <- function(posteriors,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = c("p_direction", "rope"),
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       keep_iterations = FALSE,
                                       bf_prior = NULL,
                                       BF = 1,
                                       ...) {
  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    BF = BF,
    ...
  )

  class(out) <- unique(c("describe_posterior", "see_describe_posterior", class(out)))
  out
}


#' @export
describe_posterior.double <- describe_posterior.numeric


#' @export
describe_posterior.data.frame <- describe_posterior.numeric


#' @export
describe_posterior.sim.merMod <- describe_posterior.numeric


#' @export
describe_posterior.sim <- describe_posterior.numeric


#' @export
describe_posterior.bayesQR <- function(posteriors,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = c("p_direction", "rope"),
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       keep_iterations = FALSE,
                                       parameters = NULL,
                                       ...) {
  out <- .describe_posterior(
    insight::get_parameters(posteriors),
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    effects = "fixed",
    parameters = parameters,
    ...
  )

  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @export
describe_posterior.blrm <- describe_posterior.bayesQR


#' @export
describe_posterior.mcmc <- describe_posterior.bayesQR


#' @export
describe_posterior.mcmc.list <- describe_posterior.bayesQR


#' @export
describe_posterior.BGGM <- describe_posterior.bayesQR


#' @export
describe_posterior.draws <- function(posteriors,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = c("p_direction", "rope"),
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     keep_iterations = FALSE,
                                     bf_prior = NULL,
                                     BF = 1,
                                     ...) {
  out <- .describe_posterior(
    .posterior_draws_to_df(posteriors),
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    BF = BF,
    ...
  )

  class(out) <- unique(c("describe_posterior", "see_describe_posterior", class(out)))
  out
}

#' @export
describe_posterior.rvar <- describe_posterior.draws



# easystats methods ------------------------


#' @export
describe_posterior.effectsize_std_params <- function(posteriors,
                                                     centrality = "median",
                                                     dispersion = FALSE,
                                                     ci = 0.95,
                                                     ci_method = "eti",
                                                     test = c("p_direction", "rope"),
                                                     rope_range = "default",
                                                     rope_ci = 0.95,
                                                     keep_iterations = FALSE,
                                                     bf_prior = NULL,
                                                     BF = 1,
                                                     ...) {
  class(posteriors) <- "data.frame"

  no_unique <- vapply(posteriors, function(col) {
    length(unique(col)) == 1
  }, FUN.VALUE = TRUE)

  if (any(no_unique)) {
    no_unique <- which(no_unique)

    out <- describe_posterior.data.frame(
      posteriors[, -no_unique],
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      keep_iterations = keep_iterations,
      bf_prior = bf_prior,
      BF = BF,
      ...
    )

    out_int <- data.frame(Parameter = colnames(posteriors)[no_unique])
    col_diff <- setdiff(colnames(out), colnames(out_int))
    out_int[, col_diff] <- NA
    out <- rbind(out_int, out)

    out <- out[order(match(out$Parameter, colnames(posteriors))), ]

    return(out)
  }

  describe_posterior.data.frame(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    BF = BF,
    ...
  )
}


#' @export
describe_posterior.get_predicted <- function(posteriors,
                                             centrality = "median",
                                             dispersion = FALSE,
                                             ci = 0.95,
                                             ci_method = "eti",
                                             test = NULL,
                                             ...) {
  if ("iterations" %in% names(attributes(posteriors))) {
    describe_posterior(
      as.data.frame(t(attributes(posteriors)$iterations)),
      centrality = centrality,
      dispersion = dispersion,
      ci = ci,
      ci_method = ci_method,
      test = test,
      ...
    )
  } else {
    insight::format_error("No iterations present in the output.")
  }
}




# emmeans ---------------------------


#' @export
describe_posterior.emmGrid <- function(posteriors,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = c("p_direction", "rope"),
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       keep_iterations = FALSE,
                                       bf_prior = NULL,
                                       BF = 1,
                                       ...) {
  if (any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test)) ||
    "si" %in% tolower(ci_method)) {
    samps <- .clean_priors_and_posteriors(posteriors, bf_prior)
    bf_prior <- samps$prior
    posteriors <- samps$posterior
  } else {
    posteriors <- insight::get_parameters(posteriors)
  }


  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    BF = BF,
    ...
  )

  row.names(out) <- NULL # Reset row names

  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))

  out
}


#' @export
describe_posterior.emm_list <- describe_posterior.emmGrid




# Stan ------------------------------


#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @rdname describe_posterior
#' @export
describe_posterior.stanreg <- function(posteriors,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = c("p_direction", "rope"),
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       keep_iterations = FALSE,
                                       bf_prior = NULL,
                                       diagnostic = c("ESS", "Rhat"),
                                       priors = FALSE,
                                       effects = c("fixed", "random", "all"),
                                       component = c(
                                         "location", "all", "conditional",
                                         "smooth_terms", "sigma", "distributional",
                                         "auxiliary"
                                       ),
                                       parameters = NULL,
                                       BF = 1,
                                       ...) {
  if ((any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test)) ||
    "si" %in% tolower(ci_method)) && is.null(bf_prior)) {
    bf_prior <- suppressMessages(unupdate(posteriors))
  }

  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    BF = BF,
    effects = effects,
    component = component,
    parameters = parameters,
    ...
  )

  diagnostic <- diagnostic_posterior(
    posteriors,
    diagnostic,
    effects = effects,
    component = component,
    parameters = parameters,
    ...
  )
  out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, parameters = out$Parameter, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  out <- .add_clean_parameters_attribute(out, posteriors)
  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @export
describe_posterior.stanmvreg <- function(posteriors,
                                         centrality = "median",
                                         dispersion = FALSE,
                                         ci = 0.95,
                                         ci_method = "eti",
                                         test = "p_direction",
                                         rope_range = "default",
                                         rope_ci = 0.95,
                                         keep_iterations = FALSE,
                                         bf_prior = NULL,
                                         diagnostic = c("ESS", "Rhat"),
                                         priors = FALSE,
                                         effects = c("fixed", "random", "all"),
                                         component = c(
                                           "location", "all", "conditional",
                                           "smooth_terms", "sigma", "distributional",
                                           "auxiliary"
                                         ),
                                         parameters = NULL,
                                         ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    effects = effects,
    parameters = parameters,
    ...
  )

  if (is.null(out$Response)) {
    out$Response <- gsub("(b\\[)*(.*)\\|(.*)", "\\2", out$Parameter)
  }

  diagnostic <- diagnostic_posterior(
    posteriors,
    diagnostic,
    effects = effects,
    parameters = parameters,
    ...
  )
  out <- .merge_and_sort(out, diagnostic, by = c("Parameter", "Response"), all = TRUE)

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, parameters = NULL, ...)
    priors_data$Parameter <- gsub("^(.*)\\|(.*)", replacement = "\\2", priors_data$Parameter)
    out <- .merge_and_sort(out, priors_data, by = c("Parameter", "Response"), all = TRUE)
  }

  out <- .add_clean_parameters_attribute(out, posteriors)
  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @inheritParams insight::get_parameters
#' @inheritParams diagnostic_posterior
#' @export
describe_posterior.stanfit <- function(posteriors,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = c("p_direction", "rope"),
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       keep_iterations = FALSE,
                                       diagnostic = c("ESS", "Rhat"),
                                       effects = c("fixed", "random", "all"),
                                       parameters = NULL,
                                       priors = FALSE,
                                       ...) {
  effects <- match.arg(effects)
  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    effects = effects,
    parameters = parameters,
    ...
  )

  diagnostic <- diagnostic_posterior(
    posteriors,
    diagnostic,
    effects = effects,
    parameters = parameters,
    ...
  )
  out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, parameters = out$Parameter, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  attr(out, "ci_method") <- ci_method
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @inheritParams describe_posterior.stanreg
#' @rdname describe_posterior
#' @export
describe_posterior.brmsfit <- function(posteriors,
                                       centrality = "median",
                                       dispersion = FALSE,
                                       ci = 0.95,
                                       ci_method = "eti",
                                       test = c("p_direction", "rope"),
                                       rope_range = "default",
                                       rope_ci = 0.95,
                                       keep_iterations = FALSE,
                                       bf_prior = NULL,
                                       diagnostic = c("ESS", "Rhat"),
                                       effects = c("fixed", "random", "all"),
                                       component = c(
                                         "conditional", "zi", "zero_inflated",
                                         "all", "location", "distributional",
                                         "auxiliary"
                                       ),
                                       parameters = NULL,
                                       BF = 1,
                                       priors = FALSE,
                                       ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  if ((any(c("all", "bf", "bayesfactor", "bayes_factor") %in% tolower(test)) ||
    "si" %in% tolower(ci_method)) && is.null(bf_prior)) {
    bf_prior <- suppressMessages(unupdate(posteriors))
  }

  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    bf_prior = bf_prior,
    BF = BF,
    effects = effects,
    component = component,
    parameters = parameters,
    ...
  )

  if (!is.null(diagnostic)) {
    diagnostic <- diagnostic_posterior(
      posteriors,
      diagnostic,
      effects = effects,
      component = component,
      parameters = parameters,
      ...
    )
    out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)
  }

  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, parameters = out$Parameter, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  out <- .add_clean_parameters_attribute(out, posteriors)
  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @export
describe_posterior.blavaan <- describe_posterior.stanfit




# other models --------------------------------


#' @inheritParams describe_posterior.stanreg
#' @export
describe_posterior.MCMCglmm <- function(posteriors,
                                        centrality = "median",
                                        dispersion = FALSE,
                                        ci = 0.95,
                                        ci_method = "eti",
                                        test = c("p_direction", "rope"),
                                        rope_range = "default",
                                        rope_ci = 0.95,
                                        keep_iterations = FALSE,
                                        diagnostic = "ESS",
                                        parameters = NULL,
                                        ...) {
  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    effects = "fixed",
    parameters = parameters,
    ...
  )

  if (!is.null(diagnostic) && diagnostic == "ESS") {
    diagnostic <- effective_sample(posteriors, effects = "fixed", parameters = parameters, ...)
    out <- .merge_and_sort(out, diagnostic, by = "Parameter", all = TRUE)
  }

  out
}


#' @export
describe_posterior.bcplm <- function(posteriors,
                                     centrality = "median",
                                     dispersion = FALSE,
                                     ci = 0.95,
                                     ci_method = "eti",
                                     test = c("p_direction", "rope"),
                                     rope_range = "default",
                                     rope_ci = 0.95,
                                     keep_iterations = FALSE,
                                     priors = TRUE,
                                     parameters = NULL,
                                     ...) {
  out <- .describe_posterior(
    insight::get_parameters(posteriors),
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    effects = "fixed",
    parameters = parameters,
    ...
  )
  if (isTRUE(priors)) {
    priors_data <- describe_prior(posteriors, parameters = out$Parameter, ...)
    out <- .merge_and_sort(out, priors_data, by = "Parameter", all = TRUE)
  }

  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}


#' @export
describe_posterior.bamlss <- function(posteriors,
                                      centrality = "median",
                                      dispersion = FALSE,
                                      ci = 0.95,
                                      ci_method = "eti",
                                      test = c("p_direction", "rope"),
                                      rope_range = "default",
                                      rope_ci = 0.95,
                                      keep_iterations = FALSE,
                                      component = c("all", "conditional", "location"),
                                      parameters = NULL,
                                      ...) {
  component <- match.arg(component)
  out <- .describe_posterior(
    posteriors,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    component = component,
    parameters = parameters,
    ...
  )

  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}




# BayesFactor --------------------


#' @export
describe_posterior.BFBayesFactor <- function(posteriors,
                                             centrality = "median",
                                             dispersion = FALSE,
                                             ci = 0.95,
                                             ci_method = "eti",
                                             test = c("p_direction", "rope", "bf"),
                                             rope_range = "default",
                                             rope_ci = 0.95,
                                             keep_iterations = FALSE,
                                             priors = TRUE,
                                             verbose = TRUE,
                                             ...) {
  # Match test args  to catch BFs
  if (!is.null(test)) {
    test <- .check_test_values(test)
    if ("all" %in% test) {
      test <- c("pd", "p_map", "p_rope", "p_significance", "rope", "equivalence", "bf")
    }
  }

  # Remove BF from list
  if (any(c("bf", "bayesfactor", "bayes_factor") %in% test)) {
    test <- test[!test %in% c("bf", "bayesfactor", "bayes_factor")]
    if (length(test) == 0L) test <- NULL
    compute_bf <- TRUE
  } else {
    compute_bf <- FALSE
  }

  draws <- insight::get_parameters(posteriors)
  if (all(rope_range == "default")) {
    rope_range <- rope_range(posteriors, verbose = verbose)
  }

  # Describe posterior
  out <- .describe_posterior(
    draws,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    keep_iterations = keep_iterations,
    ...
  )

  if (is.null(out)) {
    return(NULL)
  }

  # Compute and read BF a posteriori
  if (compute_bf) {
    tryCatch(
      {
        out$log_BF <- as.data.frame(bayesfactor_models(posteriors[1], ...))[-1, ]$log_BF
        out$BF <- exp(out$log_BF)
      },
      error = function(e) {
        NULL
      }
    )
  }


  # Add priors
  if (priors) {
    priors_data <- describe_prior(posteriors, ...)
    out <- .merge_and_sort(out, priors_data, by = intersect(names(out), names(priors_data)), all = TRUE)
  }

  attr(out, "ci_method") <- ci_method
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(posteriors))
  class(out) <- c("describe_posterior", "see_describe_posterior", class(out))
  out
}





# Helpers -----------------------------------------------------------------


#' @keywords internal
.check_test_values <- function(test) {
  match.arg(tolower(test), c(
    "pd", "p_direction", "pdir", "mpe", "ps", "psig", "p_significance",
    "p_rope", "rope", "equivalence", "equivalence_test", "equitest",
    "bf", "bayesfactor", "bayes_factor", "p_map", "all"
  ), several.ok = TRUE)
}
