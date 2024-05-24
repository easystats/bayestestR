# clean priors and posteriors ---------------------------------------------

#' @keywords internal
.clean_priors_and_posteriors <- function(posterior, prior, ...) {
  UseMethod(".clean_priors_and_posteriors")
}

#' @keywords internal
.clean_priors_and_posteriors.stanreg <- function(posterior, prior,
                                                 verbose = TRUE,
                                                 ...) {
  # Get Priors
  if (is.null(prior)) {
    prior <- posterior
  }


  prior <- try(unupdate(prior, verbose = verbose), silent = TRUE)
  if (methods::is(prior, "try-error")) {
    if (grepl("flat priors", prior, fixed = TRUE)) {
      prior <- paste0(
        prior, "Could not therefore compute Bayes factors, as these inform about ",
        "the raltive likelihood of two 'hypotheses', and flat priors provide no ",
        "likelihood.\n",
        "See '?bayesfactor_parameters' for more information.\n"
      )
    }
    insight::format_error(prior)
  }

  prior <- insight::get_parameters(prior, ...)
  posterior <- insight::get_parameters(posterior, ...)

  list(
    posterior = posterior,
    prior = prior
  )
}

#' @keywords internal
.clean_priors_and_posteriors.brmsfit <- .clean_priors_and_posteriors.stanreg


#' @keywords internal
.clean_priors_and_posteriors.blavaan <- function(posterior, prior,
                                                 verbose = TRUE, ...) {
  # Get Priors
  if (is.null(prior)) {
    prior <- posterior
  }

  prior <- unupdate(prior, verbose = verbose)

  prior <- insight::get_parameters(prior)
  posterior <- insight::get_parameters(posterior)

  list(
    posterior = posterior,
    prior = prior
  )
}


#' @keywords internal
.clean_priors_and_posteriors.emmGrid <- function(posterior,
                                                 prior,
                                                 verbose = TRUE,
                                                 ...) {
  insight::check_if_installed("emmeans")

  if (is.null(prior)) {
    prior <- posterior
    if (verbose) {
      insight::format_warning("Prior not specified! Please provide the original model to get meaningful results.")
    }
  }


  if (!inherits(prior, "emmGrid")) {
    # then is it a model
    on.exit(
      insight::format_error(paste0(
        "Unable to reconstruct prior estimates.\n",
        "Perhaps the emmGrid object has been transformed or regrid()-ed?\n",
        "See function details.\n\n",
        "Instead, you can reestimate the emmGrid with a prior model, Try:\n",
        "\tprior_model <- unupdate(mode)\n",
        "\tprior_emmgrid <- emmeans(prior_model, ...) # pass this as the 'prior' argument."
      ))
    )

    if (inherits(prior, "brmsfit")) {
      insight::format_error("Cannot rebuild prior emmGrid from a brmsfit model.")
    }


    prior <- try(unupdate(prior, verbose = verbose), silent = TRUE)
    if (inherits(prior, "try-error")) {
      on.exit() # undo general error message
      if (grepl("flat priors", prior, fixed = TRUE)) {
        prior <- paste0(
          prior, "Could not therefore compute Bayes factors, as these inform about ",
          "the raltive likelihood of two 'hypotheses', and flat priors provide no ",
          "likelihood.\n",
          "See '?bayesfactor_parameters' for more information.\n"
        )
      }
      insight::format_error(prior)
    }

    prior <- suppressWarnings(emmeans::ref_grid(prior))
    prior <- prior@post.beta

    if (!isTRUE(all.equal(colnames(prior), colnames(posterior@post.beta)))) {
      insight::format_error("post.beta and prior.beta are non-conformable arguments.")
    }
    prior <- stats::update(posterior, post.beta = prior)
    on.exit() # undo general error message
  }

  prior <- insight::get_parameters(prior)
  posterior <- insight::get_parameters(posterior)

  list(
    posterior = posterior,
    prior = prior
  )
}

.clean_priors_and_posteriors.emm_list <- function(posterior, prior,
                                                  verbose = TRUE, ...) {
  if (is.null(prior)) {
    prior <- posterior
    if (verbose) {
      insight::format_warning("Prior not specified! Please provide the original model to get meaningful results.")
    }
  }

  if (!inherits(prior, "emm_list")) {
    # prior is a model

    if (inherits(prior, "brmsfit")) {
      insight::format_error("Cannot rebuild prior emm_list from a brmsfit model.")
    }

    prior <- try(unupdate(prior, verbose = verbose), silent = TRUE)
    if (inherits(prior, "try-error")) {
      if (grepl("flat priors", prior, fixed = TRUE)) {
        prior <- paste0(
          prior, "Could not therefore compute Bayes factors, as these inform about ",
          "the raltive likelihood of two 'hypotheses', and flat priors provide no ",
          "likelihood.\n",
          "See '?bayesfactor_parameters' for more information.\n"
        )
      }
      insight::format_error(prior)
    }
  }
  # prior is now a model, or emm_list

  # is it a model?
  pass_em <- inherits(prior, "emm_list")

  res <- lapply(seq_along(posterior), function(i) {
    .clean_priors_and_posteriors.emmGrid(
      posterior[[i]],
      prior = if (pass_em) prior[[i]] else prior,
      verbose = verbose
    )
  })

  posterior <- do.call("cbind", lapply(res, "[[", "posterior"))
  prior <- do.call("cbind", lapply(res, "[[", "prior"))


  list(
    posterior = posterior,
    prior = prior
  )
}


# BMA ---------------------------------------------------------------------

#' @keywords internal
.get_model_table <- function(BFGrid, priorOdds = NULL, add_effects_table = TRUE, ...) {
  denominator <- attr(BFGrid, "denominator")
  BFGrid <- rbind(BFGrid[denominator, ], BFGrid[-denominator, ])
  attr(BFGrid, "denominator") <- 1

  # This looks like it does nothing, but this is needed to prevent Inf in large BFs.
  # Small BFs are better than large BFs
  BFGrid <- stats::update(BFGrid, reference = "top")

  # Prior and post odds
  Modelnames <- BFGrid$Model
  if (is.null(priorOdds)) {
    priorOdds <- rep(1, length(Modelnames) - 1)
  }
  priorOdds <- c(1, priorOdds)

  prior_logodds <- log(priorOdds)
  posterior_logodds <- prior_logodds + BFGrid$log_BF

  # norm
  prior_logodds <- prior_logodds - log(sum(exp(prior_logodds)))
  posterior_logodds <- posterior_logodds - log(sum(exp(posterior_logodds)))

  df.model <- data.frame(
    Modelnames,
    priorProbs = exp(prior_logodds),
    postProbs = exp(posterior_logodds),
    stringsAsFactors = FALSE
  )

  # add effects table
  if (add_effects_table) {
    for (m in seq_len(nrow(df.model))) {
      tmp_terms <- .make_terms(df.model$Modelnames[m])
      if (length(tmp_terms) > 0) {
        missing_terms <- !tmp_terms %in% colnames(df.model)
        if (any(missing_terms)) df.model[, tmp_terms[missing_terms]] <- NA
        df.model[m, tmp_terms] <- TRUE
      }
    }
  }

  df.model[is.na(df.model)] <- FALSE

  df.model
}

#' @keywords internal
.make_terms <- function(formula) {
  sort_interactions <- function(x) {
    if (grepl(":", x, fixed = TRUE)) {
      effs <- unlist(strsplit(x, ":", fixed = TRUE))
      x <- paste0(sort(effs), collapse = ":")
    }
    x
  }
  formula.f <- stats::as.formula(paste0("~", formula))
  all.terms <- attr(stats::terms(formula.f), "term.labels")

  # Fixed
  fix_trms <- all.terms[!grepl("|", all.terms, fixed = TRUE)] # no random
  if (length(fix_trms) > 0) {
    fix_trms <- sapply(fix_trms, sort_interactions)
  }

  # Random
  random_parts <- paste0(grep("|", all.terms, fixed = TRUE, value = TRUE)) # only random
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
    tmp_trms <- sapply(tmp_trms, sort_interactions)

    if (!any(unlist(strsplit(as.character(tmp_random[[i]])[[2]], " + ", fixed = TRUE)) == "0")) {
      tmp_trms <- c("1", tmp_trms)
    }

    rand_trms[[i]] <- paste0(tmp_trms, ":", random_units[[i]])
  }

  c(fix_trms, unlist(rand_trms))
}

# make_BF_plot_data -------------------------------------------------------

#' @keywords internal
.make_BF_plot_data <- function(posterior,
                               prior,
                               direction,
                               null,
                               extend_scale = 0.05,
                               precision = 2^8,
                               ...) {
  insight::check_if_installed("logspline")

  estimate_samples_density <- function(samples) {
    nm <- insight::safe_deparse_symbol(substitute(samples))
    samples <- utils::stack(samples)
    samples <- split(samples, samples$ind)

    samples <- lapply(samples, function(data) {
      # 1. estimate density
      x <- data$values

      x_range <- range(x)
      x_rangex <- stats::median(x) + 7 * stats::mad(x) * c(-1, 1)
      x_range <- c(
        max(c(x_range[1], x_rangex[1])),
        min(c(x_range[2], x_rangex[2]))
      )
      x_range <- range(c(x_range, null)[!is.infinite(c(x_range, null))])

      extension_scale <- diff(x_range) * extend_scale
      x_range <- x_range + c(-1, 1) * extension_scale

      x_axis <- seq(x_range[1], x_range[2], length.out = precision)
      # x_axis <- sort(unique(c(x_axis, null)))
      f_x <- .logspline(x, ...)
      y <- logspline::dlogspline(x_axis, f_x)
      d_points <- data.frame(x = x_axis, y = y)

      # 2. estimate points
      d_null <- stats::approx(d_points$x, d_points$y, xout = null)
      d_null$y[is.na(d_null$y)] <- 0

      # 3. direction?
      if (direction > 0) {
        d_points <- d_points[d_points$x >= min(null), , drop = FALSE]
        if (is.infinite(min(null))) {
          norm_factor <- 1
        } else {
          norm_factor <- 1 - logspline::plogspline(min(null), f_x)
        }
        d_points$y <- d_points$y / norm_factor
        d_null$y <- d_null$y / norm_factor
      } else if (direction < 0) {
        d_points <- d_points[d_points$x <= max(null), , drop = FALSE]
        if (is.infinite(max(null))) {
          norm_factor <- 1
        } else {
          norm_factor <- logspline::plogspline(max(null), f_x)
        }
        d_points$y <- d_points$y / norm_factor
        d_null$y <- d_null$y / norm_factor
      }

      d_points$ind <- d_null$ind <- data$ind[1]
      list(d_points, d_null)
    })

    # 4a. organize
    point0 <- lapply(samples, function(.) as.data.frame(.[[2]]))
    point0 <- do.call("rbind", point0)

    samplesX <- lapply(samples, function(.) .[[1]])
    samplesX <- do.call("rbind", samplesX)

    samplesX$Distribution <- point0$Distribution <- nm
    rownames(samplesX) <- rownames(point0) <- NULL

    list(samplesX, point0)
  }

  # 4b. orgenize
  posterior <- estimate_samples_density(posterior)
  prior <- estimate_samples_density(prior)

  list(
    plot_data = rbind(posterior[[1]], prior[[1]]),
    d_points = rbind(posterior[[2]], prior[[2]])
  )
}

# As numeric vector -------------------------------------------------------

#' @export
as.numeric.bayesfactor_inclusion <- function(x, log = FALSE, ...) {
  out <- x[["log_BF"]]
  if (!log) out <- exp(out)
  return(out)
}

#' @export
as.numeric.bayesfactor_models <- as.numeric.bayesfactor_inclusion

#' @export
as.numeric.bayesfactor_parameters <- as.numeric.bayesfactor_inclusion

#' @export
as.numeric.bayesfactor_restricted <- as.numeric.bayesfactor_inclusion

## Double:

#' @export
as.double.bayesfactor_inclusion <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_models <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_parameters <- as.numeric.bayesfactor_inclusion

#' @export
as.double.bayesfactor_restricted <- as.numeric.bayesfactor_inclusion



# logspline ---------------------------------------------------------------

#' @keywords internal
.logspline <- function(x, ...) {
  insight::check_if_installed("logspline")

  in_args <- list(...)
  # arg_names <- names(formals(logspline::logspline, envir = parent.frame()))
  arg_names <- names(formals(logspline::logspline))
  in_args <- in_args[names(in_args) %in% arg_names]
  in_args <- c(list(x = x), in_args)
  suppressWarnings(do.call(logspline::logspline, in_args))
}
