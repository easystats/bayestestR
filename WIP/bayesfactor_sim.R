#' @importFrom insight get_parameters
#' @export
bayesfactor_parameters.sim.merMod <- function(posterior, prior = NULL,
                                              direction = "two-sided", null = 0,
                                              verbose = TRUE,
                                              effects = c("fixed", "random", "all"),
                                              ...) {
  effects <- match.arg(effects)

  # find direction
  direction <- .get_direction(direction)

  posterior <- insight::get_parameters(posterior, effects = effects)
  params <- colnames(posterior)

  # Get Priors
  if (is.null(prior)) {
    prior <- as.data.frame(lapply(params, function(p) {
      distribution_normal(nrow(posterior), mean = 0, sd = 2)
    }))
    colnames(prior) <- params
    if (verbose) {
      warning(
        "Prior not specified! ",
        "Please specify priors (with column order matching 'posterior', or by",
        " providing the model object as 'prior' argument)",
        " to get meaningful results. Using normally distributed priors with",
        " location=0 and scale=2 as default.", call. = FALSE
      )
    }
  } else if (insight::is_model(prior)) {
    prior <- .armsim_weakly_priors(prior, nrow(posterior))
  }


  # Get savage-dickey BFs
  bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null, ...
  )
}



#' @importFrom insight get_parameters
#' @export
bayesfactor_parameters.sim <- function(posterior, prior = NULL,
                                       direction = "two-sided", null = 0,
                                       verbose = TRUE,
                                       ...) {
  # find direction
  direction <- .get_direction(direction)

  posterior <- insight::get_parameters(posterior)
  params <- colnames(posterior)

  # Get Priors
  if (is.null(prior)) {
    prior <- as.data.frame(lapply(params, function(p) {
      distribution_normal(nrow(posterior), mean = 0, sd = 2)
    }))
    colnames(prior) <- params
    if (verbose) {
      warning(
        "Prior not specified! ",
        "Please specify priors (with column order matching 'posterior', or by",
        " providing the model object as 'prior' argument)",
        " to get meaningful results. Using normally distributed priors with",
        " location=0 and scale=2 as default.", call. = FALSE
      )
    }
  } else if (insight::is_model(prior)) {
    prior <- .armsim_weakly_priors(prior, nrow(posterior))
  }


  # Get savage-dickey BFs
  bayesfactor_parameters.data.frame(
    posterior = posterior, prior = prior,
    direction = direction, null = null, ...
  )
}



#' @importFrom stats sd
#' @importFrom insight get_response get_data find_predictors find_parameters
.armsim_weakly_priors <- function(m, nsim) {
  y <- insight::get_response(m)
  d <- insight::get_data(m, effects = "fixed")
  pred <- insight::find_predictors(m, effects = "fixed", flatten = TRUE)

  scale.factor <- stats::sd(y, na.rm = TRUE)
  scale.b <- 2.5 * scale.factor
  scale.y <- 10 * scale.factor
  scale.pred <- NULL

  priors <- distribution_normal(nsim, mean = 0, sd = round(scale.y, 2))

  # we need to check which predictors are categorical and then "mimic"
  # their coefficient name as it is represented in the model (i.e. variable
  # name + category name)

  for (i in pred) {
    f <- d[[i]]
    if (is.factor(f)) {
      i <- sprintf("%s%s", i, levels(f)[2:nlevels(f)])
      scale.pred <- c(scale.pred, rep(scale.b, nlevels(f) - 1))
    } else {
      scale.pred <- c(scale.pred, scale.b / stats::sd(f, na.rm = TRUE))
    }
  }

  for (i in scale.pred) {
    priors <- cbind(priors, distribution_normal(nsim, mean = 0, sd = round(i, 2)))
  }

  priors <- as.data.frame(priors)
  colnames(priors) <- insight::find_parameters(m, effects = "fixed", flatten = TRUE)

  priors
}

