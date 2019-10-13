#' Convert (refit) a Bayesian model to frequentist
#'
#' Refit Bayesian model as frequentist. Can be useful for comparisons.
#'
#' @param model A Bayesian model.
#' @param data Data used by the model. If \code{NULL}, will try to extract it from the model.
#' @examples
#' \donttest{
#' # Rstanarm ----------------------
#' library(rstanarm)
#'
#' # Simple regressions
#' model <- stan_glm(Sepal.Length ~ Petal.Length * Species,
#'                   data = iris, chains = 2, refresh = 0)
#' bayesian_as_frequentist(model)
#'
#' model <- stan_glm(vs ~ mpg, family = "binomial",
#'                   data = mtcars, chains = 2, refresh = 0)
#' bayesian_as_frequentist(model)
#'
#' # Mixed models
#' model <- stan_glmer(Sepal.Length ~ Petal.Length + (1|Species),
#'                     data = iris, chains = 2, refresh = 0)
#' bayesian_as_frequentist(model)
#'
#' model <- stan_glmer(vs ~ mpg + (1|cyl), family = "binomial",
#'                     data = mtcars, chains = 2, refresh = 0)
#' bayesian_as_frequentist(model)
#' }
#'
#' @importFrom stats lm glm
#' @export
convert_bayesian_as_frequentist <- function(model, data = NULL){

  if(is.null(data)){
    data <- insight::get_data(model)
  }

  # info
  info <- insight::model_info(model)

  # Call
  called <- model$call
  # fun <- as.character(called)[1]
  formula <- called$formula
  family <- called$family

  if(info$is_mixed){
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
    }
    if(info$is_linear){
      freq <- lme4::lmer(formula, data = data)
    } else{
      freq <- lme4::glmer(formula, data = data, family = family)
    }
  } else{
    if(info$is_linear){
      freq <- lm(formula, data = data)
    } else{
      freq <- glm(formula, data = data, family = family)
    }
  }

  freq
}


#' @rdname convert_bayesian_as_frequentist
#' @export
bayesian_as_frequentist <- convert_bayesian_as_frequentist
