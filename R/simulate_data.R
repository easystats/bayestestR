#' Data Simulation
#'
#' Simulate data with specific characteristics.
#'
#' @param n The number of observations to be generated.
#' @param r A value or vector corresponding to the desired correlation
#'   coefficients.
#' @param d A value or vector corresponding to the desired difference between
#'   the groups.
#' @param mean A value or vector corresponding to the mean of the variables.
#' @param sd A value or vector corresponding to the SD of the variables.
#' @param names A character vector of desired variable names.
#' @param ... Arguments passed to or from other methods.
#' @examplesIf requireNamespace("MASS", quietly = TRUE)
#'
#' # Correlation --------------------------------
#' data <- simulate_correlation(r = 0.5)
#' plot(data$V1, data$V2)
#' cor.test(data$V1, data$V2)
#' summary(lm(V2 ~ V1, data = data))
#'
#' # Specify mean and SD
#' data <- simulate_correlation(r = 0.5, n = 50, mean = c(0, 1), sd = c(0.7, 1.7))
#' cor.test(data$V1, data$V2)
#' round(c(mean(data$V1), sd(data$V1)), 1)
#' round(c(mean(data$V2), sd(data$V2)), 1)
#' summary(lm(V2 ~ V1, data = data))
#'
#' # Generate multiple variables
#' cor_matrix <- matrix(
#'   c(
#'     1.0, 0.2, 0.4,
#'     0.2, 1.0, 0.3,
#'     0.4, 0.3, 1.0
#'   ),
#'   nrow = 3
#' )
#'
#' data <- simulate_correlation(r = cor_matrix, names = c("y", "x1", "x2"))
#' cor(data)
#' summary(lm(y ~ x1, data = data))
#'
#' # t-test --------------------------------
#' data <- simulate_ttest(n = 30, d = 0.3)
#' plot(data$V1, data$V0)
#' round(c(mean(data$V1), sd(data$V1)), 1)
#' diff(t.test(data$V1 ~ data$V0)$estimate)
#' summary(lm(V1 ~ V0, data = data))
#' summary(glm(V0 ~ V1, data = data, family = "binomial"))
#'
#' # Difference --------------------------------
#' data <- simulate_difference(n = 30, d = 0.3)
#' plot(data$V1, data$V0)
#' round(c(mean(data$V1), sd(data$V1)), 1)
#' diff(t.test(data$V1 ~ data$V0)$estimate)
#' summary(lm(V1 ~ V0, data = data))
#' summary(glm(V0 ~ V1, data = data, family = "binomial"))
#' @export
simulate_correlation <- function(n = 100,
                                 r = 0.5,
                                 mean = 0,
                                 sd = 1,
                                 names = NULL,
                                 ...) {
  insight::check_if_installed("MASS")

  # Define matrix
  if (is.matrix(r)) {
    if (isSymmetric(r)) {
      if (any(r > 1)) {
        insight::format_error("`r` should only contain values between -1 and 1.")
      } else {
        dispersion <- r
      }
    } else {
      insight::format_error("`r` should be a symetric matrix (relative to the diagonal).")
    }
  } else if (length(r) == 1L) {
    if (abs(r) > 1) {
      insight::format_error("`r` should only contain values between -1 and 1.")
    } else {
      dispersion <- matrix(c(1, r, r, 1), nrow = 2)
    }
  } else {
    insight::format_error("`r` should be a value (e.g., r = 0.5) or a square matrix.")
  }


  # Get data
  out <- MASS::mvrnorm(
    n = n,
    mu = rep_len(0, ncol(dispersion)), # Means of variables
    Sigma = dispersion,
    empirical = TRUE
  )

  # Adjust scale
  if (any(sd != 1)) {
    out <- t(t(out) * rep_len(sd, ncol(dispersion)))
  }

  # Adjust mean
  if (any(mean != 0)) {
    out <- t(t(out) + rep_len(mean, ncol(dispersion)))
  }

  out <- as.data.frame(out)

  # Rename
  if (!is.null(names) && length(names) == ncol(out)) {
    names(out) <- names
  }
  out
}



#' @rdname simulate_correlation
#' @export
simulate_ttest <- function(n = 100, d = 0.5, names = NULL, ...) {
  x <- distribution_normal(n, 0, 1) # Continuous variables
  z <- 0 + d * x # Linear combination
  pr <- 1 / (1 + exp(-z)) # Pass it through an inverse logit function
  y <- distribution_binomial(n, 1, pr, random = 3) # Bernoulli response variable

  out <- data.frame(y = as.factor(y), x = x)
  names(out) <- paste0("V", 0:(ncol(out) - 1))

  if (!is.null(names) && length(names) == ncol(out)) {
    names(out) <- names
  }
  out
}


#' @rdname simulate_correlation
#' @export
simulate_difference <- function(n = 100, d = 0.5, names = NULL, ...) {
  x <- distribution_normal(round(n / 2), -d / 2, 1)
  y <- distribution_normal(round(n / 2), d / 2, 1)

  out <- data.frame(
    y = as.factor(rep(c(0, 1), each = round(n / 2))),
    x = c(x, y)
  )
  names(out) <- paste0("V", 0:(ncol(out) - 1))

  if (!is.null(names) && length(names) == ncol(out)) {
    names(out) <- names
  }
  out
}


# Simulate regression: see https://stats.stackexchange.com/questions/363623/simulate-regression-with-specified-standardized-coefficients/508107#508107
