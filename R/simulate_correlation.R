#' Data Simulation
#'
#' Simulate data with specific characteristics.
#'
#' @param n The number of observations to be generated.
#' @param r A value or vector corresponding to the desired correlation coefficients.
#'
#' @examples
#'
#' # Correlation --------
#' data <- simulate_correlation(r = 0.5)
#' cor.test(data$V1, data$V2)
#'
#' # Specify mean and SD
#' data <- simulate_correlation(r = 0.5, n = 50, mean = c(0, 1), sd = c(0.7, 1.7))
#' cor.test(data$V1, data$V2)
#' round(c(mean(data$V1), sd(data$V1)), 1)
#' round(c(mean(data$V2), sd(data$V2)), 1)
#'
#' # Generate multiple variables
#' cor_matrix <- matrix(c(1.0, 0.2, 0.4,
#'                        0.2, 1.0, 0.3,
#'                        0.4, 0.3, 1.0),
#'                      nrow = 3)
#'
#' data <- simulate_correlation(r = cor_matrix)
#' cor(data)
#' @export
simulate_correlation <- function(n = 100, r = 0.5, mean = 0, sd = 1){

  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' required for this function to work. Please install it by running `install.packages('MASS')`.")
  }

  # Define matrix
  if(is.matrix(r)){
    if(isSymmetric(r)){
      if(any(r > 1)){
        stop("'r' should only contain values between -1 and 1.")
      } else{
        sigma <- r
      }
    } else{
      stop("'r' should be a symetric matrix (relative to the diagonal).")
    }
  } else if(length(r) == 1){
    if(abs(r) > 1){
      stop("'r' should only contain values between -1 and 1.")
    } else{
      sigma <- matrix(c(1, r, r, 1), nrow = 2)
    }
  } else{
    stop("'r' should be a value (e.g., r = 0.5) or a square matrix.")
  }


  # Get data
  data <- MASS::mvrnorm(n=n,
                        mu=rep_len(0, ncol(sigma)),  # Means of variables
                        Sigma=sigma,
                        empirical=TRUE)

  # Adjust scale
  if(any(sd != 1)){
    data <- t(t(data) * rep_len(sd, ncol(sigma)))
  }

  # Adjust mean
  if(any(mean != 0)){
    data <- t(t(data) + rep_len(mean, ncol(sigma)))
  }

  as.data.frame(data)
}
