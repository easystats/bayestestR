#' Orthonormal Contrast Matrices for Bayesian Estimation
#'
#' Returns a design or model matrix of orthonormal contrasts such that the
#' marginal prior on all effects is identical. Implementation from Singmann
#' \& Gronau's \href{https://github.com/bayesstuff/bfrms/}{\code{bfrms}},
#' following the description in Rouder, Morey, Speckman, \& Province (2012, p. 363).
#'
#' Though using this factor coding scheme might obscure the interpretation of
#' parameters, it is essential for correct estimation of Bayes factors for
#' contrasts and multi-level order restrictions. See info on specifying correct
#' priors for factors with more than 2 levels in
#' \href{https://easystats.github.io/bayestestR/articles/bayes_factors.html}{the Bayes factors vignette}.
#'
#' @param n a vector of levels for a factor, or the number of levels.
#' @param contrasts logical indicating whether contrasts should be computed.
#'
#' @references Rouder, J. N., Morey, R. D., Speckman, P. L., \& Province, J. M.
#'   (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical
#'   Psychology*, 56(5), 356-374. https://doi.org/10.1016/j.jmp.2012.08.001
#'
#' @return A \code{matrix} with n rows and k columns, with k=n-1 if contrasts is
#'   \code{TRUE} and k=n if contrasts is \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' contr.bayes(2) # Q_2 in Rouder et al. (2012, p. 363)
#' #            [,1]
#' # [1,] -0.7071068
#' # [2,]  0.7071068
#'
#' contr.bayes(5) # equivalent to Q_5 in Rouder et al. (2012, p. 363)
#' #            [,1]       [,2]       [,3]       [,4]
#' # [1,]  0.0000000  0.8944272  0.0000000  0.0000000
#' # [2,]  0.0000000 -0.2236068 -0.5000000  0.7071068
#' # [3,]  0.7071068 -0.2236068 -0.1666667 -0.4714045
#' # [4,] -0.7071068 -0.2236068 -0.1666667 -0.4714045
#' # [5,]  0.0000000 -0.2236068  0.8333333  0.2357023
#'
#' ## check decomposition
#' Q3 <- contr.bayes(3)
#' Q3 %*% t(Q3)
#' #            [,1]       [,2]       [,3]
#' # [1,]  0.6666667 -0.3333333 -0.3333333
#' # [2,] -0.3333333  0.6666667 -0.3333333
#' # [3,] -0.3333333 -0.3333333  0.6666667
#' ## 2/3 on diagonal and -1/3 on off-diagonal elements
#' }
#'
#' @export
contr.bayes <- function(n, contrasts = TRUE) {
  # validate n
  if (length(n) <= 1L) {
    if (is.numeric(n) && length(n) == 1L && n > 1L) {
      TRUE # all good
    } else {
      stop("not enough degrees of freedom to define contrasts")
    }
  } else {
    n <- length(n)
  }

  # make factor coding
  cont <- diag(n)
  if (contrasts) {
    a <- n
    I_a <- diag(a)
    J_a <- matrix(1, nrow = a, ncol = a)
    Sigma_a <- I_a - J_a / a
    cont <- eigen(Sigma_a)$vectors[, seq_len(a - 1), drop = FALSE]
  }
  cont
}
