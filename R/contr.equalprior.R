#' Contrast Matrices for Equal Marginal Priors in Bayesian Estimation
#'
#' Build contrasts for factors with equal marginal priors on all levels. The 3
#' functions give the same orthogonal contrasts, but are scaled differently to
#' allow different prior specifications (see 'Details'). Implementation from
#' Singmann & Gronau's [`bfrms`](https://github.com/bayesstuff/bfrms/),
#' following the description in Rouder, Morey, Speckman, & Province (2012, p.
#' 363).
#'
#' @inheritParams stats::contr.treatment
#'
#' @details
#' When using [`stats::contr.treatment`], each dummy variable is the difference
#' between each level and the reference level. While this is useful if setting
#' different priors for each coefficient, it should not be used if one is trying
#' to set a general prior for differences between means, as it (as well as
#' [`stats::contr.sum`] and others) results in unequal marginal priors on the
#' means the the difference between them.
#'
#' ```
#' library(brms)
#'
#' data <- data.frame(
#'   group = factor(rep(LETTERS[1:4], each = 3)),
#'   y = rnorm(12)
#' )
#'
#' contrasts(data$group) # R's default contr.treatment
#' #>   B C D
#' #> A 0 0 0
#' #> B 1 0 0
#' #> C 0 1 0
#' #> D 0 0 1
#'
#' model_prior <- brm(
#'   y ~ group, data = data,
#'   sample_prior = "only",
#'   # Set the same priors on the 3 dummy variable
#'   # (Using an arbitrary scale)
#'   prior = set_prior("normal(0, 10)", coef = c("groupB", "groupC", "groupD"))
#' )
#'
#' est <- emmeans::emmeans(model_prior, pairwise ~ group)
#'
#' point_estimate(est, centr = "mean", disp = TRUE)
#' #> Point Estimate
#' #>
#' #> Parameter |  Mean |    SD
#' #> -------------------------
#' #> A         | -0.01 |  6.35
#' #> B         | -0.10 |  9.59
#' #> C         |  0.11 |  9.55
#' #> D         | -0.16 |  9.52
#' #> A - B     |  0.10 |  9.94
#' #> A - C     | -0.12 |  9.96
#' #> A - D     |  0.15 |  9.87
#' #> B - C     | -0.22 | 14.38
#' #> B - D     |  0.05 | 14.14
#' #> C - D     |  0.27 | 14.00
#' ```
#'
#' We can see that the priors for means aren't all the same (`A` having a more
#' narrow prior), and likewise for the pairwise differences (priors for
#' differences from `A` are more narrow).
#'
#' The solution is to use one of the methods provided here, which *do* result in
#' marginally equal priors on means differences between them. Though this will
#' obscure the interpretation of parameters, setting equal priors on means and
#' differences is important for they are useful for specifying equal priors on
#' all means in a factor and their differences correct estimation of Bayes
#' factors for contrasts and order restrictions of multi-level factors (where
#' `k>2`). See info on specifying correct priors for factors with more than 2
#' levels in [the Bayes factors vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html).
#'
#' ***NOTE:*** When setting priors on these dummy variables, always:
#' 1. Use priors that are **centered on 0**! Other location/centered priors are meaningless!
#' 2. Use **identically-scaled priors** on all the dummy variables of a single factor!
#'
#' `contr.equalprior` returns the original orthogonal-normal contrasts as
#' described in Rouder, Morey, Speckman, & Province (2012, p. 363). Setting
#' `contrasts = FALSE` returns the \eqn{I_{n} - \frac{1}{n}} matrix.
#'
#' ## `contr.equalprior_pairs`
#'
#' Useful for setting priors in terms of pairwise differences between means -
#' the scales of the priors defines the prior distribution of the pair-wise
#' differences between all pairwise differences (e.g., `A - B`, `B - C`, etc.).
#'
#' ```
#' contrasts(data$group) <- contr.equalprior_pairs
#' contrasts(data$group)
#' #>         [,1]       [,2]       [,3]
#' #> A  0.0000000  0.6123724  0.0000000
#' #> B -0.1893048 -0.2041241  0.5454329
#' #> C -0.3777063 -0.2041241 -0.4366592
#' #> D  0.5670111 -0.2041241 -0.1087736
#'
#' model_prior <- brm(
#'   y ~ group, data = data,
#'   sample_prior = "only",
#'   # Set the same priors on the 3 dummy variable
#'   # (Using an arbitrary scale)
#'   prior = set_prior("normal(0, 10)", coef = c("group1", "group2", "group3"))
#' )
#'
#' est <- emmeans(model_prior, pairwise ~ group)
#'
#' point_estimate(est, centr = "mean", disp = TRUE)
#' #> Point Estimate
#' #>
#' #> Parameter |  Mean |    SD
#' #> -------------------------
#' #> A         | -0.31 |  7.46
#' #> B         | -0.24 |  7.47
#' #> C         | -0.34 |  7.50
#' #> D         | -0.30 |  7.25
#' #> A - B     | -0.08 | 10.00
#' #> A - C     |  0.03 | 10.03
#' #> A - D     | -0.01 |  9.85
#' #> B - C     |  0.10 | 10.28
#' #> B - D     |  0.06 |  9.94
#' #> C - D     | -0.04 | 10.18
#' ```
#'
#' All means have the same prior distribution, and the distribution of the
#' differences matches the prior we set of `"normal(0, 10)"`. Success!
#'
#' ## `contr.equalprior_deviations`
#'
#' Useful for setting priors in terms of the deviations of each mean from the
#' grand mean - the scales of the priors defines the prior distribution of the
#' distance (above, below) the mean of one of the levels might have from the
#' overall mean. (See examples.)
#'
#'
#' @references
#' Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012).
#' Default Bayes factors for ANOVA designs. *Journal of Mathematical
#' Psychology*, 56(5), 356-374. https://doi.org/10.1016/j.jmp.2012.08.001
#'
#' @return A `matrix` with n rows and k columns, with k=n-1 if contrasts is
#'   `TRUE` and k=n if contrasts is `FALSE`.
#'
#' @aliases contr.bayes contr.orthonorm
#'
#' @examples
#' contr.equalprior(2) # Q_2 in Rouder et al. (2012, p. 363)
#'
#' contr.equalprior(5) # equivalent to Q_5 in Rouder et al. (2012, p. 363)
#'
#' ## check decomposition
#' Q3 <- contr.equalprior(3)
#' Q3 %*% t(Q3) ## 2/3 on diagonal and -1/3 on off-diagonal elements
#' @export
contr.equalprior <- function(n, contrasts = TRUE, sparse = FALSE) {
  contr <- stats::contr.treatment(n,
    contrasts = FALSE, base = 1,
    sparse = sparse & !contrasts
  )

  k <- nrow(contr)
  contr <- contr - 1/k

  if (contrasts) {
    contr <- eigen(contr)$vectors[, seq_len(k - 1), drop = FALSE]
  }

  contr
}

#' @export
#' @rdname contr.equalprior
contr.equalprior_pairs <- function (n, contrasts = TRUE, sparse = FALSE) {
  contr <- contr.equalprior(n, contrasts, sparse) / sqrt(2)
}

#' @export
#' @rdname contr.equalprior
contr.equalprior_deviations <- function (n, contrasts = TRUE, sparse = FALSE) {
  contr <- contr.equalprior(n, contrasts, sparse)
  k <- nrow(contr)

  r <- -1 / (k - 1)
  V <- 1 - 1 / k
  VCOV <- matrix(r * V, k, k)
  diag(VCOV) <- V
  wts <- c(1 - 1 / k, rep(-1 / k, k - 1))
  scale <- as.vector(sqrt(wts %*% VCOV %*% wts))

  contr / scale
}



# OLD ------------------------------

#' @export
contr.orthonorm <- function(n, contrasts = TRUE) {
  .Deprecated(new = "contr.equalprior", old = "contr.orthonorm")
  contr.equalprior(n, contrasts = contrasts)
}

#' @export
contr.bayes <- function(n, contrasts = TRUE) {
  .Deprecated(new = "contr.equalprior", old = "contr.bayes")
  contr.equalprior(n, contrasts = contrasts)
}
