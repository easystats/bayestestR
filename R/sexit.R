#' Sequential Effect eXistence and sIgnificance Testing (SEXIT)
#'
#' The SEXIT is a new framework to describe Bayesian effects, guiding which
#' indices to use. Accordingly, the `sexit()` function returns the minimal (and
#' optimal) required information to describe models' parameters under a Bayesian
#' framework. It includes the following indices:
#' \itemize{
#'   \item{Centrality: the median of the posterior distribution. In
#'   probabilistic terms, there is `50%` of probability that the effect is higher
#'   and lower. See [`point_estimate()`][point_estimate].}
#'   \item{Uncertainty: the `95%` Highest Density Interval (HDI). In
#'   probabilistic terms, there is `95%` of probability that the effect is
#'   within this confidence interval. See [`ci()`][ci].}
#'   \item{Existence: The probability of direction allows to quantify the
#'   certainty by which an effect is positive or negative. It is a critical
#'   index to show that an effect of some manipulation is not harmful (for
#'   instance in clinical studies) or to assess the direction of a link. See
#'   [`p_direction()`][p_direction].}
#'   \item{Significance: Once existence is demonstrated with high certainty, we
#'   can assess whether the effect is of sufficient size to be considered as
#'   significant (i.e., not negligible). This is a useful index to determine
#'   which effects are actually important and worthy of discussion in a given
#'   process. See [`p_significance()`][p_significance].}
#'   \item{Size: Finally, this index gives an idea about the strength of an
#'   effect. However, beware, as studies have shown that a big effect size can
#'   be also suggestive of low statistical power (see details section).}
#' }
#'
#' @inheritParams p_direction
#' @inheritParams hdi
#' @param significant,large The threshold values to use for significant and
#'   large probabilities. If left to 'default', will be selected through
#'   [`sexit_thresholds()`][sexit_thresholds]. See the details section below.
#'
#' @details
#'
#' \subsection{Rationale}{
#' The assessment of "significance" (in its broadest meaning) is a pervasive
#' issue in science, and its historical index, the p-value, has been strongly
#' criticized and deemed to have played an important role in the replicability
#' crisis. In reaction, more and more scientists have tuned to Bayesian methods,
#' offering an alternative set of tools to answer their questions. However, the
#' Bayesian framework offers a wide variety of possible indices related to
#' "significance", and the debate has been raging about which index is the best,
#' and which one to report.
#'
#' This situation can lead to the mindless reporting of all possible indices
#' (with the hopes that with that the reader will be satisfied), but often
#' without having the writer understanding and interpreting them. It is indeed
#' complicated to juggle between many indices with complicated definitions and
#' subtle differences.
#'
#' SEXIT aims at offering a practical framework for Bayesian effects reporting,
#' in which the focus is put on intuitiveness, explicitness and usefulness of
#' the indices' interpretation. To that end, we suggest a system of description
#' of parameters that would be intuitive, easy to learn and apply,
#' mathematically accurate and useful for taking decision.
#'
#' Once the thresholds for significance (i.e., the ROPE) and the one for a
#' "large" effect are explicitly defined, the SEXIT framework does not make any
#' interpretation, i.e., it does not label the effects, but just sequentially
#' gives 3 probabilities (of direction, of significance and of being large,
#' respectively) as-is on top of the characteristics of the posterior (using the
#' median and HDI for centrality and uncertainty description). Thus, it provides
#' a lot of information about the posterior distribution (through the mass of
#' different 'sections' of the posterior) in a clear and meaningful way.
#' }
#'
#' \subsection{Threshold selection}{
#'  One of the most important thing about the SEXIT framework is that it relies
#'  on two "arbitrary" thresholds (i.e., that have no absolute meaning). They
#'  are the ones related to effect size (an inherently subjective notion),
#'  namely the thresholds for significant and large effects. They are set, by
#'  default, to `0.05` and `0.3` of the standard deviation of the outcome
#'  variable (tiny and large effect sizes for correlations according to Funder
#'  and Ozer, 2019). However, these defaults were chosen by lack of a better
#'  option, and might not be adapted to your case. Thus, they are to be handled
#'  with care, and the chosen thresholds should always be explicitly reported
#'  and justified.
#'   \itemize{
#'     \item For **linear models (lm)**, this can be generalised to \ifelse{html}{\out{0.05 * SD<sub>y</sub>}}{\eqn{[0.05*SD_{y}]}} and \ifelse{html}{\out{0.3 * SD<sub>y</sub>}}{\eqn{[0.3*SD_{y}]}} for significant and large effects, respectively.
#'     \item For **logistic models**, the parameters expressed in log odds ratio can be converted to standardized difference through the formula \ifelse{html}{\out{&pi;/&radic;(3)}}{\eqn{\pi/\sqrt{3}}}, resulting a threshold of `0.09` and `0.54`.
#'     \item For other models with **binary outcome**, it is strongly recommended to manually specify the rope argument. Currently, the same default is applied that for logistic models.
#'     \item For models from **count data**, the residual variance is used. This is a rather experimental threshold and is probably often similar to `0.05` and `0.3`, but should be used with care!
#'     \item For **t-tests**, the standard deviation of the response is used, similarly to linear models (see above).
#'     \item For **correlations**,`0.05` and `0.3` are used.
#'     \item For all other models, `0.05` and `0.3` are used, but it is strongly advised to specify it manually.
#'   }
#' }
#' \subsection{Examples}{
#' The three values for existence, significance and size provide a useful description of the posterior distribution of the effects. Some possible scenarios include:
#' \itemize{
#'   \item{The probability of existence is low, but the probability of being large is high: it suggests that the posterior is very wide (covering large territories on both side of 0). The statistical power might be too low, which should warrant any confident conclusion.}
#'   \item{The probability of existence and significance is high, but the probability of being large is very small: it suggests that the effect is, with high confidence, not large (the posterior is mostly contained between the significance and the large thresholds).}
#'   \item{The 3 indices are very low: this suggests that the effect is null with high confidence (the posterior is closely centred around 0).}}}
#'
#' @return A dataframe and text as attribute.
#'
#' @references \itemize{
#'   \item{Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019). bayestestR: Describing Effects and their Uncertainty, Existence and Significance within the Bayesian Framework. Journal of Open Source Software, 4(40), 1541. \doi{10.21105/joss.01541}}
#'   \item{Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of Effect Existence and Significance in the Bayesian Framework. Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}}
#' }
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#'
#' s <- sexit(rnorm(1000, -1, 1))
#' s
#' print(s, summary = TRUE)
#'
#' s <- sexit(iris)
#' s
#' print(s, summary = TRUE)
#'
#' if (require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt * cyl,
#'     data = mtcars,
#'     iter = 400, refresh = 0
#'   )
#'   s <- sexit(model)
#'   s
#'   print(s, summary = TRUE)
#' }
#' }
#' @export
sexit <- function(x, significant = "default", large = "default", ci = 0.95, ...) {
  thresholds <- .sexit_preprocess(x, significant, large, ...)
  significant <- thresholds$significant
  large <- thresholds$large
  thresholds_text <- thresholds$text

  # Description
  centrality <- point_estimate(x, "median")
  centrality$Effects <- centrality$Component <- NULL
  centrality_text <- paste0("Median = ", insight::format_value(centrality$Median))
  direction <- ifelse(centrality$Median < 0, "negative", "positive")
  uncertainty <- ci(x, ci = ci, method = "ETI", ...)[c("CI", "CI_low", "CI_high")]
  uncertainty_text <- insight::format_ci(uncertainty$CI_low, uncertainty$CI_high, uncertainty$CI)

  # Indices
  existence_rez <- as.numeric(p_direction(x, ...))
  existence_value <- insight::format_value(existence_rez, as_percent = TRUE)
  existence_threshold <- ifelse(direction == "negative", "< 0", "> 0")

  sig_rez <- as.numeric(p_significance(x, threshold = significant, ...))
  sig_value <- insight::format_value(sig_rez, as_percent = TRUE)
  sig_threshold <- ifelse(direction == "negative", -1 * significant, significant)
  sig_threshold <- paste0(ifelse(direction == "negative", "< ", "> "), insight::format_value(sig_threshold))


  large_rez <- as.numeric(p_significance(x, threshold = large, ...))
  large_value <- insight::format_value(large_rez, as_percent = TRUE)
  large_threshold <- ifelse(direction == "negative", -1 * large, large)
  large_threshold <- paste0(ifelse(direction == "negative", "< ", "> "), insight::format_value(large_threshold))

  if ("Parameter" %in% names(centrality)) {
    parameters <- centrality$Parameter
  } else {
    parameters <- "The effect"
  }

  text_full <- paste0(
    parameters,
    " (",
    centrality_text,
    ", ",
    uncertainty_text,
    ") has a ",
    existence_value,
    " probability of being ",
    direction,
    " (",
    existence_threshold,
    "), ",
    sig_value,
    " of being significant (",
    sig_threshold,
    "), and ",
    large_value,
    " of being large (",
    large_threshold,
    ")"
  )

  text_short <- paste0(
    parameters,
    " (",
    centrality_text,
    ", ",
    uncertainty_text,
    ") has ",
    existence_value,
    ", ",
    sig_value,
    " and ",
    large_value,
    " probability of being ",
    direction,
    " (",
    existence_threshold,
    "), significant (",
    sig_threshold,
    ") and large (",
    large_threshold,
    ")"
  )

  out <- cbind(
    centrality,
    as.data.frame(uncertainty),
    data.frame(Direction = existence_rez),
    data.frame(Significance = sig_rez),
    data.frame(Large = large_rez)
  )

  # Prepare output
  attr(out, "sexit_info") <- "Following the Sequential Effect eXistence and sIgnificance Testing (SEXIT) framework, we report the median of the posterior distribution and its 95% CI (Highest Density Interval), along the probability of direction (pd), the probability of significance and the probability of being large."
  attr(out, "sexit_ci_method") <- "ETI"
  attr(out, "sexit_significance") <- significant
  attr(out, "sexit_large") <- large
  attr(out, "sexit_textlong") <- text_full
  attr(out, "sexit_textshort") <- text_short
  attr(out, "sexit_thresholds") <- thresholds_text
  pretty_cols <- c(
    "Median",
    paste0(insight::format_value(ci * 100, protect_integers = TRUE), "% CI"),
    "Direction",
    paste0("Significance (> |", insight::format_value(significant), "|)"),
    paste0("Large (> |", insight::format_value(large), "|)")
  )
  if ("Parameter" %in% names(out)) pretty_cols <- c("Parameter", pretty_cols)
  attr(out, "pretty_cols") <- pretty_cols
  attr(out, "data") <- x

  class(out) <- unique(c("sexit", "see_sexit", class(out)))

  out
}

















#' @keywords internal
.sexit_preprocess <- function(x, significant = "default", large = "default", ...) {
  thresholds <- sexit_thresholds(x)
  if (significant == "default") significant <- thresholds[1]
  if (large == "default") large <- thresholds[2]



  suppressWarnings(resp <- tryCatch(insight::get_response(x), error = function(e) NULL))
  suppressWarnings(info <- tryCatch(insight::model_info(x), error = function(e) NULL))
  if (!is.null(resp) && !is.null(info) && info$is_linear) {
    sd1 <- significant / stats::sd(resp, na.rm = TRUE)
    sd2 <- large / stats::sd(resp, na.rm = TRUE)
    text_sd <- paste0(
      " (corresponding respectively to ",
      insight::format_value(sd1),
      " and ",
      insight::format_value(sd2),
      " of the outcome's SD)"
    )
  } else {
    text_sd <- ""
  }

  thresholds <- paste0(
    "The thresholds beyond which the effect is considered ",
    "as significant (i.e., non-negligible) and large are |",
    insight::format_value(significant),
    "| and |",
    insight::format_value(large),
    "|",
    text_sd,
    "."
  )

  list(significant = significant, large = large, text = thresholds)
}



#' @export
print.sexit <- function(x, summary = FALSE, digits = 2, ...) {
  orig_x <- x

  # Long
  if (isFALSE(summary)) {
    insight::print_color(paste0("# ", attributes(x)$sexit_info, " ", attributes(x)$sexit_thresholds, "\n\n"), "blue")

    text <- attributes(x)$sexit_textlong
    if (length(text) > 1) text <- paste0(paste0("- ", text), collapse = "\n")
    insight::print_color(text, "yellow")
    cat("\n\n")

    df <- data.frame(Median = x$Median, CI = insight::format_ci(x$CI_low, x$CI_high, NULL))
    if ("Parameter" %in% names(x)) {
      df <- cbind(data.frame(Parameter = x$Parameter), df, x[c("Direction", "Significance", "Large")])
    } else {
      df <- cbind(df, x[c("Direction", "Significance", "Large")])
    }
    names(df) <- attributes(x)$pretty_cols
    print_data_frame(df, digits = digits, ...)

    # Short
  } else {
    insight::print_color(paste0("# ", attributes(x)$sexit_thresholds, "\n\n"), "blue")

    text <- attributes(x)$sexit_textshort
    if (length(text) > 1) text <- paste0(paste0("- ", text), collapse = "\n")
    cat(text)
  }

  invisible(orig_x)
}
