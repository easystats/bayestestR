#' Sequential Effect eXistence and sIgnificance Testing (SEXIT)
#'
#' Returns the minimal (and optimal) required information to describe models' parameters under a Bayesian framework.
#'
#' @inheritParams p_direction
#'
#' @details
#' The assessment of "significance" (in its broadest meaning) is a pervasive issue in science, and its historical index, the p-value, has been strongly criticized and deemed to have played an important role in the replicability crisis. In reaction, more and more scientists have tuned to Bayesian methods, offering an alternative set of tools to answer their questions. However, the Bayesian framework offers a wide variety of possible indices related to "significance", and the debate has been raging about which index is the best, and which one to report.
#'
#' This situation can lead to the mindless reporting of all possible indices (with the hopes that with that the reader will be satisfied), but often without having the writer understanding and interpreting them. It is indeed complicated to juggle between many indices with complicated definitions and subtle differences.
#'
#' SEXIT aims at offering a practical framework for Bayesian effects reporting, in which the focus is put on intuitiveness, explicitness and usefulness of the indices' interpretation. To that end, we suggest a system of description of parameters that would be intuitive, easy to learn and apply, mathematically accurate and useful for taking decision.
#'
#' Once the thresholds for significance (i.e., the ROPE) and the one for a "large" effect are explicitly defined, the SEXIT framework does not make any interpretation, i.e., it does not label the effects, but just sequentially gives 3 probabilities (of direction, of significance and of being large, respectively) as-is on top of the characteristics of the posterior (using the median and HDI for centrality and uncertainty description). Thus, it provides a lot of information about the posterior distribution (through the mass of different 'sections' of the posterior) in a clear and meaningful way.
#'
#' @return A dataframe.
#'
#' @references \itemize{
#'   \item{Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019). bayestestR: Describing Effects and their Uncertainty, Existence and Significance within the Bayesian Framework. Journal of Open Source Software, 4(40), 1541. \doi{10.21105/joss.01541}}
#'   \item{Makowski D, Ben-Shachar MS, Chen SHA, Lüdecke D (2019) Indices of Effect Existence and Significance in the Bayesian Framework. Frontiers in Psychology 2019;10:2767. \doi{10.3389/fpsyg.2019.02767}}
#' }
#'
#' @examples
#' library(bayestestR)
#'
#' x <- rnorm(1000, -1, 1)
#' s <- sexit(x)
#' s
#' print(s, summary=TRUE)
#'
#' @export
sexit <- function(x, ...) {
  UseMethod("sexit")
}



#' @export
print.sexit <- function(x, summary=FALSE, digits=2, ...) {
  orig_x <- x
  insight::print_color(paste0("# ", attributes(x)$sexit_thresholds, "\n\n"), "blue")

  if(isFALSE(summary)){
    insight::print_color(attributes(x)$sexit_textlong, "yellow")
    cat("\n\n")
    df <- cbind(
      data.frame(Median = x$Median,
                 CI = insight::format_ci(x$CI_low, x$CI_high, NULL)),
      x[, 5:ncol(x)])
    names(df) <- attributes(x)$pretty_cols
    print_data_frame(df, digits=digits, ...)
  } else{
    cat(attributes(x)$sexit_textshort)
  }

  invisible(orig_x)
}



#' @keywords internal
.sexit_preprocess <- function(x, significant="default", large="default", ...) {

  thresholds <- sexit_thresholds(x)
  if(significant == "default") significant <- thresholds[1]
  if(large == "default") large <- thresholds[2]

  suppressWarnings(resp <- insight::get_response(x))
  suppressWarnings(info <- insight::model_info(x))
  if(!is.null(resp) && !is.null(info) && info$is_linear){
    sd1 <- significant / sd(resp, na.rm=TRUE)
    sd2 <- large / sd(resp, na.rm=TRUE)
    text_sd <- paste0(" (corresponding respectively to ",
                      insight::format_value(sd1),
                      " and ",
                      insight::format_value(sd2),
                      " of the outcome's SD)")
  } else{
    text_sd <- ""
  }

  thresholds <- paste0("The thresholds beyond which the effect is considered ",
                           "as significant (i.e., non-negligible) and large are ",
                           insight::format_value(significant),
                           " and ",
                           insight::format_value(large),
                           text_sd,
                           ".")

  list(significant = significant, large = large, text = thresholds)
}



#' @rdname sexit
#' @export
sexit.numeric <- function(x, significant="default", large="default", ci=0.95, ...) {

  thresholds <- .sexit_preprocess(x, significant, large, ...)
  significant <- thresholds$significant
  large <- thresholds$large
  thresholds_text <- thresholds$text

  # Description
  centrality <- point_estimate(x, "median")$Median
  direction <- ifelse(centrality < 0, "negative", "positive")
  centrality_text <- paste0("Median = ", insight::format_value(centrality))
  uncertainty <- ci(x, ci=ci, method="HDI", ...)
  uncertainty_text <- insight::format_ci(uncertainty$CI_low, uncertainty$CI_high, uncertainty$CI / 100)

  # Indices
  existence_rez <- p_direction(x, ...)
  existence_value <- insight::format_value(existence_rez, as_percent=TRUE)
  existence_threshold <- ifelse(direction == "negative", "< 0", "> 0")

  sig_rez <- p_significance(x, threshold=significant, ...)
  sig_value <- insight::format_value(sig_rez, as_percent=TRUE)
  if(direction == "negative") attributes(sig_rez)$threshold <- -1 * significant
  sig_threshold <- paste0(ifelse(direction == "negative", "< ", "> "), attributes(sig_rez)$threshold)


  large_rez <- p_significance(x, threshold=large, ...)
  large_value <- insight::format_value(large_rez, as_percent=TRUE)
  if(direction == "negative") attributes(large)$threshold <- -1 * large_rez
  large_threshold <- paste0(ifelse(direction == "negative", "< ", "> "), attributes(large_rez)$threshold)



  text_full <- paste0("The effect (",
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
                 ")")

  text_short <- paste0("The effect (",
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
                 ")")

  out <- cbind(
    data.frame(Median = centrality),
    as.data.frame(uncertainty),
    data.frame(Existence = existence_rez),
    data.frame(Significance = sig_rez),
    data.frame(Large = large_rez))

  # Prepare output
  attr(out, "sexit_significance") <- attributes(sig_rez)$threshold
  attr(out, "sexit_large") <- attributes(large_rez)$threshold
  attr(out, "sexit_textlong") <- text_full
  attr(out, "sexit_textshort") <- text_short
  attr(out, "sexit_thresholds") <- thresholds_text
  attr(out, "pretty_cols") <- c("Median",
                                paste0(insight::format_value(ci*100, protect_integers =TRUE), "% CI"),
                                paste0("Existence (", existence_threshold, ")"),
                                paste0("Significance (", sig_threshold, ")"),
                                paste0("Large (", large_threshold, ")"))
  attr(out, "data") <- x

  class(out) <- unique(c("sexit", "see_sexit", class(out)))

  out
}