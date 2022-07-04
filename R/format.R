#' @export
format.describe_posterior <- function(x,
                                      cp,
                                      digits = 2,
                                      format = "text",
                                      ci_string = "CI",
                                      caption = NULL,
                                      subtitles = NULL,
                                      ...) {
  # reshape CI
  if (is.data.frame(x) && insight::n_unique(x$CI) > 1) {
    att <- attributes(x)
    x <- datawizard::reshape_ci(x)
    attributes(x) <- utils::modifyList(att, attributes(x))
  }

  # format columns and values of data frame
  out <- insight::format_table(x, digits = digits, format = format, ...)

  # different CI-types as column names?
  if (ci_string != "CI" && any(grepl("CI$", colnames(out)))) {
    colnames(out) <- gsub("(.*)CI$", paste0("\\1", ci_string), colnames(out))
  }

  # match and split at components
  if (!is.null(cp) && !all(is.na(match(cp$Parameter, out$Parameter)))) {
    out <- insight::print_parameters(
      cp,
      out,
      keep_parameter_column = FALSE,
      remove_empty_column = TRUE,
      titles = caption,
      subtitles = subtitles,
      format = format
    )
  } else {
    attr(out, "table_caption") <- caption
    attr(out, "table_subtitle") <- subtitles
  }
  out
}


#' @export
format.point_estimate <- format.describe_posterior

#' @export
format.p_rope <- format.describe_posterior

#' @export
format.p_direction <- format.describe_posterior

#' @export
format.p_map <- format.describe_posterior

#' @export
format.map_estimate <- format.describe_posterior

#' @export
format.p_significance <- format.describe_posterior

#' @export
format.bayestestR_hdi <- format.describe_posterior

#' @export
format.bayestestR_eti <- format.describe_posterior

#' @export
format.bayestestR_si <- format.describe_posterior




# special handling for bayes factors ------------------


#' @export
format.bayesfactor_models <- function(x,
                                      digits = 3,
                                      log = FALSE,
                                      show_names = TRUE,
                                      format = "text",
                                      caption = NULL,
                                      exact = TRUE,
                                      ...) {
  BFE <- x
  denominator <- attr(BFE, "denominator")
  grid.type <- attr(BFE, "BF_method")
  model_names <- attr(BFE, "model_names")
  formula_length <- attr(BFE, "text_length")

  BFE <- as.data.frame(BFE)
  BFE$log_BF <- as.numeric(x, log = log)
  BFE$BF <- insight::format_bf(abs(BFE$log_BF), name = NULL, exact = exact, ...)

  if (any((sgn <- sign(BFE$log_BF) < 0)[!is.na(BFE$log_BF)])) {
    BFE$BF[sgn] <- paste0("-", BFE$BF[sgn])
  }

  BFE$Model[BFE$Model == "1"] <- "(Intercept only)" # indicate null-model

  # shorten model formulas?
  if (!is.null(formula_length) && !is.null(BFE$Model)) {
    BFE$Model <- insight::format_string(BFE$Model, length = formula_length)
  }

  if (isFALSE(show_names) || is.null(model_names) || length(model_names) != nrow(BFE)) {
    BFE$i <- paste0("[", seq_len(nrow(BFE)), "]")
  } else {
    BFE$i <- paste0("[", model_names, "]")
  }

  # Denominator
  denM <- insight::trim_ws(paste0(BFE$i, " ", BFE$Model)[denominator])
  BFE <- BFE[-denominator, ]
  BFE <- BFE[c("i", "Model", "BF")]
  colnames(BFE)[1] <- ifelse(identical(format, "html"), "Name", "")

  # footer
  if (is.null(format) || format == "text") {
    footer <- list(
      "\n* Against Denominator: ",
      c(denM, "cyan"),
      "\n*   Bayes Factor Type: ",
      c(grid.type, "cyan"),
      if (log) c("\n\nBayes Factors are on the log-scale.", "red")
    )
    # color formatting for caption
    if (!is.null(caption)) {
      caption <- c(caption, "blue")
    }
  } else {
    footer <- insight::compact_list(list(
      paste0("Against Denominator: ", denM),
      paste0("Bayes Factor Type: ", grid.type),
      if (log) "Bayes Factors are on the log-scale."
    ))
  }

  attr(BFE, "table_footer") <- footer
  attr(BFE, "table_caption") <- caption
  BFE
}



#' @export
format.bayesfactor_inclusion <- function(x,
                                         digits = 3,
                                         log = FALSE,
                                         format = "text",
                                         caption = NULL,
                                         exact = TRUE,
                                         ...) {
  priorOdds <- attr(x, "priorOdds")
  matched <- attr(x, "matched")

  # format table
  BFE <- as.data.frame(x)
  BFE$log_BF <- as.numeric(x, log = log)
  BFE$BF <- insight::format_bf(abs(BFE$log_BF), name = NULL, exact = exact, ...)

  if (any((sgn <- sign(BFE$log_BF) < 0)[!is.na(BFE$log_BF)])) {
    BFE$BF[sgn] <- paste0("-", BFE$BF[sgn])
  }

  BFE <- BFE[c("p_prior", "p_posterior", "BF")]
  BFE <- cbind(rownames(BFE), BFE)
  colnames(BFE) <- c("", "P(prior)", "P(posterior)", "Inclusion BF")
  colnames(BFE)[1] <- ifelse(identical(format, "html"), "Parameter", "")

  # footer
  if (is.null(format) || format == "text") {
    footer <- list(
      c("\n* Compared among: "),
      c(if (matched) "matched models only" else "all models", "cyan"),
      c("\n*    Priors odds: "),
      c(if (!is.null(priorOdds)) "custom" else "uniform-equal", "cyan"),
      if (log) c("\n\nBayes Factors are on the log-scale.", "red")
    )
    # color formatting for caption
    if (!is.null(caption)) {
      caption <- c(caption, "blue")
    }
  } else {
    footer <- insight::compact_list(list(
      paste0("Compared among: ", if (matched) "matched models only" else "all models"),
      paste0("Priors odds: ", if (!is.null(priorOdds)) "custom" else "uniform-equal"),
      if (log) "Bayes Factors are on the log-scale."
    ))
  }

  attr(BFE, "table_footer") <- footer
  attr(BFE, "table_caption") <- caption
  BFE
}



#' @export
format.bayesfactor_restricted <- function(x,
                                          digits = 3,
                                          log = FALSE,
                                          format = "text",
                                          caption = NULL,
                                          exact = TRUE,
                                          ...) {
  BFE <- as.data.frame(x)

  # Format
  BFE$log_BF <- as.numeric(x, log = log)
  BFE$BF <- insight::format_bf(abs(BFE$log_BF), name = NULL, exact = exact, ...)

  if (any((sgn <- sign(BFE$log_BF) < 0)[!is.na(BFE$log_BF)])) {
    BFE$BF[sgn] <- paste0("-", BFE$BF[sgn])
  }
  BFE$log_BF <- NULL
  colnames(BFE) <- c("Hypothesis", "P(Prior)", "P(Posterior)", "BF")

  # footer
  if (is.null(format) || format == "text") {
    footer <- list(
      c("\n* Bayes factors for the restricted model vs. the un-restricted model.\n"),
      if (log) c("\nBayes Factors are on the log-scale.\n", "red")
    )
    # color formatting for caption
    if (!is.null(caption)) {
      caption <- c(caption, "blue")
    }
  } else {
    footer <- insight::compact_list(list(
      "Bayes factors for the restricted model vs. the un-restricted model.",
      if (log) "Bayes Factors are on the log-scale."
    ))
  }

  attr(BFE, "table_footer") <- footer
  attr(BFE, "table_caption") <- caption
  BFE
}



#' @export
format.bayesfactor_parameters <- function(x,
                                          cp = NULL,
                                          digits = 3,
                                          log = FALSE,
                                          format = "text",
                                          exact = TRUE,
                                          ...) {
  null <- attr(x, "hypothesis")
  direction <- attr(x, "direction")

  x$log_BF <- as.numeric(x, log = log)

  x$BF_override <- insight::format_bf(abs(x$log_BF), name = NULL, exact = exact, ...)

  if (any((sgn <- sign(x$log_BF) < 0)[!is.na(x$log_BF)])) {
    x$BF_override[sgn] <- paste0("-", x$BF_override[sgn])
  }
  x$log_BF <- NULL

  # format columns and values of data frame
  out <- insight::format_table(x, digits = digits, format = format, ...)
  colnames(out)[colnames(out) == "BF_override"] <- "BF"

  # table caption
  caption <- sprintf(
    "Bayes Factor (%s)",
    if (length(null) == 1) "Savage-Dickey density ratio" else "Null-Interval"
  )

  if (is.null(format) || format == "text") {
    caption <- c(caption, "blue")
  }


  # format null-value
  if (length(null) == 1) {
    null <- insight::format_value(null, digits = digits, protect_integers = TRUE)
  } else {
    null <- insight::format_ci(null[1], null[2], ci = NULL, digits = digits)
  }


  # footer
  if (is.null(format) || format == "text") {
    footer <- list(
      c("\n* Evidence Against The Null: "),
      c(null, "cyan"),
      if (direction) c("\n*                 Direction: "),
      if (direction < 0) c("Left-Sided test", "cyan"),
      if (direction > 0) c("Right-Sided test", "cyan"),
      if (log) c("\n\nBayes Factors are on the log-scale.\n", "red")
    )
  } else {
    footer <- insight::compact_list(list(
      paste0("Evidence Against The Null: ", null),
      if (direction) c("Direction: "),
      if (direction < 0) "Left-Sided test",
      if (direction > 0) "Right-Sided test",
      if (log) "Bayes Factors are on the log-scale."
    ))
  }


  # match and split at components
  if (!is.null(cp) && !all(is.na(match(cp$Parameter, out$Parameter)))) {
    out <- insight::print_parameters(
      cp,
      out,
      keep_parameter_column = FALSE,
      remove_empty_column = TRUE,
      format = format
    )
    attr(out[[1]], "table_caption") <- caption
    attr(out[[length(out)]], "table_footer") <- footer
  } else {
    attr(out, "table_caption") <- caption
    attr(out, "table_footer") <- footer
  }

  out
}
