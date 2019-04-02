#' @export
plot.equivalence_test <- function(x, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE) && !requireNamespace("ggridges", quietly = TRUE)) {
    warning("Packages 'ggplot2' and 'ggridges' required to plot test for practical equivalence.", call. = FALSE)
    return(x)
  }

  model_name <- attr(x, "model", exact = TRUE)

  if (is.null(model_name)) {
    warning("plot() only works for equivalence_test() with model-objects.", call. = FALSE)
    return(x)
  }


  # retrieve model
  model <- tryCatch(
    {
      get(model_name, envir = parent.frame())
    },
    error = function(e) { NULL }
  )

  if (is.null(model)) {
    warning(sprintf("Can't find object '%s'.", model_name))
    return(x)
  }


  # if we have intercept-only models, keep at least the intercept
  if (x$Parameter[1] %in% c("Intercept", "(Intercept)") && nrow(x) > 1) {
    x <- x[-1, ]
  }

  tmp <- as.data.frame(model, stringsAsFactors = FALSE)[, x$Parameter, drop = FALSE]
  tmp2 <- lapply(1:nrow(x), function(i) {
    p <- x$Parameter[i]
    tmp[[p]][tmp[[p]] < x$HDI_low[i]] <- NA
    tmp[[p]][tmp[[p]] > x$HDI_high[i]] <- NA
    tmp[[p]]
  })

  names(tmp2) <- colnames(tmp)
  tmp <- as.data.frame(tmp2)

  tmp <- .to_long(tmp, names_to = "predictor", values_to = "estimate")
  # tmp$predictor <- as.factor(tmp$predictor)


  x$ROPE_Equivalence <- ifelse(
    x$ROPE_Equivalence == "accepted",
    "rejected",
    ifelse(
      x$ROPE_Equivalence == "rejected",
      "accepted",
      "undecided")
  )

  tmp$grp <- NA
  for (i in 1:nrow(x)) {
    tmp$grp[tmp$predictor == x$Parameter[i]] <- x$ROPE_Equivalence[i]
  }

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  tmp$predictor <- factor(tmp$predictor)
  tmp$predictor <- factor(tmp$predictor, levels = rev(levels(tmp$predictor)))


  # check for user defined arguments

  fill.color <- c("#018F77", "#CD423F", "#FCDA3B")
  rope.color <- "#0171D3"
  rope.alpha <- 0.15
  x.title <- sprintf("%i%% Highest Density Region of Posterior Samples", x$CI[1])
  legend.title <- "Decision on Parameters"
  labels <- levels(tmp$predictor)
  names(labels) <- labels

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("accepted", "rejected", "undecided"))))]

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("rope.color" %in% names(add.args)) rope.color <- eval(add.args[["rope.color"]])
  if ("rope.alpha" %in% names(add.args)) rope.alpha <- eval(add.args[["rope.alpha"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope.alpha
  if (rope.line.alpha > 1) rope.line.alpha <- 1


  ggplot2::ggplot(tmp, ggplot2::aes_string(x = "estimate", y = "predictor", fill = "grp")) +
    ggplot2::annotate("rect", xmin = .rope[1], xmax = .rope[2], ymin = 0, ymax = Inf, fill = rope.color, alpha = rope.alpha) +
    ggplot2::geom_vline(xintercept = 0, colour = rope.color, size = .8, alpha = rope.line.alpha) +
    ggridges::geom_density_ridges2(rel_min_height = 0.01, scale = 2, alpha = .5) +
    ggplot2::scale_fill_manual(values = fill.color) +
    ggplot2::labs(x = x.title, y = NULL, fill = legend.title) +
    ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::theme(legend.position = "bottom")
}
