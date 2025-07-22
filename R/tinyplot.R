#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_density <- function(x, centrality = "median", ci = 0.95, show_intercept = FALSE, ...) {
  insight::check_if_installed("tinyplot")

  model <- .retrieve_model(x)
  plot_data <- datawizard::data_to_long(as.data.frame(model))
  colnames(plot_data) <- c("Parameter", "x")

  # add component and effects columns
  plot_data <- merge(plot_data, insight::clean_parameters(model), by = "Parameter")

  plot_data <- .remove_intercept(plot_data, show_intercept = show_intercept)
  plot_data <- plot_data[plot_data$Component != "sigma", ]

  plot_data$Parameter <- factor(
    plot_data$Parameter,
    levels = rev(levels(factor(plot_data$Parameter)))
  )

  # summary
  split_columns <- intersect(
    c("Parameter", "Effects", "Component"),
    colnames(plot_data)
  )

  datasplit <- split(plot_data, plot_data[split_columns])

  my_summary <- do.call(
    rbind,
    insight::compact_list(lapply(datasplit, function(i) {
      if (length(i$x) > 0L) {
        Estimate <- as.numeric(point_estimate(i$x, centrality = centrality))
        CI <- as.numeric(ci(i$x, ci = ci))
        out <- data.frame(
          Parameter = unique(i$Parameter),
          x = Estimate,
          CI_low = CI[2],
          CI_high = CI[3],
          stringsAsFactors = FALSE
        )
        if ("Effects" %in% colnames(i)) {
          out$Effects <- unique(i$Effects)
        }
        if ("Component" %in% colnames(i)) {
          out$Component <- unique(i$Component)
        }
      } else {
        out <- NULL
      }
      out
    }))
  )

  # my_summary$Parameter <- factor(
  #   my_summary$Parameter,
  #   levels = levels(my_summary$Parameter)
  # )

  tinyplot::tinytheme("ridge2")
  tinyplot::tinyplot(
    Parameter ~ x,
    data = plot_data,
    type = tinyplot::type_ridge(col = "white"),
    legend = FALSE
  )
  tinyplot::tinyplot_add(
    Parameter ~ x,
    data = my_summary,
    ymin = CI_low,
    ymax = CI_high,
    type = "errorbar",
    flip = FALSE
  )
}


.intercept_names <- c(
  "(intercept)_zi",
  "intercept (zero-inflated)",
  "intercept",
  "zi_intercept",
  "(intercept)",
  "b_intercept",
  "b_zi_intercept"
)


.is_intercept <- function(x) {
  x <- tolower(x)
  x %in% .intercept_names | grepl("(?i)intercept[^a-zA-Z]", x)
}

.remove_intercept <- function(x, column = "Parameter", show_intercept = FALSE) {
  if (!show_intercept) {
    to_remove <- which(.is_intercept(x[[column]]))
    if (length(to_remove)) x <- x[-to_remove, ]
  }
  x
}
