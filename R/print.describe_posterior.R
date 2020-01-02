#' @export
print.describe_posterior <- function(x, digits = 3, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# Description of Posterior Distributions\n\n", "blue")
    if (!is.null(attributes(x)$ci_method) && tolower(attributes(x)$ci_method) == "si") {
      cn <- gsub("CI", "BF", gsub("^CI_", "SI_", colnames(x)))
      colnames(x) <- cn
    }
    print_data_frame(x, digits = digits)
  }
}
