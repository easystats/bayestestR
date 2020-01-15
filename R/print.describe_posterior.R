#' @importFrom insight format_table
#' @export
print.describe_posterior <- function(x, digits = 3, ...) {
  if ("data_plot" %in% class(x)) {
    cat(insight::format_table(as.data.frame(x), digits = digits))
  } else {
    insight::print_color("# Description of Posterior Distributions\n\n", "blue")
    if (!is.null(attributes(x)$ci_method) && tolower(attributes(x)$ci_method) == "si") {
      cn <- gsub("^CI", "SI", colnames(x))
      colnames(x) <- cn
    }
    print_data_frame(x, digits = digits)
  }
}
