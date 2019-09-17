#' @export
print.describe_posterior <- function(x, digits = 3, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# Description of Posterior Distributions\n\n", "blue")
    print_data_frame(x, digits = digits)
  }
}
