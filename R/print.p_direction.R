#' @export
print.p_direction <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# Probability of Direction (pd)\n\n", "blue")

    if ("data.frame" %in% class(x)) {
      x$Parameter <- as.character(x$Parameter)
      x$pd <- sprintf("%.*f%%", digits, x$pd * 100)
      print.data.frame(x, row.names = FALSE)
    } else {
      cat(sprintf("pd = %.*f%%", digits, x * 100))
    }
  }
}
