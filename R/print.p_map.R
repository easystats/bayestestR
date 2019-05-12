#' @export
print.p_map <- function(x, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# MAP-based p-value:\n\n", "blue")

    if ("data.frame" %in% class(x)) {
      cat(paste0(paste0("  - ", x$Parameter, sprintf(": p (MAP) = %.2f%%", x$pd)), collapse = "\n"))
    } else {
      cat(sprintf("p (MAP) = %.2f%%", x))
    }
  }
}
