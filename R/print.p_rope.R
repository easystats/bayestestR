#' @export
print.p_rope <- function(x, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# ROPE-based p-value:\n\n", "blue")

    if ("data.frame" %in% class(x)) {
      cat(paste0(paste0("  - ", x$Parameter, sprintf(": p (ROPE) = %.2f%%", x$pd)), collapse = "\n"))
    } else {
      cat(sprintf("p (ROPE) = %.2f%%", x))
    }
  }
}
