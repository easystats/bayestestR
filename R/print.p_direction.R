#' @export
print.p_direction <- function(x, ...) {
  if("data_plot" %in% class(x)){
    print(as.data.frame(x))
  } else{
    insight::print_color("# Probability of Direction (pd):\n\n", "blue")

    if("data.frame" %in% class(x)){
      cat(paste0(paste0("  - ", x$Parameter, sprintf(": pd = %.2f%%", x$pd)), collapse = "\n"))
    } else{
      cat(sprintf("pd = %.2f%%", x))
    }
  }
}
