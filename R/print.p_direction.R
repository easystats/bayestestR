#' @export
print.p_direction <- function(x, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# Probability of Direction (pd)\n\n", "blue")

    if ("data.frame" %in% class(x)) {
      pars <- format(x$Parameter)
      pd <- format(sprintf("%.2f%%", x$pd), justify = "right")
      cat(paste0("  ", pars, ": ", pd, collapse = "\n"))
    } else {
      cat(sprintf("  - pd = %.2f%%", x))
    }
  }
}
