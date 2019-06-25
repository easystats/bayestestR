#' @export
print.p_rope <- function(x, digits = 2, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# ROPE-based p-value\n\n", "blue")

    if ("data.frame" %in% class(x)) {
      pars <- format(x$Parameter)
      pmap <- format(sprintf("%.*f%%", digits, x$p_ROPE * 100), justify = "right")
      cat(paste0("  ", pars, ": ", pmap, collapse = "\n"))
    } else {
      cat(sprintf("p (ROPE) = %.*f%%", digits, x * 100))
    }
  }
}
