#' @export
print.p_map <- function(x, digits = 3, ...) {
  if ("data_plot" %in% class(x)) {
    print(as.data.frame(x))
  } else {
    insight::print_color("# MAP-based p-value\n\n", "blue")

    if ("data.frame" %in% class(x)) {
      pars <- format(x$Parameter)
      pmap <- format(sprintf("%.*f", digits, x$p_MAP), justify = "right")
      cat(paste0("  ", pars, ": ", pmap, collapse = "\n"))
    } else {
      cat(sprintf("p (MAP) = %.*f", digits, x))
    }
  }
}
