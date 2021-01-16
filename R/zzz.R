.onAttach <- function(libname, pkgname) {
  message <- "Note: The default CI width (currently `ci=0.89`) might change in future versions (see https://github.com/easystats/bayestestR/discussions/250). To prevent any issues, please set it explicitly when using bayestestR functions, via the 'ci' argument."
  if (format(Sys.time(), "%m%d") == "0504") {
    message <- paste0(message, "\n\nMay the fourth be with you!")
  }
  packageStartupMessage(message)
}
