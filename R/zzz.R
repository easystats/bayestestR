.onAttach <- function(libname, pkgname) {
  message <- "Note: The default CI width just changed :O To go back to the previous behaviour, set it explicitly as 'ci = 0.89' (see https://github.com/easystats/bayestestR/discussions/250)"
  if (format(Sys.time(), "%m%d") == "0504") {
    message <- paste0(message, "\n\nMay the fourth be with you!")
  }
  packageStartupMessage(message)
}
