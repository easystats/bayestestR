.onAttach <- function(libname, pkgname) {
  if (format(Sys.time(), "%m%d") == "0504") {
    message <- paste0(message, "\n\nMay the fourth be with you!")
  }
  packageStartupMessage(message)
}
