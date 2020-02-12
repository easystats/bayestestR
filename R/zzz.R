.onAttach <- function(libname, pkgname) {
  if (format(Sys.time(), "%m%d") == "0504") {
    packageStartupMessage("May the fourth be with you!")
  }
}
