.onAttach <- function(libname, pkgname) {
  message <- "bayestestR is using a not-recommended default-level for uncertainty intervals. Note that you probably need an effective sample size of at least 10.000 per parameter to get robust tails of your uncertainty intervals. Unfortunately, the default number of iterations for most Bayes packages return only 4.000 samples of the posterior distribution. Be prepared to set a sensible default number of interation during model ftting, or set the 'ci'-argument in bayestestR function explicitly to a value of max. 0.9."
  if (format(Sys.time(), "%m%d") == "0504") {
    message <- paste0(message, "\n\nMay the fourth be with you!")
    packageStartupMessage("May the fourth be with you!")
  }
  packageStartupMessage(message)
}
