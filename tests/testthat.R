library(testthat)
library(bayestestR)

if (length(strsplit(packageDescription("bayestestR")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllbayestestRTests" = "yes")
} else {
  Sys.setenv("RunAllbayestestRTests" = "no")
}

test_check("bayestestR")
