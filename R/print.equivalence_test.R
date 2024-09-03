#' @export
print.equivalence_test <- function(x, digits = 2, ...) {
  orig_x <- x
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, x$ROPE_low[1], digits, x$ROPE_high[1]))

  # fix "sd" pattern
  model <- .retrieve_model(x)
  if (!is.null(model)) {
    cp <- insight::clean_parameters(model)
    if (!is.null(cp$Group) && any(startsWith(cp$Group, "SD/Cor"))) {
      cp <- cp[startsWith(cp$Group, "SD/Cor"), ]
      matches <- match(cp$Parameter, x$Parameter)
      if (length(matches)) {
        new_pattern <- paste0(
          "SD/Cor: ",
          cp$Cleaned_Parameter[unique(stats::na.omit(match(x$Parameter, cp$Parameter)))]
        )
        if (length(new_pattern) == length(matches)) {
          x$Parameter[matches] <- new_pattern
        }
      }
    }
  }

  # find the longest HDI-value, so we can align the brackets in the ouput
  x$HDI_low <- sprintf("%.*f", digits, x$HDI_low)
  x$HDI_high <- sprintf("%.*f", digits, x$HDI_high)

  maxlen_low <- max(nchar(x$HDI_low))
  maxlen_high <- max(nchar(x$HDI_high))

  x$ROPE_Percentage <- sprintf("%.*f %%", digits, x$ROPE_Percentage * 100)
  x$HDI <- sprintf("[%*s %*s]", maxlen_low, x$HDI_low, maxlen_high, x$HDI_high)

  ci <- unique(x$CI)
  keep.columns <- c(
    attr(x, "grid_cols"), "Parameter", "Effects", "Component",
    "ROPE_Equivalence", "ROPE_Percentage", "CI", "HDI"
  )

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  .print_equivalence_component(x, ci, digits)

  invisible(orig_x)
}


.print_equivalence_component <- function(x, ci, digits) {
  for (i in ci) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
    colnames(xsub)[colnames(xsub) == "HDI"] <- sprintf("%i%% HDI", 100 * i)
    .print_data_frame(xsub, digits = digits)
    cat("\n")
  }
}


.retrieve_model <- function(x) {
  # retrieve model
  obj_name <- attr(x, "object_name", exact = TRUE)
  model <- NULL

  if (!is.null(obj_name)) {
    # first try, parent frame
    model <- tryCatch(get(obj_name, envir = parent.frame()), error = function(e) NULL)

    if (is.null(model)) {
      # second try, global env
      model <- tryCatch(get(obj_name, envir = globalenv()), error = function(e) NULL)
    }

    if (is.null(model)) {
      # last try
      model <- .dynGet(obj_name, ifnotfound = NULL)
    }
  }
  model
}


.dynGet <- function(x,
                    ifnotfound = stop(gettextf("%s not found", sQuote(x)), domain = NA),
                    minframe = 1L,
                    inherits = FALSE) {
  x <- insight::safe_deparse(x)
  n <- sys.nframe()
  myObj <- structure(list(.b = as.raw(7)), foo = 47L)
  while (n > minframe) {
    n <- n - 1L
    env <- sys.frame(n)
    r <- get0(x, envir = env, inherits = inherits, ifnotfound = myObj)
    if (!identical(r, myObj)) {
      return(r)
    }
  }
  ifnotfound
}
