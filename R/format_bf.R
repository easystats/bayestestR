#' Bayes Factor Formatting
#'
#' @param bf Bayes Factor.
#' @param stars Add significance stars (e.g., p < .001***).
#' @param stars_only Return only significance stars.
#' @param name Name prefixing the text. Can be \code{NULL}.
#' @inherit insight::format_value
#'
#' @return A formatted string.
#'
#' @examples
#' format_bf(1.20)
#' format_bf(c(1.20, 1557, 3.5, 12), stars = TRUE)
#' format_bf(c(1.20, 1557, 3.5, 12), name = NULL)
#' @importFrom insight format_value
#' @export
format_bf <- function(bf, stars = FALSE, stars_only = FALSE, name = "BF") {
  text <- ifelse(bf > 999, "> 999***",
    paste0(
      "= ",
      ifelse(bf > 30, paste0(insight::format_value(bf), "***"),
        ifelse(bf > 10, paste0(insight::format_value(bf), "**"),
          ifelse(bf > 3, paste0(insight::format_value(bf), "*"),
            paste0(insight::format_value(bf))
          )
        )
      )
    )
  )

  .add_prefix_and_remove_stars(text, stars, stars_only, name)
}





#' @keywords internal
.add_prefix_and_remove_stars <- function(text, stars, stars_only, name, missing = "") {
  missing_index <- is.na(text)

  if (is.null(name)) {
    text <- gsub("= ", "", text)
  } else {
    text <- paste(name, text)
  }

  if (stars_only == TRUE) {
    text <- gsub("[^\\*]", "", text)
  } else if (stars == FALSE) {
    text <- gsub("\\*", "", text)
  }

  text[missing_index] <- missing
  text
}