#' Simpson's paradox dataset simulation
#'
#' Simpson's paradox, or the Yule-Simpson effect, is a phenomenon in probability
#' and statistics, in which a trend appears in several different groups of data
#' but disappears or reverses when these groups are combined.
#'
#' @param n The number of observations for each group to be generated (minimum 4).
#' @param groups Number of groups (groups can be participants, clusters, anything).
#' @param difference Difference between groups.
#' @param group_prefix The prefix of the group name (e.g., "G_1", "G_2", "G_3", ...).
#' @inheritParams simulate_correlation
#'
#' @return A dataset.
#'
#' @examples
#' data <- simulate_simpson(n = 10, groups = 5, r = 0.5)
#'
#' if (require("ggplot2")) {
#'   ggplot(data, aes(x = V1, y = V2)) +
#'     geom_point(aes(color = Group)) +
#'     geom_smooth(aes(color = Group), method = "lm") +
#'     geom_smooth(method = "lm")
#' }
#' @export
simulate_simpson <- function(n = 100,
                             r = 0.5,
                             groups = 3,
                             difference = 1,
                             group_prefix = "G_") {
  if (n <= 3) {
    insight::format_error("The number of observations `n` should be larger than 3.")
  }

  data <- data.frame()
  for (i in 1:groups) {
    dat <- simulate_correlation(n = n, r = r)
    dat$V1 <- dat$V1 + difference * i # (i * -sign(r))
    dat$V2 <- dat$V2 + difference * (i * -sign(r))
    dat$Group <- sprintf(paste0(group_prefix, "%0", nchar(trunc(abs(groups))), "d"), i)
    data <- rbind(data, dat)
  }

  data
}
