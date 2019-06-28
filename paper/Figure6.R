library(bayestestR)
library(ggplot2)
library(strengejacke)

set.seed(123)
posterior <- distribution_chisquared(100, 3)
dat <- as.data.frame(density(posterior))

m2 <- map_estimate(posterior)
ypos2 <- dat$y[which.min(abs(dat$x - m2))]

ypos_null <- dat$y[which.min(abs(dat$x))]

ggplot(dat, aes(x=x, y=y)) +
  geom_ribbon(aes(ymin=0, ymax=y), fill="#FFC107") +
  geom_segment(x=m2, xend=m2, y=0, yend=ypos2, color="#E91E63", size=1) +
  geom_point(x=m2, y=ypos2, color="#E91E63", size=5) +
  geom_segment(x=0, xend=0, y=0, yend=ypos_null, color="#E91E63", size=1) +
  geom_point(x=0, y=ypos_null, color="#E91E63", size=5) +
  geom_vline(xintercept=0, linetype="dotted") +
  theme_classic(base_size = 20) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .25)) +
  xlab("\nParameter Value") +
  ylab("Probability Density\n")

ggsave("paper/Figure6.png", width = 13, height = 8, units = "in", dpi = 300)
